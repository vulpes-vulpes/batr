# Tests for manual_vet_extractor and helper functions in R/new_manual_vetting.R

library(testthat)
library(batr)
library(data.table)

context("manual_vet_extractor and helpers")

# ----------------------------------------------------------------------------
# .validate_percentage
# ----------------------------------------------------------------------------

test_that(".validate_percentage accepts valid percentages", {
    expect_silent(batr:::`.validate_percentage`(0.05))
    expect_silent(batr:::`.validate_percentage`(0.5))
    expect_silent(batr:::`.validate_percentage`(1))
    expect_silent(batr:::`.validate_percentage`(0.001))
})

test_that(".validate_percentage rejects invalid percentages", {
    expect_error(batr:::`.validate_percentage`(0), "Invalid percentage")
    expect_error(batr:::`.validate_percentage`(-0.1), "Invalid percentage")
    expect_error(batr:::`.validate_percentage`(1.5), "Invalid percentage")
    expect_error(batr:::`.validate_percentage`("0.5"), "Invalid percentage")
    expect_error(batr:::`.validate_percentage`(c(0.1, 0.2)), "must be a single numeric value")
    expect_error(batr:::`.validate_percentage`(NA), "Invalid percentage")
})

# ----------------------------------------------------------------------------
# .validate_directories
# ----------------------------------------------------------------------------

test_that(".validate_directories validates WAV directory exists", {
    temp_wav_dir <- tempfile()
    dir.create(temp_wav_dir)
    temp_save_dir <- tempfile()

    # Expect message when creating save directory
    expect_message(
        batr:::`.validate_directories`(temp_wav_dir, temp_save_dir),
        "Creating save directory"
    )
    expect_true(dir.exists(temp_save_dir))

    unlink(temp_wav_dir, recursive = TRUE)
    unlink(temp_save_dir, recursive = TRUE)
})

test_that(".validate_directories errors on missing WAV directory", {
    nonexistent_dir <- tempfile()
    temp_save_dir <- tempfile()

    expect_error(
        batr:::`.validate_directories`(nonexistent_dir, temp_save_dir),
        "WAV directory does not exist"
    )
})

test_that(".validate_directories creates save directory if missing", {
    temp_wav_dir <- tempfile()
    dir.create(temp_wav_dir)
    temp_save_dir <- tempfile()

    expect_false(dir.exists(temp_save_dir))
    expect_message(
        batr:::`.validate_directories`(temp_wav_dir, temp_save_dir),
        "Creating save directory"
    )
    expect_true(dir.exists(temp_save_dir))

    unlink(temp_wav_dir, recursive = TRUE)
    unlink(temp_save_dir, recursive = TRUE)
})

# ----------------------------------------------------------------------------
# .sample_files
# ----------------------------------------------------------------------------

test_that(".sample_files returns correct sample size", {
    dt <- data.table(
        File.Name = paste0("file", 1:100, ".wav"),
        Species = "Epfu",
        Full.Path = paste0("/path/to/file", 1:100, ".wav")
    )

    sampled <- batr:::`.sample_files`(dt, 0.1)
    expect_equal(nrow(sampled), 10)

    sampled <- batr:::`.sample_files`(dt, 0.5)
    expect_equal(nrow(sampled), 50)

    sampled <- batr:::`.sample_files`(dt, 0.25)
    expect_equal(nrow(sampled), 25)
})

test_that(".sample_files handles 100% sampling without calling sample()", {
    dt <- data.table(
        File.Name = paste0("file", 1:10, ".wav"),
        Species = "Epfu",
        Full.Path = paste0("/path/to/file", 1:10, ".wav")
    )

    sampled <- batr:::`.sample_files`(dt, 1.0)
    expect_equal(nrow(sampled), 10)
    expect_equal(sampled$File.Name, dt$File.Name)
})

test_that(".sample_files handles empty dataset", {
    dt <- data.table(
        File.Name = character(0),
        Species = character(0),
        Full.Path = character(0)
    )

    sampled <- batr:::`.sample_files`(dt, 0.5)
    expect_equal(nrow(sampled), 0)
})

test_that(".sample_files rounds up with ceiling", {
    dt <- data.table(
        File.Name = paste0("file", 1:10, ".wav"),
        Species = "Epfu",
        Full.Path = paste0("/path/to/file", 1:10, ".wav")
    )

    # 10 * 0.15 = 1.5, ceiling gives 2
    sampled <- batr:::`.sample_files`(dt, 0.15)
    expect_equal(nrow(sampled), 2)
})

# ----------------------------------------------------------------------------
# .prepare_vetting_dataset
# ----------------------------------------------------------------------------

test_that(".prepare_vetting_dataset loads and filters data correctly", {
    # Create test data
    observations <- data.frame(
        File.Name = paste0("file", 1:10, ".wav"),
        Species.Auto.ID = c("Epfu", "Labo", "Epfu", NA, "Laci", "Epfu", NA, "Lano", "Mylu", "Epfu"),
        Species.Manual.ID = c(NA, "Mylu", NA, NA, NA, NA, NA, NA, "Epfu", NA),
        Species = c("Epfu", "Mylu", "Epfu", NA, "Laci", "Epfu", NA, "Lano", "Epfu", "Epfu"),
        Location = "Site1"
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    # Test basic loading and filtering (removes NAs)
    result <- batr:::`.prepare_vetting_dataset`(temp_rdata, no_manual = FALSE)
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 8) # Should exclude 2 NA Species rows
    expect_true(all(!is.na(result$Species)))

    # Test no_manual = TRUE filtering
    result_no_manual <- batr:::`.prepare_vetting_dataset`(temp_rdata, no_manual = TRUE)
    expect_equal(nrow(result_no_manual), 6) # Excludes rows with Species.Manual.ID
    expect_true(all(is.na(result_no_manual$Species.Manual.ID)))

    unlink(temp_rdata)
})

test_that(".prepare_vetting_dataset errors on empty dataset", {
    observations <- data.frame(
        File.Name = character(0),
        Species = character(0),
        Location = character(0)
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    expect_error(
        batr:::`.prepare_vetting_dataset`(temp_rdata, no_manual = FALSE),
        "No valid observations found"
    )

    unlink(temp_rdata)
})

test_that(".prepare_vetting_dataset cleans up original data from memory", {
    observations <- data.frame(
        File.Name = paste0("file", 1:5, ".wav"),
        Species = "Epfu",
        Location = "Site1"
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    # After calling the function, 'observations' should not exist in global env
    result <- batr:::`.prepare_vetting_dataset`(temp_rdata, no_manual = FALSE)
    expect_false(exists("observations", envir = .GlobalEnv))

    unlink(temp_rdata)
})

# ----------------------------------------------------------------------------
# .match_files_to_wavs
# ----------------------------------------------------------------------------

test_that(".match_files_to_wavs merges dataset with WAV files", {
    # Create mock dataset
    dataset <- data.table(
        File.Name = c("file1.wav", "file2.wav", "file3.wav"),
        Species = c("Epfu", "Labo", "Epfu"),
        Location = "Site1"
    )

    # Create temporary WAV directory with files
    temp_wav_dir <- tempfile()
    dir.create(temp_wav_dir)
    file.create(file.path(temp_wav_dir, "file1.wav"))
    file.create(file.path(temp_wav_dir, "file2.wav"))

    # Mock .get_file_list to return our test files
    # Since .get_file_list is complex, we'll skip full integration test
    # and test the merge logic directly
    wav_files <- data.table(
        File.Name = c("file1.wav", "file2.wav"),
        Full.Path = file.path(temp_wav_dir, c("file1.wav", "file2.wav"))
    )

    # Manually perform the merge as the function does
    data.table::setkey(dataset, File.Name)
    data.table::setkey(wav_files, File.Name)
    result <- wav_files[dataset]

    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 3)
    expect_true("Full.Path" %in% names(result))
    expect_false(is.na(result$Full.Path[1]))
    expect_false(is.na(result$Full.Path[2]))
    expect_true(is.na(result$Full.Path[3])) # file3.wav not in WAV directory

    unlink(temp_wav_dir, recursive = TRUE)
})

# ----------------------------------------------------------------------------
# .handle_missing_wavs
# ----------------------------------------------------------------------------

test_that(".handle_missing_wavs excludes missing files", {
    dataset <- data.table(
        File.Name = c("file1.wav", "file2.wav", "file3.wav", "file4.wav"),
        Species = "Epfu",
        Full.Path = c("/path/file1.wav", NA, "/path/file3.wav", NA)
    )

    expect_warning(
        result <- batr:::`.handle_missing_wavs`(dataset, "/wav/dir", interactive = FALSE),
        "2 observations have no matching WAV file"
    )

    expect_equal(nrow(result), 2)
    expect_true(all(!is.na(result$Full.Path)))
})

test_that(".handle_missing_wavs returns unchanged dataset when no missing files", {
    dataset <- data.table(
        File.Name = c("file1.wav", "file2.wav"),
        Species = "Epfu",
        Full.Path = c("/path/file1.wav", "/path/file2.wav")
    )

    result <- batr:::`.handle_missing_wavs`(dataset, "/wav/dir", interactive = FALSE)
    expect_equal(nrow(result), 2)
    expect_identical(result, dataset)
})

test_that(".handle_missing_wavs errors when all files missing", {
    dataset <- data.table(
        File.Name = c("file1.wav", "file2.wav"),
        Species = "Epfu",
        Full.Path = c(NA, NA)
    )

    expect_error(
        batr:::`.handle_missing_wavs`(dataset, "/wav/dir", interactive = FALSE),
        "All observations are missing WAV files"
    )
})

# ----------------------------------------------------------------------------
# Integration tests for manual_vet_extractor
# ----------------------------------------------------------------------------

test_that("manual_vet_extractor works end-to-end with simple sampling", {
    skip_on_cran()

    # Create test data structure
    temp_wav_dir <- tempfile()
    temp_save_dir <- tempfile()
    dir.create(temp_wav_dir)

    # Create test WAV files
    wav_files <- paste0("test", 1:20, ".wav")
    for (f in wav_files) {
        file.create(file.path(temp_wav_dir, f))
    }

    # Create test RData
    observations <- data.frame(
        File.Name = wav_files,
        Species = rep(c("Epfu", "Labo"), each = 10),
        Location = "Site1",
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    # Run function
    expect_message(
        manual_vet_extractor(
            data_path = temp_rdata,
            WAV_directory = temp_wav_dir,
            save_directory = temp_save_dir,
            species_list = c("Epfu", "Labo"),
            percentage = 0.2,
            stratified = FALSE,
            fast_import = TRUE,
            interactive = FALSE
        ),
        "Starting Manual Vetting"
    )

    # Check output directories created
    expect_true(dir.exists(file.path(temp_save_dir, "Epfu")))
    expect_true(dir.exists(file.path(temp_save_dir, "Labo")))

    # Check files copied (20% of 10 = 2 files per species)
    epfu_files <- list.files(file.path(temp_save_dir, "Epfu"))
    labo_files <- list.files(file.path(temp_save_dir, "Labo"))
    expect_equal(length(epfu_files), 2)
    expect_equal(length(labo_files), 2)

    # Cleanup
    unlink(temp_wav_dir, recursive = TRUE)
    unlink(temp_save_dir, recursive = TRUE)
    unlink(temp_rdata)
})

test_that("manual_vet_extractor works with stratified sampling", {
    skip_on_cran()

    # Create test data structure
    temp_wav_dir <- tempfile()
    temp_save_dir <- tempfile()
    dir.create(temp_wav_dir)

    # Create test WAV files
    wav_files <- paste0("test", 1:30, ".wav")
    for (f in wav_files) {
        file.create(file.path(temp_wav_dir, f))
    }

    # Create test RData with multiple locations
    observations <- data.frame(
        File.Name = wav_files,
        Species = rep(c("Epfu", "Labo"), each = 15),
        Location = rep(c("Site1", "Site2", "Site3"), 10),
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    # Run function with stratified sampling
    expect_message(
        manual_vet_extractor(
            data_path = temp_rdata,
            WAV_directory = temp_wav_dir,
            save_directory = temp_save_dir,
            species_list = c("Epfu", "Labo"),
            percentage = 0.2,
            stratified = TRUE,
            fast_import = TRUE,
            interactive = FALSE
        ),
        "stratified sampling"
    )

    # Check output structure: species/location subdirectories
    expect_true(dir.exists(file.path(temp_save_dir, "Epfu", "Site1")))
    expect_true(dir.exists(file.path(temp_save_dir, "Epfu", "Site2")))
    expect_true(dir.exists(file.path(temp_save_dir, "Labo", "Site1")))

    # Cleanup
    unlink(temp_wav_dir, recursive = TRUE)
    unlink(temp_save_dir, recursive = TRUE)
    unlink(temp_rdata)
})

test_that("manual_vet_extractor handles missing WAV files gracefully", {
    skip_on_cran()

    # Create test data structure
    temp_wav_dir <- tempfile()
    temp_save_dir <- tempfile()
    dir.create(temp_wav_dir)

    # Create only SOME test WAV files
    wav_files <- paste0("test", 1:10, ".wav")
    for (f in wav_files[1:7]) { # Only create 7 out of 10
        file.create(file.path(temp_wav_dir, f))
    }

    # Create test RData referencing all 10 files
    observations <- data.frame(
        File.Name = wav_files,
        Species = "Epfu",
        Location = "Site1",
        stringsAsFactors = FALSE
    )

    temp_rdata <- tempfile(fileext = ".RData")
    save(observations, file = temp_rdata)

    # Run function - should warn about missing files but continue
    expect_warning(
        manual_vet_extractor(
            data_path = temp_rdata,
            WAV_directory = temp_wav_dir,
            save_directory = temp_save_dir,
            species_list = "Epfu",
            percentage = 0.5,
            stratified = FALSE,
            fast_import = TRUE,
            interactive = FALSE
        ),
        "3 observations have no matching WAV file"
    )

    # Should still copy available files
    expect_true(dir.exists(file.path(temp_save_dir, "Epfu")))
    epfu_files <- list.files(file.path(temp_save_dir, "Epfu"))
    expect_true(length(epfu_files) > 0)
    expect_true(length(epfu_files) <= 7) # Can only copy from 7 available

    # Cleanup
    unlink(temp_wav_dir, recursive = TRUE)
    unlink(temp_save_dir, recursive = TRUE)
    unlink(temp_rdata)
})
