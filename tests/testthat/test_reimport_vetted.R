# Tests for manual_vet_reimporter and helper functions in R/reimport_vetted.R

library(testthat)
library(batr)

context("manual_vet_reimporter and helpers")

# ----------------------------------------------------------------------------
# .validate_reimport_directories
# ----------------------------------------------------------------------------

test_that(".validate_reimport_directories validates vetted directory exists", {
    temp_vetted <- tempfile()
    temp_wav <- tempfile()
    dir.create(temp_vetted)
    dir.create(temp_wav)

    expect_silent(batr:::`.validate_reimport_directories`(temp_vetted, temp_wav))

    unlink(temp_vetted, recursive = TRUE)
    unlink(temp_wav, recursive = TRUE)
})

test_that(".validate_reimport_directories errors on missing vetted directory", {
    nonexistent_dir <- tempfile()
    temp_wav <- tempfile()
    dir.create(temp_wav)

    expect_error(
        batr:::`.validate_reimport_directories`(nonexistent_dir, temp_wav),
        "Vetted directory does not exist"
    )

    unlink(temp_wav, recursive = TRUE)
})

test_that(".validate_reimport_directories errors on missing WAV directory", {
    temp_vetted <- tempfile()
    nonexistent_dir <- tempfile()
    dir.create(temp_vetted)

    expect_error(
        batr:::`.validate_reimport_directories`(temp_vetted, nonexistent_dir),
        "WAV directory does not exist"
    )

    unlink(temp_vetted, recursive = TRUE)
})

# ----------------------------------------------------------------------------
# .discover_files
# ----------------------------------------------------------------------------

test_that(".discover_files finds WAV files", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    # Create dummy WAV files
    file.create(file.path(temp_dir, "file1.wav"))
    file.create(file.path(temp_dir, "file2.wav"))

    expect_message(
        result <- batr:::`.discover_files`(temp_dir, "test", fast_import = FALSE),
        "Found 2 files in test directory"
    )

    expect_equal(nrow(result), 2)
    expect_true(all(c("File.Name", "Full.Path") %in% names(result)))

    unlink(temp_dir, recursive = TRUE)
})

test_that(".discover_files errors when no WAV files found", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    expect_error(
        batr:::`.discover_files`(temp_dir, "test", fast_import = FALSE),
        "No WAV files found"
    )

    unlink(temp_dir, recursive = TRUE)
})

# ----------------------------------------------------------------------------
# .match_vetted_to_original
# ----------------------------------------------------------------------------

test_that(".match_vetted_to_original matches files correctly", {
    # Create mock file lists
    vetted_files <- data.frame(
        File.Name = c("file1.wav", "file2.wav"),
        Full.Path = c("/vetted/file1.wav", "/vetted/file2.wav"),
        stringsAsFactors = FALSE
    )

    original_files <- data.frame(
        File.Name = c("file1.wav", "file2.wav", "file3.wav"),
        Full.Path = c("/original/file1.wav", "/original/file2.wav", "/original/file3.wav"),
        stringsAsFactors = FALSE
    )

    # Create actual files for file.size and file.mtime
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    original_dir <- file.path(temp_dir, "original")
    dir.create(vetted_dir)
    dir.create(original_dir)

    writeLines("test", file.path(vetted_dir, "file1.wav"))
    writeLines("test", file.path(vetted_dir, "file2.wav"))
    writeLines("test", file.path(original_dir, "file1.wav"))
    writeLines("test", file.path(original_dir, "file2.wav"))
    writeLines("test", file.path(original_dir, "file3.wav"))

    vetted_files$Full.Path <- c(
        file.path(vetted_dir, "file1.wav"),
        file.path(vetted_dir, "file2.wav")
    )
    original_files$Full.Path <- c(
        file.path(original_dir, "file1.wav"),
        file.path(original_dir, "file2.wav"),
        file.path(original_dir, "file3.wav")
    )

    result <- batr:::`.match_vetted_to_original`(vetted_files, original_files)

    expect_equal(result$n_total, 2)
    expect_equal(result$n_ok, 2)
    expect_equal(result$n_missing, 0)
    expect_equal(result$n_conflict, 0)
    expect_true(all(result$comparison$Status == "OK"))

    unlink(temp_dir, recursive = TRUE)
})

test_that(".match_vetted_to_original detects missing files", {
    vetted_files <- data.frame(
        File.Name = c("file1.wav", "file2.wav"),
        Full.Path = c("/vetted/file1.wav", "/vetted/file2.wav"),
        stringsAsFactors = FALSE
    )

    original_files <- data.frame(
        File.Name = c("file1.wav"),
        Full.Path = c("/original/file1.wav"),
        stringsAsFactors = FALSE
    )

    # Create actual files
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    original_dir <- file.path(temp_dir, "original")
    dir.create(vetted_dir)
    dir.create(original_dir)

    writeLines("test", file.path(vetted_dir, "file1.wav"))
    writeLines("test", file.path(vetted_dir, "file2.wav"))
    writeLines("test", file.path(original_dir, "file1.wav"))

    vetted_files$Full.Path <- c(
        file.path(vetted_dir, "file1.wav"),
        file.path(vetted_dir, "file2.wav")
    )
    original_files$Full.Path <- c(
        file.path(original_dir, "file1.wav")
    )

    result <- batr:::`.match_vetted_to_original`(vetted_files, original_files)

    expect_equal(result$n_missing, 1)
    expect_true("MISSING" %in% result$comparison$Status)

    unlink(temp_dir, recursive = TRUE)
})

test_that(".match_vetted_to_original detects conflicts (duplicate originals)", {
    vetted_files <- data.frame(
        File.Name = c("file1.wav"),
        Full.Path = c("/vetted/file1.wav"),
        stringsAsFactors = FALSE
    )

    original_files <- data.frame(
        File.Name = c("file1.wav", "file1.wav"),
        Full.Path = c("/original/path1/file1.wav", "/original/path2/file1.wav"),
        stringsAsFactors = FALSE
    )

    # Create actual files
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    original_dir1 <- file.path(temp_dir, "original", "path1")
    original_dir2 <- file.path(temp_dir, "original", "path2")
    dir.create(vetted_dir)
    dir.create(original_dir1, recursive = TRUE)
    dir.create(original_dir2, recursive = TRUE)

    writeLines("test", file.path(vetted_dir, "file1.wav"))
    writeLines("test", file.path(original_dir1, "file1.wav"))
    writeLines("test", file.path(original_dir2, "file1.wav"))

    vetted_files$Full.Path <- file.path(vetted_dir, "file1.wav")
    original_files$Full.Path <- c(
        file.path(original_dir1, "file1.wav"),
        file.path(original_dir2, "file1.wav")
    )

    result <- batr:::`.match_vetted_to_original`(vetted_files, original_files)

    expect_equal(result$n_conflict, 1)
    expect_true("CONFLICT" %in% result$comparison$Status)

    unlink(temp_dir, recursive = TRUE)
})

test_that(".match_vetted_to_original errors on duplicate vetted files", {
    vetted_files <- data.frame(
        File.Name = c("file1.wav", "file1.wav"),
        Full.Path = c("/vetted/path1/file1.wav", "/vetted/path2/file1.wav"),
        stringsAsFactors = FALSE
    )

    original_files <- data.frame(
        File.Name = c("file1.wav"),
        Full.Path = c("/original/file1.wav"),
        stringsAsFactors = FALSE
    )

    expect_error(
        batr:::`.match_vetted_to_original`(vetted_files, original_files),
        "Duplicate filenames found in vetted directory"
    )
})

test_that(".match_vetted_to_original detects size mismatches", {
    # Create files with different sizes
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    original_dir <- file.path(temp_dir, "original")
    dir.create(vetted_dir)
    dir.create(original_dir)

    # Create files with different sizes (>10% difference)
    writeLines(rep("test", 100), file.path(vetted_dir, "file1.wav"))
    writeLines(rep("test", 50), file.path(original_dir, "file1.wav"))

    vetted_files <- data.frame(
        File.Name = "file1.wav",
        Full.Path = file.path(vetted_dir, "file1.wav"),
        stringsAsFactors = FALSE
    )

    original_files <- data.frame(
        File.Name = "file1.wav",
        Full.Path = file.path(original_dir, "file1.wav"),
        stringsAsFactors = FALSE
    )

    result <- batr:::`.match_vetted_to_original`(vetted_files, original_files)

    expect_equal(result$n_size_diff, 1)
    expect_true("SIZE DIFF" %in% result$comparison$Status)

    unlink(temp_dir, recursive = TRUE)
})

# ----------------------------------------------------------------------------
# .copy_vetted_files
# ----------------------------------------------------------------------------

test_that(".copy_vetted_files copies files successfully", {
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    original_dir <- file.path(temp_dir, "original")
    dir.create(vetted_dir)
    dir.create(original_dir)

    # Create test files
    writeLines("vetted content", file.path(vetted_dir, "file1.wav"))
    writeLines("original content", file.path(original_dir, "file1.wav"))

    comparison <- data.frame(
        File.Name = "file1.wav",
        Vetted.Path = file.path(vetted_dir, "file1.wav"),
        Original.Path = file.path(original_dir, "file1.wav"),
        Status = "OK",
        stringsAsFactors = FALSE
    )

    result <- batr:::`.copy_vetted_files`(comparison)

    expect_true(result$success)
    expect_equal(result$n_copied, 1)
    expect_equal(result$n_failed, 0)
    expect_equal(result$n_skipped, 0)

    # Verify file was overwritten
    content <- readLines(file.path(original_dir, "file1.wav"))
    expect_equal(content, "vetted content")

    unlink(temp_dir, recursive = TRUE)
})

test_that(".copy_vetted_files skips non-OK files", {
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    original_dir <- file.path(temp_dir, "original")
    dir.create(vetted_dir)
    dir.create(original_dir)

    writeLines("vetted", file.path(vetted_dir, "file1.wav"))
    writeLines("vetted", file.path(vetted_dir, "file2.wav"))
    writeLines("original", file.path(original_dir, "file1.wav"))

    comparison <- data.frame(
        File.Name = c("file1.wav", "file2.wav"),
        Vetted.Path = c(
            file.path(vetted_dir, "file1.wav"),
            file.path(vetted_dir, "file2.wav")
        ),
        Original.Path = c(
            file.path(original_dir, "file1.wav"),
            NA
        ),
        Status = c("OK", "MISSING"),
        stringsAsFactors = FALSE
    )

    result <- batr:::`.copy_vetted_files`(comparison)

    expect_equal(result$n_copied, 1)
    expect_equal(result$n_skipped, 1)

    unlink(temp_dir, recursive = TRUE)
})

test_that(".copy_vetted_files handles empty comparison", {
    comparison <- data.frame(
        File.Name = character(0),
        Vetted.Path = character(0),
        Original.Path = character(0),
        Status = character(0),
        stringsAsFactors = FALSE
    )

    result <- batr:::`.copy_vetted_files`(comparison)

    expect_true(result$success)
    expect_equal(result$n_copied, 0)
    expect_equal(result$n_failed, 0)
})

# ----------------------------------------------------------------------------
# Integration tests for manual_vet_reimporter
# ----------------------------------------------------------------------------

test_that("manual_vet_reimporter works end-to-end in dry run mode", {
    skip_on_cran()

    # Create test structure
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    wav_dir <- file.path(temp_dir, "wavs")
    dir.create(file.path(vetted_dir, "Epfu"), recursive = TRUE)
    dir.create(file.path(wav_dir, "Site1"), recursive = TRUE)

    # Create test WAV files
    writeLines("vetted content", file.path(vetted_dir, "Epfu", "file1.wav"))
    writeLines("vetted content", file.path(vetted_dir, "Epfu", "file2.wav"))
    writeLines("original content", file.path(wav_dir, "Site1", "file1.wav"))
    writeLines("original content", file.path(wav_dir, "Site1", "file2.wav"))

    # Create dummy data file
    data_path <- file.path(temp_dir, "data.RData")
    observations <- data.frame(
        File.Name = c("file1.wav", "file2.wav"),
        Species = c("Epfu", "Epfu")
    )
    save(observations, file = data_path)

    # Run in dry run mode (non-interactive)
    expect_message(
        result <- manual_vet_reimporter(
            vetted_directory = vetted_dir,
            WAV_directory = wav_dir,
            data_path = data_path,
            dry_run = TRUE,
            fast_import = FALSE,
            ask_user = FALSE
        ),
        "Dry run complete"
    )

    expect_true(result$success)
    expect_equal(result$n_copied, 0) # Dry run doesn't copy

    # Verify files were NOT overwritten
    content <- readLines(file.path(wav_dir, "Site1", "file1.wav"))
    expect_equal(content, "original content")

    unlink(temp_dir, recursive = TRUE)
})

test_that("manual_vet_reimporter copies files when dry_run = FALSE", {
    skip_on_cran()

    # Create test structure
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    wav_dir <- file.path(temp_dir, "wavs")
    dir.create(file.path(vetted_dir, "Epfu"), recursive = TRUE)
    dir.create(file.path(wav_dir, "Site1"), recursive = TRUE)

    # Create test WAV files
    writeLines("vetted content", file.path(vetted_dir, "Epfu", "file1.wav"))
    writeLines("original content", file.path(wav_dir, "Site1", "file1.wav"))

    # Create dummy data file
    data_path <- file.path(temp_dir, "data.RData")
    observations <- data.frame(File.Name = "file1.wav", Species = "Epfu")
    save(observations, file = data_path)

    # Mock user saying "yes" by setting ask_user = FALSE
    result <- manual_vet_reimporter(
        vetted_directory = vetted_dir,
        WAV_directory = wav_dir,
        data_path = data_path,
        dry_run = FALSE,
        fast_import = FALSE,
        ask_user = FALSE
    )

    expect_true(result$success)
    expect_equal(result$n_copied, 1)

    # Verify file WAS overwritten
    content <- readLines(file.path(wav_dir, "Site1", "file1.wav"))
    expect_equal(content, "vetted content")

    unlink(temp_dir, recursive = TRUE)
})

test_that("manual_vet_reimporter handles nested folder structures", {
    skip_on_cran()

    # Create complex nested structure
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    wav_dir <- file.path(temp_dir, "wavs")

    # Nested vetted structure
    dir.create(file.path(vetted_dir, "Epfu", "Site1"), recursive = TRUE)
    dir.create(file.path(vetted_dir, "Labo", "Site2"), recursive = TRUE)

    # Nested original structure
    dir.create(file.path(wav_dir, "2023", "Site1", "recordings"), recursive = TRUE)
    dir.create(file.path(wav_dir, "2024", "Site2"), recursive = TRUE)

    # Create files in different nested locations
    writeLines("vetted1", file.path(vetted_dir, "Epfu", "Site1", "rec1.wav"))
    writeLines("vetted2", file.path(vetted_dir, "Labo", "Site2", "rec2.wav"))

    writeLines("orig1", file.path(wav_dir, "2023", "Site1", "recordings", "rec1.wav"))
    writeLines("orig2", file.path(wav_dir, "2024", "Site2", "rec2.wav"))

    data_path <- file.path(temp_dir, "data.RData")
    observations <- data.frame(
        File.Name = c("rec1.wav", "rec2.wav"),
        Species = c("Epfu", "Labo")
    )
    save(observations, file = data_path)

    result <- manual_vet_reimporter(
        vetted_directory = vetted_dir,
        WAV_directory = wav_dir,
        data_path = data_path,
        dry_run = FALSE,
        fast_import = FALSE,
        ask_user = FALSE
    )

    expect_true(result$success)
    expect_equal(result$n_copied, 2)

    # Verify files were copied to correct original locations
    expect_equal(
        readLines(file.path(wav_dir, "2023", "Site1", "recordings", "rec1.wav")),
        "vetted1"
    )
    expect_equal(
        readLines(file.path(wav_dir, "2024", "Site2", "rec2.wav")),
        "vetted2"
    )

    unlink(temp_dir, recursive = TRUE)
})

test_that("manual_vet_reimporter errors when directories don't exist", {
    expect_error(
        manual_vet_reimporter(
            vetted_directory = "/nonexistent/vetted",
            WAV_directory = "/nonexistent/wav",
            data_path = "/nonexistent/data.RData",
            ask_user = FALSE
        ),
        "Vetted directory does not exist"
    )
})

test_that("manual_vet_reimporter errors when no WAV files in vetted directory", {
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    vetted_dir <- file.path(temp_dir, "vetted")
    wav_dir <- file.path(temp_dir, "wavs")
    dir.create(vetted_dir)
    dir.create(wav_dir)

    # Create WAV in original but not in vetted
    writeLines("original", file.path(wav_dir, "file1.wav"))

    data_path <- file.path(temp_dir, "data.RData")
    observations <- data.frame(File.Name = "file1.wav", Species = "Epfu")
    save(observations, file = data_path)

    expect_error(
        manual_vet_reimporter(
            vetted_directory = vetted_dir,
            WAV_directory = wav_dir,
            data_path = data_path,
            fast_import = FALSE,
            ask_user = FALSE
        ),
        "No WAV files found"
    )

    unlink(temp_dir, recursive = TRUE)
})
