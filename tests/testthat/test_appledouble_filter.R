test_that("AppleDouble files are filtered out from file discovery", {
    # Create a temporary directory with WAV files and AppleDouble files
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "appledouble_test")
    dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

    # Create some regular WAV files
    regular_file1 <- file.path(test_dir, "recording1.wav")
    regular_file2 <- file.path(test_dir, "site_123.wav")

    # Create AppleDouble files (these should be filtered)
    appledouble_file1 <- file.path(test_dir, "._recording1.wav")
    appledouble_file2 <- file.path(test_dir, "._site_123.wav")

    # Write empty files (just need them to exist)
    file.create(regular_file1, regular_file2, appledouble_file1, appledouble_file2)

    # Test with fast_import = FALSE (standard file listing)
    file_list_standard <- batr:::.get_file_list(test_dir, fast_import = FALSE)

    expect_equal(nrow(file_list_standard), 2)
    expect_true(all(!grepl("^\\._", file_list_standard$File.Name)))
    expect_true("recording1.wav" %in% file_list_standard$File.Name)
    expect_true("site_123.wav" %in% file_list_standard$File.Name)
    expect_false("._recording1.wav" %in% file_list_standard$File.Name)
    expect_false("._site_123.wav" %in% file_list_standard$File.Name)

    # Test with list = TRUE (direct file path input)
    all_files <- c(regular_file1, regular_file2, appledouble_file1, appledouble_file2)
    file_list_direct <- batr:::.get_file_list(all_files, list = TRUE)

    expect_equal(nrow(file_list_direct), 2)
    expect_true(all(!grepl("^\\._", file_list_direct$File.Name)))
    expect_true("recording1.wav" %in% file_list_direct$File.Name)
    expect_true("site_123.wav" %in% file_list_direct$File.Name)

    # Cleanup
    unlink(test_dir, recursive = TRUE)
})

test_that("AppleDouble filter handles edge cases", {
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "appledouble_edge_test")
    dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

    # Create files with underscores but NOT starting with ._
    valid_underscore1 <- file.path(test_dir, "site_name_123.wav")
    valid_underscore2 <- file.path(test_dir, "location_data.wav")

    # Create AppleDouble file
    appledouble <- file.path(test_dir, "._valid_file.wav")

    file.create(valid_underscore1, valid_underscore2, appledouble)

    file_list <- batr:::.get_file_list(test_dir, fast_import = FALSE)

    # Should only get the 2 valid files
    expect_equal(nrow(file_list), 2)
    expect_true("site_name_123.wav" %in% file_list$File.Name)
    expect_true("location_data.wav" %in% file_list$File.Name)
    expect_false("._valid_file.wav" %in% file_list$File.Name)

    # Cleanup
    unlink(test_dir, recursive = TRUE)
})
