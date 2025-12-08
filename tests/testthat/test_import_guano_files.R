# Tests for file discovery helpers in R/new_importer.R

library(testthat)
library(batr)

context("new_importer file discovery")

make_temp_wavs <- function(n = 3, exts = c(".wav", ".WAV")) {
    td <- file.path(tempdir(), paste0("ni_", as.integer(runif(1, 1e6, 9e6))))
    dir.create(td, recursive = TRUE)
    files <- character(0)
    for (i in seq_len(n)) {
        ext <- exts[(i - 1) %% length(exts) + 1]
        fp <- file.path(td, paste0("file_", i, ext))
        writeBin(raw(), fp)
        files <- c(files, fp)
    }
    # Add a non-wav to ensure it's ignored
    writeLines("not a wav", file.path(td, "note.txt"))
    list(dir = td, files = files)
}

# ----------------------------------------------------------------------------
# .get_file_list standard path
# ----------------------------------------------------------------------------

test_that(".get_file_list finds wavs with standard listing", {
    tmp <- make_temp_wavs(n = 4)
    on.exit(unlink(tmp$dir, recursive = TRUE), add = TRUE)
    fl <- batr:::`.get_file_list`(tmp$dir, fast_import = FALSE)
    expect_s3_class(fl, "data.frame")
    expect_true(all(c("File.Name", "Full.Path", "File.Modified") %in% names(fl)))
    expect_equal(nrow(fl), length(tmp$files))
    expect_true(all(grepl("\\.wav$", fl$File.Name, ignore.case = TRUE)))
    expect_type(fl$File.Modified, "double")
})

# ----------------------------------------------------------------------------
# .get_file_list(list=TRUE)
# ----------------------------------------------------------------------------

test_that(".get_file_list handles list=TRUE correctly", {
    tmp <- make_temp_wavs(n = 2)
    on.exit(unlink(tmp$dir, recursive = TRUE), add = TRUE)
    fl <- batr:::`.get_file_list`(tmp$files, fast_import = FALSE, list = TRUE)
    expect_equal(nrow(fl), length(tmp$files))
    expect_setequal(fl$Full.Path, tmp$files)
})

# ----------------------------------------------------------------------------
# .get_file_list_singlepass (best-effort; skip if unavailable)
# ----------------------------------------------------------------------------

test_that(".get_file_list_singlepass returns expected columns", {
    # Only test on unix/macOS where find+stat should exist
    if (.Platform$OS.type != "unix") skip("singlepass test only on unix/macOS")
    tmp <- make_temp_wavs(n = 3)
    on.exit(unlink(tmp$dir, recursive = TRUE), add = TRUE)
    sp <- batr:::`.get_file_list_singlepass`(tmp$dir)
    if (nrow(sp) == 0) skip("singlepass not available in this environment")
    expect_true(all(c("Full.Path", "File.Modified") %in% names(sp)))
    expect_true(all(file.exists(sp$Full.Path)))
})
