# Tests for log file parser functions in R/new_logfileparser.R

library(testthat)
library(batr)

context("logfileparser")

# ----------------------------------------------------------------------------
# Helper: Create mock log directory structure
# ----------------------------------------------------------------------------

make_temp_logs <- function(type = "wa", n_files = 2) {
    td <- file.path(tempdir(), paste0("logs_", as.integer(runif(1, 1e6, 9e6))))
    dir.create(td, recursive = TRUE)

    if (type == "wa") {
        # Wildlife Acoustics format: site-date.txt with DATE and TIME columns
        for (i in seq_len(n_files)) {
            site <- paste0("Site", letters[i])
            fp <- file.path(td, paste0(site, "-2023.txt"))
            content <- data.frame(
                DATE = c("2023-Jan-15", "2023-Jan-15", "2023-Jan-16"),
                TIME = c("20:30:00", "21:45:00", "02:15:00")
            )
            write.table(content, fp, sep = ",", row.names = FALSE, quote = FALSE)
        }
    } else if (type == "swift") {
        # Anabat Swift format: subfolder per site with "log YYYY-MM-DD.csv" files
        for (i in seq_len(n_files)) {
            site <- paste0("SwiftSite", letters[i])
            site_dir <- file.path(td, site)
            dir.create(site_dir)
            fp <- file.path(site_dir, "log 2023-01-15.csv")
            # Simplified Swift log format (3 columns minimum)
            content <- data.frame(
                col1 = c("Event1", "Event2", "Event3"),
                col2 = c("FILE", "FILE", "Status"),
                col3 = c("Info", "Info", "Status: Recording now"),
                stringsAsFactors = FALSE
            )
            write.table(content, fp, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
        }
    } else if (type == "ranger") {
        # Anabat Ranger format: subfolder per site with "ranger-YYYY-MM-DD-*.csv" files
        for (i in seq_len(n_files)) {
            site <- paste0("RangerSite", letters[i])
            site_dir <- file.path(td, site)
            dir.create(site_dir)
            fp <- file.path(site_dir, "ranger-2023-01-15-001.csv")
            # Simplified Ranger log format (4 columns)
            content <- data.frame(
                Date = c("2023-01-15", "2023-01-15", "2023-01-15"),
                MessageType = c("Status", "File", "Status"),
                Function = c("Check", "Record", "Record"),
                Note = c("Info", "File: recording.wav", "Status: ALL CLEAR - ready to record"),
                stringsAsFactors = FALSE
            )
            write.table(content, fp, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
        }
    }

    return(td)
}

# ----------------------------------------------------------------------------
# .read_wa_logs
# ----------------------------------------------------------------------------

test_that(".read_wa_logs parses Wildlife Acoustics files correctly", {
    td <- make_temp_logs("wa", n_files = 2)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    file_list <- list.files(td, pattern = "\\.txt$", full.names = TRUE)
    expect_true(length(file_list) == 2)

    result <- batr:::`.read_wa_logs`(file_list, td)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("Date", "Location", "Log_Count") %in% names(result)))
    expect_s3_class(result$Date, "Date")
    expect_type(result$Log_Count, "integer")

    # Should have entries for both sites
    expect_true(length(unique(result$Location)) == 2)
})

test_that(".read_wa_logs handles noon-to-noon night convention", {
    td <- make_temp_logs("wa", n_files = 1)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    file_list <- list.files(td, pattern = "\\.txt$", full.names = TRUE)
    result <- batr:::`.read_wa_logs`(file_list, td)

    # Recording at 02:15:00 on Jan-16 should be assigned to Jan-15 night
    jan15_count <- result$Log_Count[result$Date == as.Date("2023-01-15")]
    # Should have 3 recordings (two on Jan-15 evening, one early morning Jan-16)
    expect_equal(jan15_count, 3)
})

# ----------------------------------------------------------------------------
# .read_swift_logs
# ----------------------------------------------------------------------------

test_that(".read_swift_logs parses Anabat Swift files correctly", {
    td <- make_temp_logs("swift", n_files = 2)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    file_list <- list.files(td,
        pattern = "log \\d{4}-\\d{2}-\\d{2}\\.csv$",
        full.names = TRUE, recursive = TRUE
    )
    expect_true(length(file_list) == 2)

    result <- batr:::`.read_swift_logs`(file_list, td)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("Date", "Location", "Log_Count") %in% names(result)))
    expect_s3_class(result$Date, "Date")

    # Should have entries for both sites
    expect_true(length(unique(result$Location)) == 2)

    # Mock files indicate active recording (Log_Count = 1)
    expect_true(all(result$Log_Count == 1))
})

test_that(".read_swift_logs detects inactive recorders", {
    td <- make_temp_logs("swift", n_files = 1)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    # Modify log to indicate microphone failure
    site_dir <- list.dirs(td, recursive = FALSE)[1]
    log_file <- list.files(site_dir, full.names = TRUE)[1]

    bad_content <- data.frame(
        col1 = c("Event1", "Event2"),
        col2 = c("Status", "Status"),
        col3 = c("Status: Check microphone", "Status: Check microphone"),
        stringsAsFactors = FALSE
    )
    write.table(bad_content, log_file,
        sep = ",", row.names = FALSE,
        col.names = TRUE, quote = FALSE
    )

    file_list <- list.files(td,
        pattern = "log \\d{4}-\\d{2}-\\d{2}\\.csv$",
        full.names = TRUE, recursive = TRUE
    )
    result <- batr:::`.read_swift_logs`(file_list, td)

    # Should detect failure (Log_Count = 0)
    expect_equal(result$Log_Count, 0)
})

# ----------------------------------------------------------------------------
# .read_ranger_logs
# ----------------------------------------------------------------------------

test_that(".read_ranger_logs parses Anabat Ranger files correctly", {
    td <- make_temp_logs("ranger", n_files = 2)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    file_list <- list.files(td,
        pattern = "ranger.*\\.csv$",
        full.names = TRUE, recursive = TRUE
    )
    expect_true(length(file_list) == 2)

    result <- batr:::`.read_ranger_logs`(file_list, td)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("Date", "Location", "Log_Count") %in% names(result)))
    expect_s3_class(result$Date, "Date")

    # Should have entries for both sites
    expect_true(length(unique(result$Location)) == 2)

    # Mock files indicate active recording (Log_Count = 1)
    expect_true(all(result$Log_Count == 1))
})

# ----------------------------------------------------------------------------
# .gap_generator
# ----------------------------------------------------------------------------

test_that(".gap_generator fills missing dates", {
    active <- data.frame(
        Date = as.Date(c("2023-01-01", "2023-01-03", "2023-01-01")),
        Location = c("A", "A", "B"),
        Log_Count = c(1, 1, 1)
    )

    result <- batr:::`.gap_generator`(active, "2023-01-01", "2023-01-03")

    # Should have 3 dates × 2 locations = 6 rows
    expect_equal(nrow(result), 6)

    # Missing date (Jan-02 for both sites) should have Log_Count = 0
    jan02 <- result[result$Date == as.Date("2023-01-02"), ]
    expect_equal(nrow(jan02), 2)
    expect_true(all(jan02$Log_Count == 0))
})

test_that(".gap_generator uses data range when start/end not specified", {
    active <- data.frame(
        Date = as.Date(c("2023-01-05", "2023-01-10")),
        Location = c("A", "A"),
        Log_Count = c(1, 1)
    )

    result <- batr:::`.gap_generator`(active, NULL, NULL)

    # Should span from 2023-01-05 to 2023-01-10 (6 days)
    expect_equal(nrow(result), 6)
    expect_equal(min(result$Date), as.Date("2023-01-05"))
    expect_equal(max(result$Date), as.Date("2023-01-10"))
})

# ----------------------------------------------------------------------------
# import_logs (integration)
# ----------------------------------------------------------------------------

test_that("import_logs validates input path", {
    nonexistent <- file.path(tempdir(), "definitely_missing_log_dir_zzz")
    rdata_tmp <- tempfile(fileext = ".RData")

    expect_error(
        import_logs(nonexistent, data_path = rdata_tmp),
        "Log path does not exist"
    )
})

test_that("import_logs validates date formats", {
    td <- make_temp_logs("wa", n_files = 1)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    rdata_tmp <- tempfile(fileext = ".RData")

    expect_error(
        import_logs(td, data_path = rdata_tmp, monitoring_start = "not-a-date"),
        "Invalid monitoring_start date"
    )
})

test_that("import_logs handles empty directory", {
    td <- tempfile("empty_logs_")
    dir.create(td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    rdata_tmp <- tempfile(fileext = ".RData")

    expect_error(
        import_logs(td, data_path = rdata_tmp),
        "No log files found"
    )
})

test_that("import_logs processes Wildlife Acoustics logs end-to-end", {
    td <- make_temp_logs("wa", n_files = 2)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    rdata_tmp <- tempfile(fileext = ".RData")
    on.exit(unlink(rdata_tmp), add = TRUE)

    # Mock .validate_rdata_path and .save_to_RDATA
    mock_validate <- function(path, must_exist = TRUE) {
        if (is.null(path)) {
            stop("data_path cannot be NULL")
        }
        return(invisible(NULL))
    }
    mock_save <- function(active_dates, data_path) {
        save(active_dates, file = data_path)
    }

    testthat::with_mocked_bindings(
        {
            result <- import_logs(td, data_path = rdata_tmp)

            expect_true(file.exists(rdata_tmp))

            # Load and verify
            env <- new.env()
            load(rdata_tmp, envir = env)
            expect_true(exists("active_dates", envir = env))
            expect_s3_class(env$active_dates, "data.frame")
            expect_true(all(c("Date", "Location", "Log_Count") %in% names(env$active_dates)))
        },
        .validate_rdata_path = mock_validate,
        .save_to_rdata = mock_save
    )
})
