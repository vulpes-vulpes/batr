#' Reintegrate Manually Vetted Files Back to Original Directory
#'
#' \code{manual_vet_reimporter} copies manually vetted WAV files from a vetting
#' directory back to their original locations in the main WAV directory,
#' overwriting the originals. Files are matched by filename only, ignoring
#' folder structure. Use this after manually reviewing files extracted with
#' \code{manual_vet_extractor}.
#'
#' @family Manual Vetting Functions
#'
#' @param vetted_directory Character. Path to directory containing manually
#'   vetted WAV files (organized by species/location folders from
#'   \code{manual_vet_extractor}).
#' @param WAV_directory Character. Path to main WAV directory where original
#'   files will be overwritten.
#' @param data_path Character. Path to RData file (for reference in final
#'   message).
#' @param dry_run Logical. If \code{TRUE} (default), shows comparison table and
#'   prompts before copying. If \code{FALSE}, skips table and proceeds with
#'   warning only.
#' @param fast_import Logical. If \code{TRUE} (default), uses optimized file
#'   discovery. If \code{FALSE}, uses standard R file operations.
#' @param ask_user Logical. Whether to prompt user for input when issues arise.
#'   Defaults to \code{interactive()}.
#'
#' @return Invisible list with elements:
#'   \describe{
#'     \item{success}{Logical, TRUE if all files copied successfully}
#'     \item{n_copied}{Integer, number of files successfully copied}
#'     \item{n_failed}{Integer, number of files that failed to copy}
#'     \item{n_skipped}{Integer, number of files skipped}
#'     \item{failed_files}{Character vector of filenames that failed}
#'   }
#'
#' @details After manual vetting (editing Species.Manual.ID in GUANO metadata),
#'   this function copies the vetted files back to overwrite their originals.
#'   Files are matched by filename only, so folder structure in
#'   \code{vetted_directory} is ignored.
#'
#'   In dry-run mode (default), a detailed comparison table is displayed showing
#'   file sizes and modification times before prompting to proceed. This allows
#'   verification that files were actually modified during vetting.
#'
#'   After copying, run \code{import_guano(action = "Update", ...)} to update
#'   your RData file with the new Species.Manual.ID values.
#'
#' @examples
#' \dontrun{
#' # Default: dry run with comparison table
#' manual_vet_reimporter(
#'     vetted_directory = "path/to/vetted/files",
#'     WAV_directory = "path/to/original/wavs",
#'     data_path = "path/to/data.RData"
#' )
#'
#' # Skip dry run, proceed with warning
#' manual_vet_reimporter(
#'     vetted_directory = "path/to/vetted/files",
#'     WAV_directory = "path/to/original/wavs",
#'     data_path = "path/to/data.RData",
#'     dry_run = FALSE
#' )
#' }
#' @export
manual_vet_reimporter <- function(vetted_directory,
                                  WAV_directory,
                                  data_path,
                                  dry_run = TRUE,
                                  fast_import = TRUE,
                                  ask_user = interactive()) {
    message("\n========== Starting Manual Vetting Reintegration ==========")

    # Validate inputs
    .validate_reimport_directories(vetted_directory, WAV_directory)

    # Discover files
    message("Discovering files...")
    vetted_files <- .discover_files(vetted_directory, "vetted", fast_import)
    original_files <- .discover_files(WAV_directory, "original", fast_import)

    # Match files and build comparison data
    match_result <- .match_vetted_to_original(vetted_files, original_files)

    # Handle issues (missing, duplicates, conflicts)
    if (!.handle_file_issues(match_result, ask_user)) {
        message("\n========== Reintegration Cancelled ==========")
        return(invisible(list(
            success = FALSE,
            n_copied = 0,
            n_failed = 0,
            n_skipped = nrow(match_result$comparison),
            failed_files = character(0)
        )))
    }

    # Display results based on dry_run mode
    if (dry_run) {
        # Show detailed comparison table
        .display_comparison_table(match_result$comparison)
        .display_summary(match_result)

        # Ask to proceed
        if (ask_user) {
            response <- readline(prompt = sprintf(
                "\nProceed with copying %d files? (y/n): ",
                match_result$n_ready
            ))
            if (!tolower(trimws(response)) %in% c("y", "yes")) {
                message("Operation cancelled by user.")
                message("\n========== Reintegration Cancelled ==========")
                return(invisible(list(
                    success = FALSE,
                    n_copied = 0,
                    n_failed = 0,
                    n_skipped = match_result$n_ready,
                    failed_files = character(0)
                )))
            }
        } else {
            message("\nDry run complete. Run again with dry_run = FALSE to copy files.")
            message("\n========== Reintegration Complete (Dry Run) ==========")
            return(invisible(list(
                success = TRUE,
                n_copied = 0,
                n_failed = 0,
                n_skipped = 0,
                failed_files = character(0)
            )))
        }
    } else {
        # Dry run disabled - show warning only
        message(sprintf(
            "\nWARNING: About to overwrite %d files in %s",
            match_result$n_ready,
            WAV_directory
        ))

        if (ask_user) {
            response <- readline(prompt = "Are you sure you want to proceed? (y/n): ")
            if (!tolower(trimws(response)) %in% c("y", "yes")) {
                message("Operation cancelled by user.")
                message("\n========== Reintegration Cancelled ==========")
                return(invisible(list(
                    success = FALSE,
                    n_copied = 0,
                    n_failed = 0,
                    n_skipped = match_result$n_ready,
                    failed_files = character(0)
                )))
            }
        }
    }

    # Proceed with copying
    copy_result <- .copy_vetted_files(match_result$comparison)

    # Display final results
    .display_copy_results(copy_result)
    .display_import_instructions(WAV_directory, data_path)

    message("\n========== Reintegration Complete ==========")

    invisible(copy_result)
}

# Helper Functions ------------------------------------------------------------

#' Validate directories for reimport
#' @keywords internal
.validate_reimport_directories <- function(vetted_directory, WAV_directory) {
    if (!dir.exists(vetted_directory)) {
        stop(
            "Vetted directory does not exist: '", vetted_directory, "'.\n",
            "Please check the path and try again."
        )
    }
    if (!dir.exists(WAV_directory)) {
        stop(
            "WAV directory does not exist: '", WAV_directory, "'.\n",
            "Please check the path and try again."
        )
    }
    invisible(TRUE)
}

#' Discover WAV files in a directory
#' @keywords internal
.discover_files <- function(directory, label, fast_import) {
    message(sprintf("  Scanning %s directory: %s", label, directory))

    file_list <- .get_file_list(directory, fast_import)

    if (nrow(file_list) == 0) {
        stop(sprintf("No WAV files found in %s directory: %s", label, directory))
    }

    message(sprintf("  Found %d files in %s directory", nrow(file_list), label))
    return(file_list)
}

#' Match vetted files to original files by filename
#' @keywords internal
.match_vetted_to_original <- function(vetted_files, original_files) {
    message("\nMatching vetted files to originals...")

    # Check for duplicate filenames in vetted directory
    vetted_dupes <- vetted_files$File.Name[duplicated(vetted_files$File.Name)]
    if (length(vetted_dupes) > 0) {
        stop(
            "Duplicate filenames found in vetted directory:\n  ",
            paste(unique(vetted_dupes), collapse = "\n  "),
            "\n\nPlease ensure each filename appears only once in the vetted directory."
        )
    }

    # Build lookup table for original files
    original_lookup <- split(original_files$Full.Path, original_files$File.Name)

    # Build comparison data frame
    comparison <- data.frame(
        File.Name = vetted_files$File.Name,
        Vetted.Path = vetted_files$Full.Path,
        Original.Path = NA_character_,
        Vetted.Size = file.size(vetted_files$Full.Path),
        Original.Size = NA_real_,
        Vetted.Modified = file.mtime(vetted_files$Full.Path),
        Original.Modified = as.POSIXct(NA),
        Status = NA_character_,
        stringsAsFactors = FALSE
    )

    # Match files
    n_missing <- 0
    n_conflict <- 0
    n_size_diff <- 0
    n_ok <- 0

    for (i in seq_len(nrow(comparison))) {
        filename <- comparison$File.Name[i]
        original_paths <- original_lookup[[filename]]

        if (is.null(original_paths)) {
            # Not found
            comparison$Status[i] <- "MISSING"
            n_missing <- n_missing + 1
        } else if (length(original_paths) > 1) {
            # Multiple matches (conflict)
            comparison$Original.Path[i] <- paste(original_paths, collapse = " | ")
            comparison$Status[i] <- "CONFLICT"
            n_conflict <- n_conflict + 1
        } else {
            # Single match - get file info
            comparison$Original.Path[i] <- original_paths[1]
            comparison$Original.Size[i] <- file.size(original_paths[1])
            comparison$Original.Modified[i] <- file.mtime(original_paths[1])

            # Check size difference
            size_diff_pct <- abs(comparison$Vetted.Size[i] - comparison$Original.Size[i]) /
                comparison$Original.Size[i] * 100

            if (size_diff_pct > 10) {
                comparison$Status[i] <- "SIZE DIFF"
                n_size_diff <- n_size_diff + 1
            } else {
                comparison$Status[i] <- "OK"
                n_ok <- n_ok + 1
            }
        }
    }

    list(
        comparison = comparison,
        n_total = nrow(comparison),
        n_ok = n_ok,
        n_missing = n_missing,
        n_conflict = n_conflict,
        n_size_diff = n_size_diff,
        n_ready = n_ok + n_size_diff
    )
}

#' Handle file matching issues (missing, conflicts)
#' @keywords internal
.handle_file_issues <- function(match_result, ask_user) {
    has_issues <- match_result$n_missing > 0 || match_result$n_conflict > 0

    if (!has_issues) {
        return(TRUE) # No issues, proceed
    }

    # Display issues
    if (match_result$n_missing > 0) {
        message(sprintf(
            "\nWARNING: %d vetted files not found in WAV directory:",
            match_result$n_missing
        ))
        missing_files <- match_result$comparison$File.Name[
            match_result$comparison$Status == "MISSING"
        ]
        for (i in seq_along(missing_files)) {
            message(sprintf("  - %s", missing_files[i]))
        }
    }

    if (match_result$n_conflict > 0) {
        message(sprintf(
            "\nWARNING: %d vetted files have multiple matches in WAV directory:",
            match_result$n_conflict
        ))
        conflict_rows <- match_result$comparison[match_result$comparison$Status == "CONFLICT", ]
        for (i in seq_len(nrow(conflict_rows))) {
            message(sprintf("  - %s found at:", conflict_rows$File.Name[i]))
            paths <- strsplit(conflict_rows$Original.Path[i], " \\| ")[[1]]
            for (path in paths) {
                message(sprintf("      %s", path))
            }
        }
    }

    if (!ask_user) {
        message("\nCannot proceed in non-interactive mode with missing/conflicting files.")
        return(FALSE)
    }

    # Ask user what to do
    message(sprintf(
        "\n%d files ready to copy, %d problematic files.",
        match_result$n_ready,
        match_result$n_missing + match_result$n_conflict
    ))
    response <- readline(prompt = "Skip problematic files and continue with remaining? (y/n): ")

    if (tolower(trimws(response)) %in% c("y", "yes")) {
        # Filter out problematic files
        match_result$comparison <- match_result$comparison[
            match_result$comparison$Status %in% c("OK", "SIZE DIFF"),
        ]
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Display comparison table
#' @keywords internal
.display_comparison_table <- function(comparison) {
    message("\n========== File Comparison Table ==========")

    # Format sizes in MB
    format_size <- function(bytes) {
        if (is.na(bytes)) {
            return("-")
        }
        sprintf("%.2f MB", bytes / 1024^2)
    }

    # Format dates
    format_date <- function(dt) {
        if (is.na(dt)) {
            return("-")
        }
        format(dt, "%Y-%m-%d %H:%M:%S")
    }

    # Prepare display table
    display <- data.frame(
        `File Name` = comparison$File.Name,
        `Vetted Size` = sapply(comparison$Vetted.Size, format_size),
        `Original Size` = sapply(comparison$Original.Size, format_size),
        `Vetted Modified` = sapply(comparison$Vetted.Modified, format_date),
        `Original Modified` = sapply(comparison$Original.Modified, format_date),
        Status = comparison$Status,
        check.names = FALSE,
        stringsAsFactors = FALSE
    )

    # Print table
    print(display, row.names = FALSE, right = FALSE)
}

#' Display summary statistics
#' @keywords internal
.display_summary <- function(match_result) {
    message("\n========== Summary ==========")
    message(sprintf("  Total vetted files: %d", match_result$n_total))
    message(sprintf("  Ready to copy: %d", match_result$n_ready))
    if (match_result$n_missing > 0) {
        message(sprintf("  Missing in WAV directory: %d", match_result$n_missing))
    }
    if (match_result$n_conflict > 0) {
        message(sprintf("  Conflicts (duplicates): %d", match_result$n_conflict))
    }
    if (match_result$n_size_diff > 0) {
        message(sprintf("  Size mismatches (>10%%): %d", match_result$n_size_diff))
    }
}

#' Copy vetted files to original locations
#' @keywords internal
.copy_vetted_files <- function(comparison) {
    # Filter to copyable files only
    to_copy <- comparison[comparison$Status %in% c("OK", "SIZE DIFF"), ]

    if (nrow(to_copy) == 0) {
        return(list(
            success = TRUE,
            n_copied = 0,
            n_failed = 0,
            n_skipped = 0,
            failed_files = character(0)
        ))
    }

    message(sprintf("\nCopying %d files...", nrow(to_copy)))

    n_copied <- 0
    n_failed <- 0
    failed_files <- character(0)

    for (i in seq_len(nrow(to_copy))) {
        if (nrow(to_copy) > 10 && i %% 10 == 0) {
            message(sprintf("  Progress: %d of %d files...", i, nrow(to_copy)))
        }

        success <- file.copy(
            from = to_copy$Vetted.Path[i],
            to = to_copy$Original.Path[i],
            overwrite = TRUE
        )

        if (success) {
            n_copied <- n_copied + 1
        } else {
            n_failed <- n_failed + 1
            failed_files <- c(failed_files, to_copy$File.Name[i])
        }
    }

    list(
        success = n_failed == 0,
        n_copied = n_copied,
        n_failed = n_failed,
        n_skipped = nrow(comparison) - nrow(to_copy),
        failed_files = failed_files
    )
}

#' Display copy results
#' @keywords internal
.display_copy_results <- function(copy_result) {
    message("\n========== Results ==========")
    message(sprintf("  Successfully copied: %d files", copy_result$n_copied))
    if (copy_result$n_failed > 0) {
        message(sprintf("  Failed to copy: %d files", copy_result$n_failed))
        message("  Failed files:")
        for (f in copy_result$failed_files) {
            message(sprintf("    - %s", f))
        }
    }
    if (copy_result$n_skipped > 0) {
        message(sprintf("  Skipped: %d files", copy_result$n_skipped))
    }
}

#' Display import_guano instructions
#' @keywords internal
.display_import_instructions <- function(WAV_directory, data_path) {
    message("\n========== Next Steps ==========")
    message("To update your RData file with the new Species.Manual.ID values, run:")
    message("")
    message("  batr::import_guano(")
    message("    action = \"Update\",")
    message(sprintf("    input_path = \"%s\",", WAV_directory))
    message("    site_col = \"[your_site_column]\",")
    message("    timezone = \"[your_timezone]\",")
    message(sprintf("    data_path = \"%s\"", data_path))
    message("  )")
    message("")
}
