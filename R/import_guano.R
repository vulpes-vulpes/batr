#' Read GUANO Metadata From WAV Files
#'
#' \code{import_guano} reads GUANO metadata stored in WAV files generated
#' through bat acoustic monitoring and saves them as a data.frame in an RData
#' file for easy reference. This function can generate a new RData file, add
#' GUANO from new files to an existing RData file, or update metadata in an
#' existing RData file where the metadata of the original files has changed.
#'
#' @section Required GUANO Fields:
#' Files must contain the following metadata fields:
#' \itemize{
#'   \item Location / site name (specified by \code{site_col})
#'   \item Timestamp
#'   \item Latitude and Longitude (from \code{Loc Position})
#' }
#' Species identifiers (\code{Species.Manual.ID} or \code{Species.Auto.ID}) are
#' recommended but optional; missing species IDs trigger a warning.
#'
#' @section Note: This function will take a long time for large data sets.
#'   Folders containing tens of thousands of files may take several hours to
#'   read! For this reason it is recommended to complete all manual vetting
#'   before proceeding. However, files with changed metadata can be updated later
#'   using the "Update" action.
#'
#' @family Import FunctionsImport Functions
#'
#' @param action Character. Must equal "New", "Add" or "Update". Use "New" to
#'   read GUANO from a new set of files and create a fresh RData file. Use "Add"
#'   to add GUANO from a new set of files to an existing RData file that already
#'   contains other data. Use "Update" to update an existing RData files from
#'   WAV that have been previously imported and then had their GUANO metadata
#'   changed.
#'
#' @param input_path Character. Path to the directory containing the files you
#'   wish to read the metadata from. Note that sub-directories are also read.
#'
#' @param site_col Character. Name of the GUANO metadata field specifying the
#'   recording location / site name.
#'
#' @param timezone Character. The timezone to be used for the timestamps in the
#'   metadata in the TZ identifer format (https://en.wikipedia.org/wiki/List_of
#'   _tz_database_time_zones), e.g. "America/New_York".
#'
#' @param data_path Character. Path to an existing RData file. Optional when
#'   "New" action is specified (a location will be requested before data are
#'   read and saved). Required when "Add" or "Update" actions are specified.
#'
#' @param fast_import Logical (default: TRUE). When TRUE, uses optimized single-pass
#'   system calls to rapidly discover WAV files and their modification times. When
#'   FALSE, uses standard R functions (slower but more portable).
#'
#' @return An RData file saved in a specified location containing GUANO metadata
#'   read from WAV files created through bat acoustic monitoring.
#'
#' @family Import Functionsimport tools
#'
#' @examples
#' \dontrun{
#' # New import (cross-platform paths)
#' import_guano(
#'   "New", "path/to/wav/files", "Location",
#'   "America/New_York", "path/to/data.RData"
#' )
#'
#' # Windows example
#' import_guano(
#'   "New", "C:/Folder/WAVs", "Location",
#'   "America/New_York", "C:/Folder/Data.RData"
#' )
#' }
#' @export
import_guano <- function(action, input_path, site_col, timezone, data_path = NULL, fast_import = TRUE) {
  # Validate action
  if (!action %in% c("New", "Add", "Update")) {
    stop(
      "Invalid action given, please specify an action (\"New\", \"Add\" or \"Update\") and try again. ",
      "See ?import_guano for more information."
    )
  }

  # Validate data path (allow creating new file for "New" action)
  .validate_rdata_path(data_path, must_exist = (action != "New"))

  # Execute appropriate action
  if (action == "New") {
    .new_observations(input_path, site_col, timezone, data_path, fast_import)
  } else if (action == "Add") {
    .add_observations(input_path, site_col, timezone, data_path, fast_import)
  } else if (action == "Update") {
    .update_observations(input_path, site_col, timezone, data_path, fast_import)
  }
}

#' Validate timezone string
#' @keywords internal
.validate_timezone <- function(timezone) {
  if (!timezone %in% OlsonNames()) {
    stop(
      "Invalid timezone: '", timezone, "'.\n",
      "Please use a valid timezone from OlsonNames().\n",
      "Common examples: 'America/New_York', 'America/Los_Angeles', 'Europe/London', 'UTC'."
    )
  }
  invisible(TRUE)
}

#' Validate input path exists
#' @keywords internal
.validate_input_path <- function(input_path) {
  if (!dir.exists(input_path)) {
    stop(
      "Input path does not exist: '", input_path, "'.\n",
      "Please check the path and try again."
    )
  }
  invisible(TRUE)
}

.new_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  .validate_timezone(timezone)
  .validate_input_path(input_path)

  message("\n========== Starting New Import ==========")
  start_time <- Sys.time()

  file_list <- .get_file_list(input_path, fast_import)
  observations <- .read_file_guano(file_list, site_col, timezone, fast_import)
  .save_to_rdata(observations, data_path)

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
  message("\n========== Import Complete ==========")
  message(sprintf(
    "Processed %d observations from %d files in %s seconds.",
    nrow(observations), nrow(file_list), elapsed
  ))
  message(sprintf("Data saved to: %s", data_path))

  invisible(NULL)
}

.add_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  .validate_timezone(timezone)
  .validate_input_path(input_path)

  message("\n========== Starting Add Import ==========")
  start_time <- Sys.time()

  # Load existing data into local env to avoid polluting global env
  message("Loading existing data from: ", data_path)
  env <- new.env()
  load(data_path, envir = env)
  if (!exists("observations", envir = env)) {
    stop(
      "Loaded RData does not contain an object named 'observations'.\n",
      "Please ensure you are loading a valid GUANO import file."
    )
  }
  original_observations <- env$observations
  message(sprintf("Existing data contains %d observations.", nrow(original_observations)))

  # Get candidate files (file list only)
  candidate_files <- .get_file_list(input_path, fast_import)
  # Determine which files are truly new
  is_existing <- candidate_files$File.Name %in% original_observations$File.Name
  n_duplicate <- sum(is_existing)

  if (all(is_existing)) {
    stop(
      "All ", nrow(candidate_files), " files already exist in the data file.\n",
      "No new files to import. Use action='Update' to refresh existing files."
    )
  }
  if (any(is_existing)) {
    message(sprintf(
      "Found %d duplicate files (skipping). Importing %d new files.",
      n_duplicate, sum(!is_existing)
    ))
  }
  files_to_import <- candidate_files[!is_existing, , drop = FALSE]

  # Read GUANO metadata for these files
  observations_new <- .read_file_guano(files_to_import, site_col, timezone, fast_import)

  # Harmonize column names: add missing columns as NA to both
  cols_union <- union(names(original_observations), names(observations_new))
  for (cname in setdiff(cols_union, names(original_observations))) original_observations[[cname]] <- NA
  for (cname in setdiff(cols_union, names(observations_new))) observations_new[[cname]] <- NA

  # Combine original and new observations
  observations <- rbind(original_observations[, cols_union], observations_new[, cols_union])
  observations <- observations[order(observations$File.Name), ]
  rownames(observations) <- NULL

  # Save updated data
  .save_to_rdata(observations, data_path)

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
  message("\n========== Add Complete ==========")
  message(sprintf("Added %d new observations in %s seconds.", nrow(observations_new), elapsed))
  message(sprintf("Total observations in dataset: %d", nrow(observations)))
  message(sprintf("Data saved to: %s", data_path))

  invisible(NULL)
}

.update_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  .validate_timezone(timezone)
  .validate_input_path(input_path)

  message("\n========== Starting Update Import ==========")
  start_time <- Sys.time()

  # Load existing data into local env to avoid polluting global env
  message("Loading existing data from: ", data_path)
  env <- new.env()
  load(data_path, envir = env)
  if (!exists("observations", envir = env)) {
    stop(
      "RData does not contain 'observations'.\n",
      "Please ensure you are loading a valid GUANO import file."
    )
  }
  observations_all <- env$observations
  message(sprintf("Existing data contains %d observations.", nrow(observations_all)))

  # Keep key file metadata
  observations_files <- observations_all[, c("File.Name", "File.Path", "File.Modified")]
  names(observations_files)[1:3] <- c("File.Name", "Observations.Path", "Observations.Modified")

  # Get current file list from directory
  directory_files <- .get_file_list(input_path, fast_import)
  files_comparison <- merge(directory_files, observations_files, by = "File.Name", all = TRUE)
  files_comparison <- files_comparison[!is.na(files_comparison$Full.Path), ]

  # Determine which files changed: compare numeric modified times
  files_comparison$update <- ifelse(
    mapply(function(x, y) isTRUE(all.equal(x, y)), files_comparison$File.Modified, files_comparison$Observations.Modified),
    "N",
    ifelse(files_comparison$File.Modified > files_comparison$Observations.Modified, "Y", "N")
  )

  # Subset to files to update
  update_files <- subset(files_comparison, update == "Y")
  if (nrow(update_files) == 0) stop("No files to update!")

  # Read updated files
  update_paths <- update_files$Full.Path
  update_list <- .get_file_list(update_paths, fast_import, list = TRUE)

  # Read GUANO metadata for these files
  observations_update <- .read_file_guano(update_list, site_col, timezone, fast_import)

  # Keep original observations that are not being updated
  observations_original <- observations_all[!(observations_all$File.Name %in% observations_update$File.Name), ]

  # Harmonize columns
  cols_union <- union(names(observations_original), names(observations_update))
  for (cname in setdiff(cols_union, names(observations_original))) observations_original[[cname]] <- NA
  for (cname in setdiff(cols_union, names(observations_update))) observations_update[[cname]] <- NA

  # Combine original and updated observations
  observations <- rbind(observations_original[, cols_union], observations_update[, cols_union])
  observations <- observations[order(observations$File.Name), ]
  rownames(observations) <- NULL

  # Save updated data
  .save_to_rdata(observations, data_path)

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
  message("\n========== Update Complete ==========")
  message(sprintf("Updated %d observations in %s seconds.", nrow(update_files), elapsed))
  message(sprintf("Total observations in dataset: %d", nrow(observations)))
  message(sprintf("Data saved to: %s", data_path))

  invisible(NULL)
}

#' Create Species Column from Manual/Auto IDs
#'
#' Generates a unified Species column by preferring manual IDs over automatic IDs.
#' Handles cases where one or both ID columns are missing.
#'
#' @param observations Data.frame containing GUANO metadata.
#'
#' @return Data.frame with added \code{Species} column.
#'
#' @keywords internal
.create_species_column <- function(observations) {
  has_manual <- "Species.Manual.ID" %in% colnames(observations)
  has_auto <- "Species.Auto.ID" %in% colnames(observations)

  if (!has_manual && !has_auto) {
    warning("Neither Species.Manual.ID nor Species.Auto.ID found in metadata. ",
      "Species column will be NA for all observations. ",
      "Species-dependent summaries, plots, and reports may be empty or fail.",
      call. = FALSE
    )
    observations$Species <- NA_character_
  } else if (has_manual && !has_auto) {
    observations$Species <- as.character(observations$Species.Manual.ID)
  } else if (!has_manual && has_auto) {
    observations$Species <- as.character(observations$Species.Auto.ID)
  } else {
    # Both exist: prefer manual, fall back to auto
    observations <- dplyr::mutate(observations,
      Species = dplyr::if_else(is.na(Species.Manual.ID),
        as.character(Species.Auto.ID),
        as.character(Species.Manual.ID)
      )
    )
  }

  return(observations)
}

#' Standardize GPS Coordinates Per Location
#'
#' Some recorders report slightly different GPS coordinates for files from the
#' same physical location. This function standardizes all coordinates for a given
#' location to the first recorded value.
#'
#' @param observations Data.frame containing processed observations.
#'
#' @return Data.frame with standardized coordinates per location.
#'
#' @keywords internal
.standardize_swift_locations <- function(observations) {
  message(sprintf("Standardizing coordinates for %d recordings...", nrow(observations)))

  observations <- observations %>%
    dplyr::group_by(Location) %>%
    dplyr::mutate(
      Latitude = dplyr::first(Latitude),
      Longitude = dplyr::first(Longitude)
    ) %>%
    dplyr::ungroup()

  return(observations)
}

#' Harmonize Location Column Name Variations
#'
#' Handles different naming conventions for latitude/longitude in GUANO metadata.
#' Merges data when both variants exist (preferring capitalized versions).
#'
#' @param observations Data.frame containing GUANO metadata.
#'
#' @return Data.frame with standardized \code{Latitude} and \code{Longitude} columns.
#'
#' @keywords internal
.harmonize_location_columns <- function(observations) {
  # Handle latitude variants
  if ("Loc.Position.latitude" %in% colnames(observations)) {
    if ("Loc.Position.Lat" %in% colnames(observations)) {
      # Both exist: merge data using coalesce (prefer capitalized)
      lat_primary <- suppressWarnings(as.numeric(as.character(observations$Loc.Position.Lat)))
      lat_alt <- suppressWarnings(as.numeric(as.character(observations$Loc.Position.latitude)))
      observations$Loc.Position.Lat <- dplyr::coalesce(lat_primary, lat_alt)
    } else {
      # Only lowercase exists, rename to capitalized
      colnames(observations)[colnames(observations) == "Loc.Position.latitude"] <- "Loc.Position.Lat"
    }
    # Drop lowercase after merge/rename
    if ("Loc.Position.latitude" %in% colnames(observations)) {
      observations[["Loc.Position.latitude"]] <- NULL
    }
  }

  # Handle longitude variants
  if ("Loc.Position.longitude" %in% colnames(observations)) {
    if ("Loc.Position.Lon" %in% colnames(observations)) {
      # Both exist: merge data using coalesce (prefer capitalized)
      lon_primary <- suppressWarnings(as.numeric(as.character(observations$Loc.Position.Lon)))
      lon_alt <- suppressWarnings(as.numeric(as.character(observations$Loc.Position.longitude)))
      observations$Loc.Position.Lon <- dplyr::coalesce(lon_primary, lon_alt)
    } else {
      # Only lowercase exists, rename to capitalized
      colnames(observations)[colnames(observations) == "Loc.Position.longitude"] <- "Loc.Position.Lon"
    }
    # Drop lowercase after merge/rename
    if ("Loc.Position.longitude" %in% colnames(observations)) {
      observations[["Loc.Position.longitude"]] <- NULL
    }
  }

  # Rename to final standard names
  if ("Loc.Position.Lat" %in% colnames(observations)) {
    colnames(observations)[colnames(observations) == "Loc.Position.Lat"] <- "Latitude"
  }
  if ("Loc.Position.Lon" %in% colnames(observations)) {
    colnames(observations)[colnames(observations) == "Loc.Position.Lon"] <- "Longitude"
  }

  # Ensure numeric types for final columns
  if ("Latitude" %in% colnames(observations)) {
    observations$Latitude <- suppressWarnings(as.numeric(as.character(observations$Latitude)))
  }
  if ("Longitude" %in% colnames(observations)) {
    observations$Longitude <- suppressWarnings(as.numeric(as.character(observations$Longitude)))
  }

  return(observations)
}

#' Single-pass WAV Discovery with Modification Times
#'
#' Uses OS-native tools (find + stat on Unix/macOS, PowerShell on Windows) to
#' discover WAV files and retrieve their modification times in a single traversal.
#' This is significantly faster than separate discovery and stat operations.
#'
#' @param input_path Character. Path to the root directory to search.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{Full.Path}{Character. Absolute path to each WAV file.}
#'     \item{File.Modified}{Numeric. Unix timestamp (seconds since epoch) of last modification.}
#'   }
#'   Returns an empty data.frame (0 rows) if no files are found or system calls fail.
#'
#' @details
#' Platform-specific implementations:
#' \itemize{
#'   \item \strong{Unix/macOS}: Uses \code{find -iname '*.wav' -exec stat ...}
#'         Tries BSD format first (macOS), then GNU format (Linux).
#'   \item \strong{Windows}: Uses PowerShell \code{Get-ChildItem} with \code{LastWriteTimeUtc}.
#' }
#'
#' All searches are case-insensitive to match both .wav and .WAV extensions.
#'
#' @keywords internal
.get_file_list_singlepass <- function(input_path) {
  mk_empty <- function() data.frame(Full.Path = character(), File.Modified = numeric(), stringsAsFactors = FALSE)

  if (.Platform$OS.type == "unix") {
    root <- shQuote(input_path)
    cmd_mac <- sprintf("find %s -type f -iname '*.wav' -exec stat -f '%s' {} +", root, "%m\t%N")
    out <- tryCatch(system(cmd_mac, intern = TRUE), error = function(e) character(0))
    if (length(out) == 0) {
      cmd_lin <- sprintf("find %s -type f -iname '*.wav' -exec stat -c '%s' {} +", root, "%Y\t%n")
      out <- tryCatch(system(cmd_lin, intern = TRUE), error = function(e) character(0))
    }
    if (length(out) == 0) {
      return(mk_empty())
    }
    sp <- strsplit(out, "\t", fixed = TRUE)
    mt <- suppressWarnings(vapply(sp, function(z) as.numeric(z[1]), numeric(1)))
    fp <- vapply(sp, function(z) if (length(z) >= 2) z[2] else NA_character_, character(1))
    ok <- !is.na(mt) & !is.na(fp)
    if (!any(ok)) {
      return(mk_empty())
    }
    return(data.frame(Full.Path = fp[ok], File.Modified = mt[ok], stringsAsFactors = FALSE))
  } else if (.Platform$OS.type == "windows") {
    ps_quote <- function(x) sprintf("'%s'", gsub("'", "''", x, fixed = TRUE))
    ps <- sprintf(
      "$p = Get-ChildItem -LiteralPath %s -Recurse -File -Filter *.wav; $p | ForEach-Object { '{0}`t{1}' -f $_.FullName, ([DateTimeOffset]$_.LastWriteTimeUtc).ToUnixTimeSeconds() }",
      ps_quote(input_path)
    )
    cmd <- sprintf('powershell -NoProfile -Command "%s"', ps)
    out <- tryCatch(shell(cmd, intern = TRUE, translate = FALSE), error = function(e) character(0))
    if (length(out) == 0) {
      return(mk_empty())
    }
    sp <- strsplit(out, "\t", fixed = TRUE)
    fp <- vapply(sp, function(z) if (length(z) >= 1) z[1] else NA_character_, character(1))
    fp <- gsub("\\\\", "/", fp)
    mt <- suppressWarnings(vapply(sp, function(z) if (length(z) >= 2) as.numeric(z[2]) else NA_real_, numeric(1)))
    ok <- !is.na(mt) & !is.na(fp)
    if (!any(ok)) {
      return(mk_empty())
    }
    return(data.frame(Full.Path = fp[ok], File.Modified = mt[ok], stringsAsFactors = FALSE))
  } else {
    return(mk_empty())
  }
}

#' Get List of WAV Files with Metadata
#'
#' Discovers WAV files in a directory tree and retrieves their modification times.
#' Can operate in fast mode (system calls) or standard mode (R functions).
#'
#' @param input_path Character. Either a directory path to search (when \code{list=FALSE})
#'   or a character vector of file paths (when \code{list=TRUE}).
#' @param fast_import Logical. If TRUE, uses optimized single-pass system calls.
#'   If FALSE, uses standard R \code{list.files()} and \code{file.info()}.
#' @param list Logical. If TRUE, treats \code{input_path} as a vector of file paths
#'   rather than a directory to search.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{File.Name}{Character. Basename of each file.}
#'     \item{Full.Path}{Character. Absolute path to each file.}
#'     \item{File.Modified}{Numeric. Unix timestamp of last modification.}
#'   }
#'
#' @details
#' When \code{fast_import=TRUE}, attempts single-pass discovery. If this fails,
#' automatically falls back to standard R functions with a warning message.
#'
#' @keywords internal
.get_file_list <- function(input_path, fast_import = TRUE, list = FALSE) {
  # Identify if input_path is already a character vector of file paths and reformat if so
  wav_pattern <- "\\.wav$"
  if (list) {
    file_list_full <- as.character(input_path)
    if (length(file_list_full) == 0) {
      stop("No files provided in 'input_path' when list=TRUE.")
    }
    file_list_short <- sub(".*/", "", file_list_full)
    # Otherwise, read files from directory
  } else {
    message("Discovering WAV files...")
    start_discover <- Sys.time()

    if (fast_import == TRUE) {
      fast_df <- .get_file_list_singlepass(input_path)
      if (nrow(fast_df) > 0) {
        elapsed <- round(as.numeric(difftime(Sys.time(), start_discover, units = "secs")), 2)
        message(sprintf(
          "Found %d WAV files using fast discovery in %s seconds.",
          nrow(fast_df), elapsed
        ))
        file_list_full <- fast_df$Full.Path
        file_list_short <- sub(".*\\/", "", file_list_full)
        file_list <- data.frame(
          File.Name = file_list_short,
          Full.Path = file_list_full,
          File.Modified = fast_df$File.Modified,
          stringsAsFactors = FALSE
        )
        return(file_list)
      } else {
        message("Fast discovery returned no results. Using standard file listing...")
        file_list_full <- list.files(input_path, pattern = wav_pattern, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
        file_list_short <- sub(".*/", "", file_list_full)
      }
    } else {
      message("Using standard file listing...")
      file_list_full <- list.files(input_path, pattern = wav_pattern, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      file_list_short <- sub(".*/", "", file_list_full)
    }

    elapsed <- round(as.numeric(difftime(Sys.time(), start_discover, units = "secs")), 2)
    if (length(file_list_full) > 0) {
      message(sprintf("Found %d WAV files in %s seconds.", length(file_list_full), elapsed))
    }
  }

  # Handle case of no files found
  if (length(file_list_full) == 0) {
    stop(
      "No WAV files found in '", input_path, "'.\n",
      "Please check the path and ensure it contains .wav files."
    )
  }

  # Get file modification times (use file.info here to avoid extra system calls on fallback)
  file_modified <- as.numeric(file.info(file_list_full)$mtime)

  # Create file list data.frame
  file_list <- data.frame(
    File.Name = file_list_short,
    Full.Path = file_list_full,
    File.Modified = file_modified,
    stringsAsFactors = FALSE
  )

  return(file_list)
}

#' Read GUANO Metadata from WAV File List
#'
#' Reads GUANO metadata from a list of WAV files, processes location columns,
#' handles missing data, and prepares observations for analysis.
#'
#' @param file_list A data.frame from \code{.get_file_list()} containing file paths.
#' @param site_col Character. Name of the GUANO field containing location/site name.
#' @param timezone Character. Valid timezone string for timestamp conversion.
#' @param fast_import Logical. If TRUE, reads files in parallel using \code{future}.
#'
#' @return A data.frame containing processed observations with standardized columns:
#'   Timestamp, Species, Location, Latitude, Longitude, and all GUANO metadata fields.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Reads GUANO metadata from each file (with error handling)
#'   \item Harmonizes location column name variations
#'   \item Checks for missing critical data (location, coordinates)
#'   \item Computes night dates using noon-to-noon convention
#'   \item Standardizes Species column from manual/auto IDs
#'   \item Corrects inconsistent GPS readings for Anabat Swift devices
#' }
#'
#' @keywords internal
.read_file_guano <- function(file_list, site_col, timezone, fast_import = FALSE) {
  paths <- as.character(file_list$Full.Path)
  n_files <- length(paths)

  message(sprintf("Reading GUANO metadata from %d files...", n_files))
  read_start <- Sys.time()

  # Helper to read single file safely
  safe_read <- function(path) {
    tryCatch(
      {
        df <- read.guano(path)
        as.data.frame(df)
      },
      error = function(e) {
        list(.error = TRUE, .path = path, .message = conditionMessage(e))
      }
    )
  }

  # Read files (possibly in parallel)
  if (fast_import) {
    message("Using parallel processing...")
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession)
    res_list <- progressr::with_progress({
      p <- progressr::progressor(steps = length(paths))
      future.apply::future_lapply(paths, function(path) {
        res <- safe_read(path)
        p()
        res
      }, future.packages = "batr")
    })
  } else {
    res_list <- lapply(paths, safe_read)
  }

  read_elapsed <- round(as.numeric(difftime(Sys.time(), read_start, units = "secs")), 1)
  message(sprintf("Completed reading files in %s seconds.", read_elapsed))

  # Separate successes / failures
  failures <- vapply(res_list, function(x) is.list(x) && !is.data.frame(x), logical(1))
  if (any(failures)) {
    failed_info <- Filter(function(x) is.list(x) && !is.data.frame(x), res_list)
    n_failed <- length(failed_info)
    warning(sprintf("Failed to read %d of %d files due to errors.", n_failed, n_files),
      call. = FALSE
    )
    # Show first few failures
    if (n_failed <= 3) {
      for (fail in failed_info) {
        message("  Failed: ", basename(fail$.path), " - ", fail$.message)
      }
    } else {
      message("  First 3 failures:")
      for (i in 1:3) {
        message("    ", basename(failed_info[[i]]$.path), " - ", failed_info[[i]]$.message)
      }
      message(sprintf("    ... and %d more", n_failed - 3))
    }
  }

  # Keep only successful data.frames
  dfs <- Filter(is.data.frame, res_list)
  if (length(dfs) == 0) {
    stop(
      "No files could be read successfully.\n",
      "Please check that the files contain valid GUANO metadata."
    )
  }
  message(sprintf("Successfully read %d of %d files.", length(dfs), n_files))

  message("Combining metadata...")
  observations <- dplyr::bind_rows(dfs)

  # Harmonize location column names
  observations <- .harmonize_location_columns(observations)

  # Check for missing required data
  .missing_data_checker(observations, site_col)

  # Combine guano from files with data from file list, and remove unneeded columns
  observations <- merge(observations, file_list, by.x = "File.Name", by.y = "File.Name")
  observations <- observations[, !names(observations) %in% c("Full.Path", "Anabat.Signature")]

  # Ensure Timestamp is POSIXct and in intended timezone
  observations$Timestamp <- as.POSIXct(observations$Timestamp)
  observations$Timestamp <- lubridate::force_tz(observations$Timestamp, tzone = timezone)

  # Compute Night: recordings after noon (11:59:59) belong to that night,
  # recordings before noon belong to the previous night. This convention
  # matches standard bat monitoring practice where a "night" spans from
  # noon-to-noon rather than midnight-to-midnight.
  local_ts <- lubridate::with_tz(observations$Timestamp, tzone = timezone)
  local_date <- as.Date(local_ts)
  observations$Night <- ifelse(lubridate::hour(local_ts) > 11, local_date, local_date - 1)
  observations$Night <- as.Date(observations$Night, origin = "1970-01-01")

  # Generate Species column
  observations <- .create_species_column(observations)

  colnames(observations)[colnames(observations) == site_col] <- "Location"
  observations <- observations[!is.na(observations$Location), ]

  # For Anabat Swift files: standardize lat/lon to first value per location
  observations <- .standardize_swift_locations(observations)
  observations <- dplyr::select(
    observations,
    Timestamp,
    Species,
    Location,
    Latitude,
    Longitude,
    Species.Auto.ID,
    dplyr::any_of("Species.Manual.ID"),
    dplyr::everything()
  )

  return(observations)
}

#' Check for Missing Required Data
#'
#' Identifies files with missing required GUANO fields and stops with
#' an error describing the missing data.
#'
#' @param observations Data.frame containing observations with File.Name column.
#' @param site_col Character. Name of the column containing location/site names.
#'
#' @return Data.frame listing files with missing data and which fields are missing.
#'   Returns empty data.frame (0 rows) if all files have complete data.
#'
#' @details
#' Required fields are: the location column (\code{site_col}), \code{Latitude},
#' \code{Longitude}, and \code{Timestamp}. Missing required fields cause
#' the import to stop with an error. Species identifiers are optional; if both
#' \code{Species.Manual.ID} and \code{Species.Auto.ID} are missing, a warning
#' is issued but import continues.
#'
#' @keywords internal
.missing_data_checker <- function(observations, site_col) {
  missing_data <- data.frame(
    File.Name = character(),
    Missing.Fields = character(),
    stringsAsFactors = FALSE
  )

  required_cols <- c(site_col, "Latitude", "Longitude", "Timestamp")
  missing_cols <- setdiff(required_cols, names(observations))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required GUANO columns: ", paste(missing_cols, collapse = ", "), ".\n",
      "Required columns: ", paste(required_cols, collapse = ", "), "."
    )
  }

  missing_loc_idx <- is.na(observations[[site_col]]) |
    (is.character(observations[[site_col]]) & trimws(observations[[site_col]]) == "")
  missing_lat_idx <- is.na(observations$Latitude)
  missing_lon_idx <- is.na(observations$Longitude)
  missing_ts_idx <- is.na(observations$Timestamp) |
    (is.character(observations$Timestamp) & trimws(observations$Timestamp) == "")

  missing_files <- unique(c(
    observations$File.Name[missing_loc_idx],
    observations$File.Name[missing_lat_idx],
    observations$File.Name[missing_lon_idx],
    observations$File.Name[missing_ts_idx]
  ))

  if (length(missing_files) > 0) {
    missing_data <- data.frame(
      File.Name = missing_files,
      Missing.Fields = sapply(missing_files, function(fn) {
        missing_fields <- c()
        if (fn %in% observations$File.Name[missing_loc_idx]) {
          missing_fields <- c(missing_fields, "Location")
        }
        if (fn %in% observations$File.Name[missing_lat_idx]) {
          missing_fields <- c(missing_fields, "Latitude")
        }
        if (fn %in% observations$File.Name[missing_lon_idx]) {
          missing_fields <- c(missing_fields, "Longitude")
        }
        if (fn %in% observations$File.Name[missing_ts_idx]) {
          missing_fields <- c(missing_fields, "Timestamp")
        }
        paste(missing_fields, collapse = ", ")
      }, USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )

    preview <- missing_data[seq_len(min(10, nrow(missing_data))), , drop = FALSE]
    preview_lines <- paste0(preview$File.Name, " [", preview$Missing.Fields, "]")
    more_count <- nrow(missing_data) - nrow(preview)
    more_text <- if (more_count > 0) paste0(" (", more_count, " more)") else ""

    stop(
      "Missing required GUANO fields in ", nrow(missing_data), " file(s).\n",
      "Required fields: ", paste(required_cols, collapse = ", "), ".\n",
      "Examples: ", paste(preview_lines, collapse = "; "), more_text
    )
  }

  has_manual <- "Species.Manual.ID" %in% names(observations)
  has_auto <- "Species.Auto.ID" %in% names(observations)

  if (!has_manual && !has_auto) {
    warning(
      "No species ID columns found (Species.Manual.ID or Species.Auto.ID). ",
      "Species will be NA for all observations. ",
      "Species-dependent summaries, plots, and reports may be empty or fail."
    )
    return(missing_data)
  }

  manual_missing <- if (has_manual) {
    is.na(observations$Species.Manual.ID) |
      (is.character(observations$Species.Manual.ID) & trimws(observations$Species.Manual.ID) == "")
  } else {
    rep(TRUE, nrow(observations))
  }
  auto_missing <- if (has_auto) {
    is.na(observations$Species.Auto.ID) |
      (is.character(observations$Species.Auto.ID) & trimws(observations$Species.Auto.ID) == "")
  } else {
    rep(TRUE, nrow(observations))
  }
  missing_species_idx <- manual_missing & auto_missing
  if (any(missing_species_idx)) {
    preview <- observations$File.Name[missing_species_idx]
    preview <- preview[seq_len(min(10, length(preview)))]
    more_count <- sum(missing_species_idx) - length(preview)
    more_text <- if (more_count > 0) paste0(" (", more_count, " more)") else ""
    warning(
      sum(missing_species_idx), " file(s) are missing both Species.Manual.ID and Species.Auto.ID. ",
      "Species will be NA for these files. Species-dependent outputs may be sparse or empty if this is widespread. ",
      "Examples: ", paste(preview, collapse = ", "), more_text
    )
  }

  return(missing_data)
}
