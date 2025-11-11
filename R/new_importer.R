#' Read GUANO Metadata From WAV Files
#'
#' \code{import_guano} reads GUANO metadata stored in WAV files generated
#' through bat acoustic monitoring and saves them as a data.frame in an RData
#' file for easy reference. This function can generate a new RData file, add
#' GUANO from new files to an existing RData file, or update metadata in an
#' existing RData file where the metadata of the original files has changed.
#'
#' @section Note: This function will take a long time for large data sets.
#'   Folders containing tens of thousands of files may take several hours to
#'   read! For this reason it is recommended to complete all manual vetting
#'   before proceeding. However, files with changed metadata can be updated later
#'   using the "Update" action.
#'
#' @family Import Functions
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
#' @param fast_import Option to increase import speed (experimental).
#'
#' @return An RData file saved in a specified location containing GUANO metadata
#'   read from WAV files created through bat acoustic monitoring.
#'
#' @family import tools
#'
#' @examples
#' \dontrun{
#' #' import_guano("New", "C:/Folder/Folder/WAVs_Folder", "Location", "C:/Folder/Folder/Data.RData")
#' }
#' @export
import_guano <- function(action, input_path, site_col, timezone, data_path = NULL, fast_import = TRUE) {
  if (action == "New" || action == "Add" || action == "Update") {
    data_path <- .check_data_path(data_path, action, object = action)
  } # Check that a valid action is selected
  if (action == "New") {
    .new_observations(input_path, site_col, timezone, data_path, fast_import)
  } else if (action == "Add") {
    .add_observations(input_path, site_col, timezone, data_path, fast_import)
  } else if (action == "Update") {
    .update_observations(input_path, site_col, timezone, data_path, fast_import)
  } else {
    stop(
      "Invalid action given, please specifiy an action (\"New\", \"Add\" or \"Update\") and try again. ",
      "See ?read_wavs for more information"
    )
  }
}

.new_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  file_list <- .get_file_list(input_path, fast_import)
  observations <- .read_file_guano(file_list, site_col, timezone, fast_import)
  .save_to_RDATA(observations, data_path)
}

.add_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  # Load existing data into local env to avoid polluting global env
  env <- new.env()
  load(data_path, envir = env)
  if (!exists("observations", envir = env)) {
    stop("Loaded RData does not contain an object named 'observations'")
  }
  original_observations <- env$observations

  # Get candidate files (file list only)
  candidate_files <- .get_file_list(input_path, fast_import)
  # Determine which files are truly new
  is_existing <- candidate_files$File.Name %in% original_observations$File.Name
  if (all(is_existing)) {
    stop("All of the files you are trying to add already exist in the data file. No files will be imported. Please use the Update action.")
  }
  if (any(is_existing)) {
    warning("Some files exist already; only new files will be imported.")
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
  .save_to_RDATA(observations, data_path)
}

.update_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  # Load existing data into local env to avoid polluting global env
  env <- new.env()
  load(data_path, envir = env)
  if (!exists("observations", envir = env)) stop("RData does not contain 'observations'.")
  observations_all <- env$observations

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
  .save_to_RDATA(observations, data_path)
}

.get_file_list <- function(input_path, fast_import = TRUE, list = FALSE) {
  # Identify if input_path is already a character vector of file paths and reformat if so
  if (list) {
    file_list_full <- as.character(input_path)
    if (length(file_list_full) == 0) stop("No files provided in 'input_path' when list=TRUE")
    file_list_short <- sub(".*/", "", file_list_full)
    # Otherwise, read files from directory
  } else {
    message("Making list of available WAV files.")
    if (fast_import == TRUE) {
      message("Using fast file reading!")
      if (.Platform$OS.type == "unix") {
        file_list_full <- system(sprintf('find "%s" -name "*.wav"', input_path), intern = TRUE) # Unix system call
      } else if (.Platform$OS.type == "windows") {
        file_list_full <- shell(sprintf('dir /s /b "%s\\*.wav"', input_path), intern = TRUE) # Windows system call
        file_list_full <- gsub("\\\\", "/", file_list_full)
      }
      file_list_short <- sub(".*\\/", "", file_list_full)
    } else {
      message("Using slow file reading :(")
      file_list_full <- list.files(input_path, pattern = wav_pattern, full.names = TRUE, recursive = TRUE, perl = TRUE)
      file_list_short <- sub(".*/", "", file_list_full)
    }
  }

  # Handle case of no files found
  if (length(file_list_full) == 0) {
    message("No WAV files found.")
    return(data.frame(File.Name = character(), Local.Path = character(), Full.Path = character(), File.Modified = numeric(), stringsAsFactors = FALSE))
  }

  # Create file list data.frame
  fileinfo <- file.info(file_list_full)
  file_list <- data.frame(
    File.Name = file_list_short,
    Full.Path = file_list_full,
    File.Modified = as.numeric(fileinfo$mtime),
    stringsAsFactors = FALSE
  )

  # Return file list
  message("File list complete.")
  return(file_list)
}

.read_file_guano <- function(file_list, site_col, timezone, fast_import = FALSE) {
  paths <- as.character(file_list$Full.Path)
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
    message("Fast import selected: setting up multisession.")
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
    message("Reading WAV files, please wait...")
    res_list <- lapply(paths, safe_read)
  }

  # Separate successes / failures
  failures <- vapply(res_list, function(x) is.list(x) && !is.data.frame(x), logical(1))
  if (any(failures)) {
    failed_info <- Filter(function(x) is.list(x) && !is.data.frame(x), res_list)
    warning(sprintf("Failed to read %d files. See returned 'read_failures' attribute.", length(failed_info)))
    attr(res_list, "read_failures") <- failed_info
  }

  # Keep only successful data.frames
  dfs <- Filter(is.data.frame, res_list)
  if (length(dfs) == 0) stop("No files could be read successfully.")
  observations <- do.call(plyr::rbind.fill, dfs)

  # Harmonize location column names: handle both lowercase and capitalized variants
  # If both variants exist, merge data (prefer capitalized, fill with lowercase where capitalized is NA)
  if ("Loc.Position.latitude" %in% colnames(observations)) {
    if ("Loc.Position.Lat" %in% colnames(observations)) {
      # Both exist: merge data (prefer capitalized, use lowercase to fill NAs)
      observations$Loc.Position.Lat <- ifelse(
        is.na(observations$Loc.Position.Lat),
        observations$Loc.Position.latitude,
        observations$Loc.Position.Lat
      )
    } else {
      # Only lowercase exists, rename to capitalized
      colnames(observations)[colnames(observations) == "Loc.Position.latitude"] <- "Loc.Position.Lat"
    }
    # Drop lowercase after merge/rename
    if ("Loc.Position.latitude" %in% colnames(observations)) {
      observations[["Loc.Position.latitude"]] <- NULL
    }
  }
  if ("Loc.Position.longitude" %in% colnames(observations)) {
    if ("Loc.Position.Lon" %in% colnames(observations)) {
      # Both exist: merge data (prefer capitalized, use lowercase to fill NAs)
      observations$Loc.Position.Lon <- ifelse(
        is.na(observations$Loc.Position.Lon),
        observations$Loc.Position.longitude,
        observations$Loc.Position.Lon
      )
    } else {
      # Only lowercase exists, rename to capitalized
      colnames(observations)[colnames(observations) == "Loc.Position.longitude"] <- "Loc.Position.Lon"
    }
    # Drop lowercase after merge/rename
    if ("Loc.Position.longitude" %in% colnames(observations)) {
      observations[["Loc.Position.longitude"]] <- NULL
    }
  }
  # Finally, rename capitalized variants to final names
  if ("Loc.Position.Lat" %in% colnames(observations)) {
    colnames(observations)[colnames(observations) == "Loc.Position.Lat"] <- "Latitude"
  }
  if ("Loc.Position.Lon" %in% colnames(observations)) {
    colnames(observations)[colnames(observations) == "Loc.Position.Lon"] <- "Longitude"
  }

  # Check for missing critical data and get list of files to exclude
  missing_data <- .missing_data_checker(observations, site_col)

  # Filter out files with missing data
  if (nrow(missing_data) > 0) {
    observations <- observations[!(observations$File.Name %in% missing_data$File.Name), ]
    if (nrow(observations) == 0) {
      stop("All files have missing critical data. No files to process.")
    }
  }

  # Combine guano from files with data from file list, and remove unneeded columns
  observations <- merge(observations, file_list, by.x = "File.Name", by.y = "File.Name")
  observations <- observations[, !names(observations) %in% c("Full.Path", "Anabat.Signature")]

  # Ensure Timestamp is POSIXct and in intended timezone
  observations$Timestamp <- as.POSIXct(observations$Timestamp)
  observations$Timestamp <- lubridate::force_tz(observations$Timestamp, tzone = timezone)

  # Compute Night: if local hour > 11 then use that date, else date - 1
  local_ts <- lubridate::with_tz(observations$Timestamp, tzone = timezone)
  local_date <- as.Date(local_ts)
  observations$Night <- ifelse(lubridate::hour(local_ts) > 11, local_date, local_date - 1)
  observations$Night <- as.Date(observations$Night, origin = "1970-01-01")
  observations <- within(observations, {
    Species <- ifelse(is.na(observations$Species.Manual.ID), # nolint: object_name_linter.
      as.character(observations$Species.Auto.ID),
      as.character(observations$Species.Manual.ID)
    )
  }) # Generate Species column
  # observations <- observations[observations$Species %in% species_list, ] # Remove rows without species in species list

  colnames(observations)[colnames(observations) == site_col] <- "Location" # Set location column
  observations <- observations[!is.na(observations$Location), ]
  swift_files <- observations[observations$Model == "Swift", ] # Subset files recorded with Anabat Swift
  location_list <- unique(swift_files$Location) # Find locations
  for (location in location_list) {
    location_subset <- swift_files[swift_files$Location == Location, ] # Subset once more by location
    location_subset$Latitude <- location_subset[1, "Latitude"] # Replace all lat values with first instance
    location_subset$Longitude <- location_subset[1, "Longitude"] # Replace all lon values with first instance
    if (!exists("swift_files_mod")) {
      swift_files_mod <- location_subset
    } else {
      swift_files_mod <- rbind(swift_files_mod, location_subset)
    } # Create modified observations.frame if it doesn't exist, or apend if it does
  }
  observations <- observations[!(observations$Model == "Swift"), ] # Remove modified rows
  if (exists("swift_files_mod")) {
    observations <- rbind(observations, swift_files_mod) # Replace removed rows with modified rows
  }
  rm(swift_files_mod, swift_files, location_list, location, location_subset)
  observations <- dplyr::select(
    observations,
    Timestamp,
    Species,
    Location,
    Latitude,
    Longitude,
    Species.Auto.ID,
    dplyr::everything()
  )
  if ("Species.Manual.ID" %in% colnames(observations)) {
    observations <- dplyr::select(observations, Species.Manual.ID, dplyr::everything())
  }
  return(observations)
}

.missing_data_checker <- function(observations, site_col) {
  missing_data <- data.frame(
    File.Name = character(),
    Missing.Fields = character(),
    stringsAsFactors = FALSE
  )

  # Check for site_col presence in all observations
  if (!(site_col %in% names(observations))) {
    warning(sprintf("Location column '%s' not found in metadata. All files flagged as missing location data.", site_col))
    missing_data <- data.frame(
      File.Name = observations$File.Name,
      Missing.Fields = "Location",
      stringsAsFactors = FALSE
    )
    # Immediately return with all files as missing
    return(missing_data)
  }

  # Check for missing location data
  missing_loc_idx <- is.na(observations[[site_col]])
  missing_lat_idx <- is.na(observations$Latitude)
  missing_lon_idx <- is.na(observations$Longitude)

  # Identify files with any missing critical data
  missing_files <- unique(c(
    observations$File.Name[missing_loc_idx],
    observations$File.Name[missing_lat_idx],
    observations$File.Name[missing_lon_idx]
  ))

  # If there are missing files, build the data frame and ask user to proceed
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
        paste(missing_fields, collapse = ", ")
      }, USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )

    # Display warning and ask user
    cat("WARNING: ", nrow(missing_data), " files have missing critical data (Location, Latitude, or Longitude).\n", sep = "")
    cat("These files will be excluded from further processing.\n\n")

    # Show first N files (to avoid flooding console), then summary
    max_show <- 10
    if (nrow(missing_data) > max_show) {
      cat("Showing first ", max_show, " files with missing critical data:\n", sep = "")
      print(missing_data[1:max_show, ])
      cat("\n... and ", nrow(missing_data) - max_show, " more files.\n", sep = "")
    } else {
      cat("Files with missing critical data:\n")
      print(missing_data)
    }
    cat("\n")

    # Prompt user for confirmation (handle both interactive and non-interactive contexts)
    if (interactive()) {
      repeat {
        response <- readline(prompt = "Do you want to (p)rint all files, (y)es proceed, or (n)o cancel? [p/y/n]: ")
        response <- tolower(trimws(response))

        if (response %in% c("p", "print")) {
          # User wants to see all missing files
          cat("\n")
          cat("All files with missing critical data:\n")
          print(missing_data)
          cat("\n")
          # Loop back to prompt again
        } else if (response %in% c("y", "yes")) {
          # User confirms; return the missing data frame so caller can filter it out
          return(missing_data)
        } else if (response %in% c("n", "no")) {
          # User cancels; stop execution
          stop("Import cancelled by user due to missing critical data.")
        } else {
          cat("Please enter 'p', 'y', or 'n'.\n")
          # Loop back to prompt again
        }
      }
    } else {
      # Non-interactive mode: automatically exclude and proceed with warning
      cat("WARNING: Running in non-interactive mode. Automatically excluding files with missing critical data.\n")
      cat("(To view all missing files, check the 'missing_data' attribute on the result.)\n")
      return(missing_data)
    }
  }

  # No missing data; return empty data frame
  return(missing_data)
}
