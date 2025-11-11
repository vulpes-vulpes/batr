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
  observations <- suppressWarnings(.read_file_guano(file_list, site_col, timezone, fast_import))
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
  observations_new <- suppressWarnings(.read_file_guano(files_to_import, site_col, timezone, fast_import))

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
  observations_update <- suppressWarnings(.read_file_guano(update_list, site_col, timezone, fast_import))

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
  if (fast_import == TRUE) {
    message("Fast import selected: setting up multisession.")
    future::plan(future::multisession)
    message("Reading WAV files, please wait...")
    observations <- do.call(
      plyr::rbind.fill,
      (future.apply::future_lapply(future.apply::future_lapply(
        as.list(file_list$Full.Path),
        read.guano
      ), as.data.frame))
    )
  } else {
    message("Reading WAV files, please wait...")
    observations <- do.call(
      plyr::rbind.fill,
      (lapply(pbapply::pblapply(as.list(file_list$Full.Path), read.guano), as.data.frame))
    )
  }
  .missing_data_checker(observations, site_col)
  observations <- merge(observations, file_list, by.x = "File.Name", by.y = "File.Name")
  observations <- observations[, !names(observations) %in% c("Full.Path", "Anabat.Signature")]
  observations <- within(observations, {
    Night <- ifelse(lubridate::hour(observations$Timestamp) > 11, # nolint: object_name_linter.
      as.Date(format(as.POSIXct(observations$Timestamp), "%Y-%m-%d"), tz = "EST"),
      as.Date(format(as.POSIXct(observations$Timestamp), "%Y-%m-%d"), tz = "EST") - 1
    )
  }) # Generate Night column, accounting for observations after midnight
  observations$Night <- as.Date(observations$Night, origin = "1970-01-01") # Set format of date column
  observations$Timestamp <- lubridate::force_tz(observations$Timestamp, timezone)
  observations <- within(observations, {
    Species <- ifelse(is.na(observations$Species.Manual.ID), # nolint: object_name_linter.
      as.character(observations$Species.Auto.ID),
      as.character(observations$Species.Manual.ID)
    )
  }) # Generate Species column
  # observations <- observations[observations$Species %in% species_list, ] # Remove rows without species in species list
  colnames(observations)[colnames(observations) == "Loc.Position.Lat"] <- "Latitude"
  colnames(observations)[colnames(observations) == "Loc.Position.Lon"] <- "Longitude"
  colnames(observations)[colnames(observations) == site_col] <- "Location" # Set location column
  observations <- observations[!is.na(observations$Location), ]
  swift_files <- observations[observations$Model == "Swift", ] # Subset files recorded with Anabat Swift
  location_list <- unique(swift_files$Location) # Find locations
  for (Location in location_list) {
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
  if ((site_col %in% colnames(observations)) == FALSE) {
    stop("Location data (i.e. your specified site_col) is missing from all files. Please check and try again.")
  } else if (sum(is.na(observations[[site_col]])) != 0) {
    message("Error: Some files are missing Location (site_col) data.
      Would you like to see a list of files with missing data?")
    report <- readline(prompt = "y/n:")
    if (report == "y" || report == "Y") {
      missing_files <- observations[is.na(observations[[site_col]]), ]
      missing_files <- missing_files$File.Name
      message("Ending due to missing Location (site_col) data. No files have been imported")
      assign("files_missing_locations", as.data.frame(missing_files), envir = globalenv())
    }
  } else {
    message("Location data present, proceeding.")
  } # Check if location column exists and exits if not

  # if("Species.Auto.ID" %in% colnames(observations) && sum(is.na(observations$Species.Auto.ID)) == 0) {
  #  message("Full Species Auto ID data present, proceeding.")
  # } else if("Species.Manual.ID" %in% colnames(observations) && sum(is.na(observations$Species.Manual.ID)) == 0) {
  #  message("Full Manual Auto ID data present, proceeding.")
  # } else {
  #  stop("Incomplete Species ID data found, please check and try again.")
  # } # Check is species ID data (either automated or manual) exists and has no NAs, and exits if not

  if (("Loc.Position.Lat" %in% colnames(observations)) == FALSE) {
    stop("Lat Lon data is missing from all files. Please check and try again.")
  } else if (sum(is.na(observations$Loc.Position.Lat)) != 0) {
    message("Error: Some files are missing Lat Lon data.
      Would you like to see a list of files with missing data?")
    report <- readline(prompt = "y/n:")
    if (report == "y" || report == "Y") {
      missing_files <- observations[is.na(observations$Loc.Position.Lat), ]
      missing_files <- missing_files$File.Name
      message("Ending due to missing Lat Lon data. No files have been imported")
      assign("files_missing_latlon", as.data.frame(missing_files), envir = globalenv())
    }
  } else {
    message("Lat Lon data present, proceeding.")
  }
} # Check is species ID column exists and has no NAs, and exits if not
