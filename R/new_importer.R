#' Read GUANO Metadata From WAV Files
#'
#' \code{import_GUANO} reads GUANO metadata stored in WAV files generated
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
#' \dontrun{#' import_GUANO("New", "C:/Folder/Folder/WAVs_Folder", "Location", "C:/Folder/Folder/Data.RData")}
#' @export
import_GUANO <- function(action, input_path, site_col, timezone, data_path = NULL, fast_import = F) {

  if (action == "New" | action == "Add" | action == "Update") {
    data_path <- .check_data_path(data_path, action)
  } # Check that a valid action is selected
  if (action == "New") {
    .new_observations(input_path, site_col, timezone, data_path, fast_import)
  } else if (action == "Add") {
    .add_observations(input_path, site_col, timezone, data_path, fast_import)
  } else if (action == "Update") {
    .update_observations(input_path, site_col, timezone, data_path, fast_import)
  } else {
    stop("Invalid action given, please specifiy an action (\"New\", \"Add\" or \"Update\") and try again. See ?read_wavs for more information")
  }
}

.new_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  file_list <- .get_file_list(input_path, fast_import) # Get list of files
  observations <- suppressWarnings(.read_file_GUANO(file_list, site_col, timezone, fast_import)) # Read GUANO
  .save_to_RDATA(observations, data_path)
}

.add_observations <- function(input_path, site_col, timezone, data_path, fast_import) {
  load(data_path) # Load current data
  observations_original <- observations # Rename current observations for clarity
  rm(observations) # Remove current observations df for clarity
  observations_new <- .get_file_list(input_path, fast_import) # Get list observations in input director
  if (sum(observations_new$File.Name %in% observations_original$File.Name, na.rm = TRUE) == length(observations_new$File.Name)) { 
    stop("All of the files you are trying to add already exist in the data file. No files will be imported, please use the Update action of import_GUANO to update existing files.")
  } else if (sum(observations_new$File.Name %in% observations_original$File.Name, na.rm = TRUE) != 0) {
    warning("Some of the data you are trying to add already exists in the data file. Only files that do not currently exist will be imported. Please use the Update action of import_GUANO to update existing files")
  } # Check whether the new files already exist in the data file, and warn / exit if they do. 
  observations_new <- observations_new[!(observations_new$File.Name %in% observations_original$File.Name),] # Remove directory observations already present in current data
  observations_new <-suppressWarnings(.read_file_GUANO(observations_new, site_col, timezone, fast_import = F)) # Read GUANO for new files
  #observations_new <- merge(observations_new2, observations_new, by.x = "File.Name", by.y = "File.Name") # Combine GUANO and file metadata
  #observations_new <- observations_new[, which(names(observations_new) != "Full.Path")] # Remove duplicate column
  observations_original[setdiff(names(observations_new), names(observations_original))] <- NA # Prepare for column differences in tables
  observations_new[setdiff(names(observations_original), names(observations_new))] <- NA
  observations <- rbind(observations_original, observations_new) # Bind original and new data
  observations <- observations[order(observations$File.Name),] # Sort
  rownames(observations) <- NULL # Reset row names
  .save_to_RDATA(observations, data_path)
}

.update_observations <- function(input_path, site_col, data_path, fast_import = F) {
  load(data_path) # Load existing data
  observations_files <- observations[, c("File.Name", "File.Path", "File.Modified")] # Trim existing data to relevant columns
  rm(observations) # Remove current observations df for clarity
  names(observations_files)[1:3] <- c("File.Name","Observations.Path", "Observations.Modified") # Rename existing data columns for clarify
  directory_files <- .get_file_list(input_path, fast_import = F) # Get input file list
  files_comparison <- merge(directory_files, observations_files, by = "File.Name", all = T) # Merge new and existing data frames by file name
  files_comparison <- files_comparison[!is.na(files_comparison$Full.Path),]
  files_comparison$update <- ifelse(mapply(function(x, y) {isTRUE(all.equal(x, y))}, files_comparison$File.Modified, files_comparison$Observations.Modified), "N", 
                                    ifelse(files_comparison$File.Modified > files_comparison$Observations.Modified, "Y", "N")) # Mark files where modified dates don't match, and input file version is newer
  update_files <- subset(files_comparison, update == "Y") # Subset files to update
  if (length(update_files$Full.Path) == 0) {
    stop("No files to update!")
  }
  update_files <- update_files$Full.Path
  update_files <- .get_file_list(update_files, list = T)
  observations_update <- suppressWarnings(.read_file_GUANO(update_files, site_col, timezone, fast_import = F))
  observations_original <- observations[!(observations$File.Name %in% observations_update$File.Name),] # Remove updated rows from original data
  observations_files[setdiff(names(observations_update), names(observations_original))] <- NA # Account for differences in columns
  observations_update[setdiff(names(observations_original), names(observations_update))] <- NA # ^
  observations <- rbind(observations_original, observations_update) # Bind new and updated rows
  observations <- observations[order(observations$File.Name),] # Reorder by filename
  rownames(observations) <- NULL # Reset row names
  .save_to_RDATA(observations, data_path)
}  

.get_file_list <- function(input_path, fast_import = F, list = F) {
  if (list == F) {
    message("Making list of available WAV files.")
    if (fast_import == T) {
      message("Using fast file reading!")
      if (.Platform$OS.type == "unix") {
        file_list_full <- system(sprintf('find "%s" -name "*.wav"', input_path), intern=T) # Faster system call to get file list
      } else if (.Platform$OS.type == "windows") {
        file_list_full <- shell(sprintf('dir /s /b "%s\\*.wav"', input_path), intern=T) # System call for Windows environments
        file_list_full <- gsub("\\\\", "/", file_list_full)
      }
    file_list_short <- sub('.*\\/', '', file_list_full)
    } else {
      message("Using slow file reading :(")
      file_list_full <- list.files(input_path, full.names = TRUE, recursive = TRUE, pattern = ".wav")
      file_list_short <- list.files(input_path, full.names = FALSE, recursive = TRUE, pattern = ".wav")
    }
  } else {
    file_list <- input_path
    message("WTF!!!")
  }
  message("Adding file modified times to list.")
  file_list <- cbind(file=file_list_short,
                     dir=file_list_full,
                     data.frame(file.info(file_list_full), row.names =NULL),
                     stringsAsFactors=FALSE)
  file_list$filename <- sub('.*\\/', '', file_list$file)
  file_list <- file_list[, c("filename", 'file', 'dir', "mtime")]
  names(file_list)[1:4] <- c("File.Name", "Local.Path", "Full.Path", "File.Modified")
  file_list$File.Modified <- as.numeric(file_list$File.Modified)
  message("File list complete, moving to next step.")
  return(file_list)
}

.read_file_GUANO <- function(file_list, site_col, timezone, fast_import = F) {
  if (fast_import == T) {
    message("Fast import selected: setting up multisession.")
    future::plan(future::multisession)
    message("Reading WAV files, please wait...")
    observations <- do.call(plyr::rbind.fill, (future.apply::future_lapply(future.apply::future_lapply(as.list(file_list$Full.Path), read.guano), as.data.frame))) # Read GUANO
  } else {
    message("Reading WAV files, please wait...")
    observations <- do.call(plyr::rbind.fill, (lapply(pbapply::pblapply(as.list(file_list$Full.Path), read.guano), as.data.frame))) # Read GUANO
  }
  .missing_data_checker(observations, site_col) # Call missing data checker to exit and inform user if required columns are missing or incomplete
  observations <- merge(observations, file_list, by.x = "File.Name", by.y = "File.Name") # Add file modified columns to GUNAO df
  observations <- observations[ , !names(observations) %in% c("Full.Path","Anabat.Signature")] # Remove undesired columns
  observations <- within(observations, {
    Night = ifelse(lubridate::hour(observations$Timestamp) > 11, 
                   as.Date(format(as.POSIXct(observations$Timestamp), "%Y-%m-%d"), tz = "EST" ), 
                   as.Date(format(as.POSIXct(observations$Timestamp), "%Y-%m-%d"), tz = "EST" ) - 1)
  }) # Generate Night column, accounting for observations after midnight
  observations$Night <- as.Date(observations$Night, origin = "1970-01-01") # Set format of date column
  observations$Timestamp <- lubridate::force_tz(observations$Timestamp, timezone)
  observations = within(observations, {
    Species = ifelse(is.na(observations$Species.Manual.ID), as.character(observations$Species.Auto.ID), as.character(observations$Species.Manual.ID))
  }) # Generate Species column
  # observations <- observations[observations$Species %in% species_list, ] # Remove rows without species in species list
  colnames(observations)[colnames(observations)=="Loc.Position.Lat"] <- "Latitude"
  colnames(observations)[colnames(observations)=="Loc.Position.Lon"] <- "Longitude"
  colnames(observations)[colnames(observations)==site_col] <- "Location" # Set location column
  observations <- observations[!is.na(observations$Location),]
  swift_files <- observations[observations$Model == 'Swift',] # Subset files recorded with Anabat Swift
  location_list <- unique(swift_files$Location) # Find locations
  for (Location in location_list) {
    location_subset           <- swift_files[swift_files$Location == Location,] # Subset once more by location
    location_subset$Latitude  <- location_subset[1,"Latitude"] # Replace all lat values with first instance
    location_subset$Longitude <- location_subset[1,"Longitude"] # Replace all lon values with first instance
    if (!exists("swift_files_mod")) {
      swift_files_mod <- location_subset
    } else {
      swift_files_mod <- rbind(swift_files_mod, location_subset)
    } # Create modified observations.frame if it doesn't exist, or apend if it does
  }
  observations <- observations[!(observations$Model == 'Swift'),] # Remove modified rows
  if (exists("swift_files_mod")) {
    observations <- rbind(observations, swift_files_mod) # Replace removed rows with modified rows
  }
  rm(swift_files_mod, swift_files, location_list, location, location_subset)
  observations <- dplyr::select(observations, Timestamp, Species, Location, Latitude, Longitude, Species.Auto.ID, Species.Manual.ID, Night, File.Name, everything())
  return(observations)
}

.missing_data_checker <- function(observations, site_col) {
  if((site_col %in% colnames(observations)) == FALSE) {
    stop("Location data (i.e. your specified site_col) is missing from all files. Please check and try again.")
  } else if (sum(is.na(observations[[site_col]])) != 0) {
    message("Error: Some files are missing Location (site_col) data. 
      Would you like to see a list of files with missing data?")
    report <- readline(prompt="y/n:")
    if (report == "y" | report == "Y") {
      missing_files <- observations[is.na(observations[[site_col]]),]
      missing_files <- missing_files$File.Name
      message("Ending due to missing Location (site_col) data. No files have been imported")
      assign("files_missing_locations", as.data.frame(missing_files), envir=globalenv())
    }
  } else {
    message("Location data present, proceeding.")
  }  # Check if location column exists and exits if not
  
  #if("Species.Auto.ID" %in% colnames(observations) && sum(is.na(observations$Species.Auto.ID)) == 0) {
  #  message("Full Species Auto ID data present, proceeding.")
  #} else if("Species.Manual.ID" %in% colnames(observations) && sum(is.na(observations$Species.Manual.ID)) == 0) {
  #  message("Full Manual Auto ID data present, proceeding.")
  #} else {
  #  stop("Incomplete Species ID data found, please check and try again.")
  #} # Check is species ID data (either automated or manual) exists and has no NAs, and exits if not
  
  if(("Loc.Position.Lat" %in% colnames(observations)) == FALSE) {
    stop("Lat Lon data is missing from all files. Please check and try again.")
  } else if (sum(is.na(observations$Loc.Position.Lat)) != 0) {
      message("Error: Some files are missing Lat Lon data. 
      Would you like to see a list of files with missing data?")
      report <- readline(prompt="y/n:")
      if (report == "y" | report == "Y") {
        missing_files <- observations[is.na(observations$Loc.Position.Lat),]
        missing_files <- missing_files$File.Name
        message("Ending due to missing Lat Lon data. No files have been imported")
        assign("files_missing_latlon", as.data.frame(missing_files), envir=globalenv())
      }
  } else {
    message("Lat Lon data present, proceeding.")
    }
  } # Check is species ID column exists and has no NAs, and exits if not