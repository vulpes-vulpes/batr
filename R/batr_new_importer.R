#'Read GUANO Metadata From Files
#'
#'\code{GUANO_reader} reads available GUANO metadata embedded in a folder of bat
#'acoustic. wav files processed by SonoBat. It creates a text file output of
#'this metadata in the parent folder of the files that can be loaded for further
#'analyses by \code{GUANO_loader}.
#'
#'@section Note: This function will take a long time for large datasets. Folders
#'  containing tens of thousands of files may take several hours to read!
#'  However, once complete this action does not need to repeated unless any
#'  changes are made to the original files. For this reason it is recommended to
#'  complete all manual vetting before proceeding.
#'
#'@family Import Functions
#'
#'@param folderpath Character, a path to the folder containing the files you
#'  wish to read the metadata from.
#'@param project_name Character, a brief and relevant reference phrase that will be used
#'  to name the text file.
#'
#'@return A textfile in the parent folder of the files folder.
#'
#'@examples
#'\dontrun{
#' GUANO.reader("C:/Folder/Folder/File_Folder", "Project_NA")
#'}
#'@export
read_wavs <- function(action, input_path, site_col, data_path = NULL) {

  if (action == "New" | action == "Add" | action == "Update") {
    data_path <- .check_data_path(data_path, action)
  }
  if (action == "New") {
      .new_observations(input_path, site_col, data_path)
  } else if (action == "Add") {
    .add_observations(input_path, site_col, data_path)
  } else if (action == "Update") {
    .update_observations(input_path, site_col, data_path)
  } else {
    stop("Invalid action given, please specifiy an action (\"New\", \"Add\" or \"Update\") and try again. See ?read_wavs for more information")
  }
}



.new_observations <- function(input_path, site_col, data_path) {
  file_list <- .get_file_list(input_path) # Get list of files
  observations <- suppressWarnings(.read_file_GUANO(file_list, site_col)) # Read GUANO
  .save_to_RDATA(observations, data_path)
}

.add_observations <- function(input_path, site_col, data_path) {
  load(data_path) # Load current data
  observations_original <- observations # Rename current observations for clarity
  rm(observations) # Remove current observations df for clarity
  observations_new <- .get_file_list(input_path) # Get list observations in input director
  observations_new <- observations_new[!(observations_new$File.Name %in% observations_original$File.Name),] # Remove directory observations already present in current data
  observations_new <-suppressWarnings(.read_file_GUANO(observations_new, site_col)) # Read GUANO for new files
  #observations_new <- merge(observations_new2, observations_new, by.x = "File.Name", by.y = "File.Name") # Combine GUANO and file metadata
  #observations_new <- observations_new[, which(names(observations_new) != "Full.Path")] # Remove duplicate column
  observations_original[setdiff(names(observations_new), names(observations_original))] <- NA # Prepare for column differences in tables
  observations_new[setdiff(names(observations_original), names(observations_new))] <- NA
  observations <- rbind(observations_original, observations_new) # Bind original and new data
  observations <- observations[order(observations$File.Name),] # Sort
  rownames(observations) <- NULL # Reset row names
  .save_to_RDATA(observations, data_path)
}

.update_observations <- function(input_path, site_col, data_path) {
  directory_files <- .get_file_list(input_path) # Get input file list
  load(data_path) # Load existing data
  observations_files <- observations[, c("File.Name", "File.Path", "File.Modified")] # Trim existing data to relevant columns
  names(observations_files)[1:3] <- c("File.Name","Observations.Path", "Observations.Modified") # Rename existing data columns for clarify
  files_comparison <- merge(directory_files, observations_files, by = "File.Name", all = T) # Merge new and existing data frames by filename
  files_comparison <- files_comparison[!is.na(files_comparison$Full.Path),]
  files_comparison$update <- ifelse(mapply(function(x, y) {isTRUE(all.equal(x, y))}, files_comparison$File.Modified, files_comparison$Observations.Modified), "N", 
                                    ifelse(files_comparison$File.Modified > files_comparison$Observations.Modified, "Y", "N")) # Mark files where modified dates don't match, and input file version is newer
  update_files <- subset(files_comparison, update == "Y") # Subset files to update
  if (length(update_files$Full.Path) == 0) {
    stop("No files to update!")
  }
  update_files <- update_files$Full.Path
  update_files <- .get_file_list(update_files, list = T)
  observations_update <- suppressWarnings(.read_file_GUANO(update_files, site_col))

  observations_original <- observations[!(observations$File.Name %in% observations_update$File.Name),] # Remove updated rows from original data
  observations_files[setdiff(names(observations_update), names(observations_original))] <- NA # Account for differences in columns
  observations_update[setdiff(names(observations_original), names(observations_update))] <- NA # ^
  observations <- rbind(observations_original, observations_update) # Bind new and updated rows
  observations <- observations[order(observations$File.Name),] # Reorder by filename
  rownames(observations) <- NULL # Reset row names
  .save_to_RDATA(observations, data_path)
}  

.get_file_list <- function(input_path, list = F) {
  if (list == F) {
    file_list <- list.files(input_path, full.names = TRUE, recursive = TRUE, pattern = ".wav")
  } else {
    file_list <- input_path
  }
  file_list <- cbind(file=file_list,
                   dir=dirname(file_list),
                   data.frame(file.info(file_list), row.names =NULL),
                   stringsAsFactors=FALSE)
  file_list$filename <- sub('.*\\/', '', file_list$file)
  file_list <- file_list[, c("filename", 'file', "mtime")]
  names(file_list)[1:3] <- c("File.Name", "Full.Path", "File.Modified")
  file_list$File.Modified <- as.numeric(file_list$File.Modified)
  return(file_list)
}

.read_file_GUANO <- function(file_list, site_col) {
  
  observations <- do.call(plyr::rbind.fill, (lapply(pbapply::pblapply(as.list(file_list$Full.Path), read.guano), as.data.frame))) # Read GUANO
  observations <- merge(observations, file_list, by.x = "File.Name", by.y = "File.Name") # Add file modified columns to GUNAO df
  observations <- observations[ , !names(observations) %in% c("Full.Path","Anabat.Signature")] # Remove undesired columns
  
  observations <- within(observations, {
    Night = ifelse(lubridate::hour(observations$Timestamp) > 11, 
                   as.Date(observations$Timestamp, tz = "EST"), 
                   as.Date(observations$Timestamp, tz = "EST") - 1)
  }) # Generate Night column, accounting for observations after midnight
  observations$Night <- as.Date(observations$Night, origin = "1970-01-01") # Set format of date column
  
  observations = within(observations, {
    Species = ifelse(is.na(observations$Species.Manual.ID), as.character(observations$Species.Auto.ID), as.character(observations$Species.Manual.ID))
  }) # Generate Species column
  
  # observations <- observations[observations$Species %in% species_list, ] # Remove rows without species in species list
  
  colnames(observations)[colnames(observations)=="Loc.Position.Lat"] <- "Latitude"
  colnames(observations)[colnames(observations)=="Loc.Position.Lon"] <- "Longitude"
  colnames(observations)[colnames(observations)==site_col] <- "Location" # Set location column
    
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
  
  observations <- dplyr::select(observations, Timestamp, Species, Location, Latitude, Longitude, Species.Auto.ID, Species.Manual.ID, Night, File.Name, everything())
  return(observations)
}
