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
read_wavs <- function(action = NULL, input_path = NULL, data_path = NULL) { #, verbose_observations = F, species_list = c("Epfu", "Labo", "Laci", "Lano", "Myle", "Mylu", "Myse", "Mysp", "Pesu")) {

  if (is.null(action)) {
    stop("No action given, please specifiy an action (\"New\", \"Add\" or \"Update\") and try again. See ?read_wavs for more information")
  } else if (action == "New") {
    .new_observations(input_path, data_path)
  } else if (action == "Add") {
    .add_observations(input_path, data_path)
  } else if (action == "Update") {
    .update_observations()
  } else {
    stop("Invalid action given, please specifiy an action (\"New\", \"Add\" or \"Update\") and try again. See ?read_wavs for more information")
  }
}

#'New Observations
#'
#'Reads file GUANO and saves them into an RData file. 
#'
#'@family Import Functions
#'
#'@param path Path to dataset.
#'
#'@return A data frame of observations in the environment and a new
#'  observations.csv saved at target folder.
.new_observations <- function(input_path, data_path) {
  file_list <- .get_file_list(input_path)
  observations <- do.call(plyr::rbind.fill, (lapply(pbapply::pblapply(as.list(file_list$Full.Path), read.guano), as.data.frame)))
  observations <- merge(observations, file_list, by.x = "File.Name", by.y = "File.Name")
  observations <- observations[, which(names(observations) != "Full.Path")]
  if (is.null(data_path)) {
    save_path <- readline(prompt="No 'data_path' specified. Please input path to save data file (e.g. C/user/directory):")
    if (!dir.exists(save_path)) {
      print("Error! Directory does not exist. Exiting, file not saved!.")
    } else {
      filename <- readline(prompt="Please input your desired filename:")
      data_path <- paste(save_path, "/", filename, ".RData", sep = "")
      save(observations, file = data_path)
      # assign("observations", observations, envir=globalenv())
    }
  } else if (!is.null(data_path)) {
    save(observations, file = data_path)
  }
}
  
#'Update Observations
#'
#'Compares directory files to existing dataset and updates as required.
#'
#'@family Import Functions
#'
#'@param path Path to dataset. 
#'
#'@return A data frame of observations in the environment.
.update_observations <- function(input_path, data_path) {
  
  # Get list of .wav files and file modification dates:
  
  directory_files <- .get_file_list(input_path)
  
  # Get list of file names and modification dates from the observations table:
  
  load(data_path)
  observations_files <- observations[, c("File.Name", "File.Path", "File.Modified")]
  names(observations_files)[1:3] <- c("File.Name","Observations.Path", "Observations.Modified")
  
  # Merge the two lists
  
  files_comparison <- merge(directory_files, observations_files, by = "File.Name", all = T)
  
  # Identify files to update in dataset
  files_comparison$update <- ifelse(mapply(function(x, y) {isTRUE(all.equal(x, y))}, files_comparison$Directory.Modified, files_comparison$Observations.Modified), "N", 
                                    ifelse(files_comparison$Directory.Modified > files_comparison$Observations.Modified, "Y", "N"))
  
  # Read GUNANO from updated files
  update_files <- subset(files_comparison, update == "Y")
  update_files <- update_files$Directory.Path
  update_files <- cbind(file=update_files,
                        dir=dirname(update_files),
                        data.frame(file.info(update_files), row.names =NULL),
                        stringsAsFactors=FALSE)
  update_files$filename <- sub('.*\\/', '', update_files$file)
  update_files <- update_files[, c("filename", 'file', "mtime")]
  names(update_files)[1:3] <- c("File.Name", "Full.Path", "File.Modified")
  update_files$File.Modified <- as.numeric(update_files$File.Modified)
  observations_update <- do.call(plyr::rbind.fill, (lapply(pbapply::pblapply(as.list(update_files$Full.Path), read.guano), as.data.frame)))
  observations_update <- merge(observations_update, update_files, by.x = "File.Name", by.y = "File.Name")
  observations_update <- observations_update[, which(names(observations_update) != "Full.Path")]
  
  # Remove rows to update from dataset
  observations_original <- observations[!(observations$File.Name %in% observations_update$File.Name),]
  
  # Account for differences in columns
  observations_files[setdiff(names(observations_update), names(observations_original))] <- NA
  observations_update[setdiff(names(observations_original), names(observations_update))] <- NA
  
  # Bind original and updated rows, set order by filename and reset rownames
  observations <- rbind(observations_original, observations_update)
  observations <- observations[order(observations$File.Name),]
  rownames(observations) <- NULL
  
  # Assign and save!
  #assign("observations", observations, envir=globalenv())
  #save(observations, file = paste(path, "/BAM_Data.RData", sep = ""))  
  if (is.null(data_path)) {
    save_path <- readline(prompt="No 'data_path' specified. Please input path to save data file (e.g. C/user/directory):")
    if (!dir.exists(save_path)) {
      print("Error! Directory does not exist. Exiting, file not saved!.")
    } else {
      filename <- readline(prompt="Please input your desired filename:")
      data_path <- paste(save_path, "/", filename, ".RData", sep = "")
      save(observations, file = data_path)
      # assign("observations", observations, envir=globalenv())
    }
  }
}  

.add_observations <- function(input_path, data_path) {
  load(data_path) # Load current data
  observations_original <- observations # Rename current observations for clarity
  rm(observations) # Remove current observations df for clarity
  observations_new <- .get_file_list(input_path) # Get list observations in input director
  observations_new <- observations_new[!(observations_new$File.Name %in% observations_original$File.Name),] # Remove directory observations already present in current data
  
  observations_new2 <- do.call(plyr::rbind.fill, (lapply(pbapply::pblapply(as.list(observations_new$Full.Path), read.guano), as.data.frame)))
  observations_new <- merge(observations_new2, observations_new, by.x = "File.Name", by.y = "File.Name")
  observations_new <- observations_new[, which(names(observations_new) != "Full.Path")]
  
  observations_original[setdiff(names(observations_new), names(observations_original))] <- NA
  observations_new[setdiff(names(observations_original), names(observations_new))] <- NA
  observations <- rbind(observations_original, observations_new)
  observations <- observations[order(observations$File.Name),]
  rownames(observations) <- NULL
  save(observations, file = data_path)
}

.get_file_list <- function(input_path) {
  file_list <- list.files(input_path, full.names = TRUE, recursive = TRUE, pattern = ".wav")
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