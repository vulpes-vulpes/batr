#'Load Observations
#'
#'Loads an existing observations.csv file into the workspace as a data frame.
#'
#'@family Import Functions
#'
#'@param path Path to dataset.
#'
#'@return A data frame of observations in the environment.
load_observations <- function(path) {
  load(paste(path, "/BAM_Data.RData", sep = ""))
  assign("observations", observations, envir=globalenv())
}

#'New Observations
#'
#'Reads file GUANO and creates: a new observations.csv file, and loads the
#'result into the workspace as a data frame.
#'
#'@family Import Functions
#'
#'@param path Path to dataset.
#'
#'@return A data frame of observations in the environment and a new
#'  observations.csv saved at target folder.
new_observations <- function(path) {
  filesdf <- list.files(path, full.names = TRUE, recursive = TRUE, pattern = ".wav")
  filesdf <- cbind(file=filesdf,
                   dir=dirname(filesdf),
                   data.frame(file.info(filesdf), row.names =NULL),
                   stringsAsFactors=FALSE)
  filesdf$filename <- sub('.*\\/', '', filesdf$file)
  filesdf <- filesdf[, c("filename", 'file', "mtime")]
  names(filesdf)[1:3] <- c("File.Name", "Full.Path", "File.Modified")
  filesdf$File.Modified <- as.numeric(filesdf$File.Modified)
  observations <- do.call(plyr::rbind.fill, (lapply(pbapply::pblapply(as.list(filesdf$Full.Path), read.guano), as.data.frame)))
  observations <- merge(observations, filesdf, by.x = "File.Name", by.y = "File.Name")
  observations <- observations[, which(names(observations) != "Full.Path")]
  assign("observations", observations, envir=globalenv())
  save(observations, file = paste(path, "/BAM_Data.RData", sep = ""))
  
  #write.csv(observations, paste(path, "/Observations.csv", sep = ""))
}

#'Update Observations
#'
#'???
#'
#'@family Import Functions
#'
#'@param path Path to dataset. 
#'
#'@return A data frame of observations in the environment.
update_observations <- function(path) {
  
  # Get list of .wav files and file modification dates:
  
  directory_files <- list.files(path, full.names = TRUE, recursive = TRUE, pattern = ".wav")
  directory_files <- cbind(file=directory_files,
                   dir=dirname(directory_files),
                   data.frame(file.info(directory_files), row.names =NULL),
                   stringsAsFactors=FALSE)
  directory_files$filename <- sub('.*\\/', '', directory_files$file)
  directory_files <- directory_files[, c("filename", "file", "mtime")]
  names(directory_files)[1:3] <- c("File.Name","Directory.Path", "Directory.Modified")
  directory_files$Directory.Modified <- as.numeric(directory_files$Directory.Modified)
  
  # Get list of file names and modification dates from the observations table:
  
  load(paste(path, "/BAM_Data.RData", sep = ""))
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
  assign("observations", observations, envir=globalenv())
  save(observations, file = paste(path, "/BAM_Data.RData", sep = ""))  
  
}