#'Copy a Random Subset of Files to a New Directory for Manual Vetting
#'
#'\code{manual_vet_extractor} copies a randomly selected subset of files in a
#'project to a new folder to facilitate manual vetting. The function will only
#'move files with an accepted automatic species assignment from SonoBat.
#'
#'@family Manual Vetting Functions
#'
#'@param data_path Character. Path to an existing RData file for the data set
#'  you wish to manually vet.
#'@param WAV_directory Character, the parent directory containing original WAV
#'  files.
#'@param save_directory Character, the destination where selected files will be
#'  copied to
#'@param species_list Character, a single species or list in the form of four
#'  character species codes used by SonoBat, e.g. "Epfu". Defaults to non
#'  Ontario SAR: \code{c("Epfu", "Labo", "Laci", "Lano")}.
#'@param percentage Number. What proportion of files do you wish to extract.
#'  Defaults to 0.05 - 5 percent.
#'@param no_manual Logical vector, defaults to \code{FALSE}. If \code{TRUE} the
#'  function will ignore files that have already been assigned an ID.
#'
#'@return Creates folder within the file folder filled with a percentage of
#'  files of the selected copied.
#'
#'@examples
#'\dontrun{
#' manual_vet_extractor("raw_data_project", "C:/Folder/Folder/File_Folder")
#'}
#'@export
manual_vet_extractor <- function(data_path, WAV_directory, save_directory, species_list, percentage = 0.05, no_manual = FALSE) {
  .check_data_path(data_path)
  load(data_path)
  dataset <- observations # rename for ease
  if(no_manual == TRUE) {
    dataset <- dataset[is.na(dataset$Species.Manual.ID),]
  } # drop observations with existing manual IDs if selected
  dataset <- dataset[!is.na(dataset$Species),] # drop observations without speices
  message("Matching observations to WAV files.")
  WAV_directory_files <- .get_file_list(WAV_directory) # get file list from the directory of WAV files
  dataset2 <- merge(dataset, WAV_directory_files, by = "File.Name", all.x = T) # merge the list of observations with the list of WAV files provided
  if (sum(is.na(dataset2$Full.Path)) != 0) {
    message("Error: cannot locate WAV files for some observations in the specified WAV_directory. 
    Would you like to see a list of observations that cannot be located?")
    report <- readline(prompt="y/n:")
    if (report == "y" | report == "Y") {
      missing_files <- dataset2[is.na(dataset2$Full.Path),]
      missing_files <- missing_files$File.Name
      message("Ending due to missing WAV files, no file have been copied for manual vetting.")
      return(missing_files)
    }
    stop("Ending due to missing WAV files, no file have been copied for manual vetting.")
  } # check if any WAV files are missing for the specified observations and offer a list if not
  for (i in species_list) {
    message(paste("Copying", i, "vetting files to output directory."))
    if(!dir.exists(paste(save_directory,"/",i,sep=""))) # Does species folder exist within 5% folder?
      dir.create(paste(save_directory,"/",i,sep="")) # If not then create it.
    temp_dataset <- dataset2[ sample( which( dataset2$Species == i ) , (sum(dataset2$Species == i)*percentage) ) , ] # Create a temporary dataset with a random 5% of the rows for species.
    file.copy(temp_dataset$Full.Path,
              paste(save_directory,"/",i,sep="")) # Copy the five percent subset to the previously created species folder.
  }
}