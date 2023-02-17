#'Copy a Random Subset of Files to a New Directory for Manual Vetting
#'
#'\code{manual_vet_extractor} copies a randomly selected subset of files in a
#'project to a new folder to facilitate manual vetting. The function will only
#'move files with an accepted automatic species assignment from SonoBat.
#'
#'@family Manual Vetting Functions
#'
#'@param data_path Character. Path to the directory containing the files you
#'   wish to read the from. Note that sub-directories are also read.
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
manual_vet_extractor <- function(data_path, save_directory, species_list, percentage = 0.05, no_manual = FALSE) {
  .check_data_path(data_path)
  load(data_path)
  dataset <- observations
  if(no_manual == TRUE) {
    dataset <- dataset[is.na(dataset$Species.Manual.ID),]
  }
  dataset <- dataset[!is.na(dataset$Species),]
  for (i in species_list) {
    if(!dir.exists(paste(save_directory,"/Five_Percent/",sep=""))) # Does 5% folder exist?
      dir.create(paste(save_directory,"/Five_Percent/",sep="")) # If not then create it.
    if(!dir.exists(paste(save_directory,"/Five_Percent/",i,sep=""))) # Does species folder exist within 5% folder?
      dir.create(paste(save_directory,"/Five_Percent/",i,sep="")) # If not then create it.
    temp_dataset <- dataset[ sample( which( dataset$Species == i ) , (sum(dataset$Species == i)*percentage) ) , ] # Create a temporary dataset with a random 5% of the rows for species.
    file.copy(temp_dataset$File.Path,paste(save_directory,"/Five_Percent/",i,sep="")) # Copy the five percent subset to the previuosly created species folder.
  }
}