#'Copy a Random Subset of Files to a New Directory for Manual Vetting
#'
#'\code{manual_vet_extractor} copies a randomly selected subset of files in a
#'project to a new folder to faciiliate manual vetting. The function will only
#'move files with an accepted automatic species assingment from SonoBat.
#'
#'@family Manual Vetting Functions
#'
#'@param dataset Character, a "raw_data_..." output from \code{GUANO_loader}.
#'@param directory Character, the directory where selected files shoud be copied
#'  to.
#'@param species_list Character, a single species or list in the form of four
#'  character species codes used by SonoBat, e.g. "Epfu". Defaults to non
#'  Ontario SAR: \code{c("Epfu", "Labo", "Laci", "Lano")}.
#'@param percentage Number. What proportion of files do you wish to extract.
#'  Defaults to 0.05 - 5 percent.
#'@param no_manual Logical vector, defaults to \code{FALSE}. If \code{TRUE} the
#'  function will ignore files that have already been assigned an ID.
#'
#'@return A folder within the file folder filled with a percentage of files of
#'  the selected copied.
#'
#'@examples
#'\dontrun{
#' manual_vet_extractor("raw_data_project", "C:/Folder/Folder/File_Folder")
#'}
#'@export
manual_vet_extractor_old <- function(dataset, directory, species_list = c("Epfu", "Labo", "Laci", "Lano"), percentage = 0.05, no_manual = FALSE) {
  if(no_manual == TRUE)
    dataset <- dataset[is.na(dataset$Species.Manual.ID),]
  dataset <- dataset[!is.na(dataset$Species),]
  for (i in species_list) {
    if(!dir.exists(paste(directory,"/Five_Percent/",sep=""))) # Does 5% folder exist?
      dir.create(paste(directory,"/Five_Percent/",sep="")) # If not then create it.
    if(!dir.exists(paste(directory,"/Five_Percent/",i,sep=""))) # Does species folder exist within 5% folder?
      dir.create(paste(directory,"/Five_Percent/",i,sep="")) # If not then create it.
    temp_dataset <- dataset[ sample( which( dataset$Species == i ) , (sum(dataset$Species == i)*percentage) ) , ] # Create a temporary dataset with a random 5% of the rows for species.
    file.copy(temp_dataset$File.Path,paste(directory,"/Five_Percent/",i,sep="")) # Copy the five percent subset to the previuosly created species folder.
  }
}

#'Copy or Move a Random Subset of High Frequency Files to a New Sub Directory for Manual Vetting
#'
#'\code{high_frequency_extractor} moves or copies a proportion of high frequency
#'files from a directory. To do so reqruies a text file output to be generated
#'for the directory in SonoBat.
#'
#'@section Note: this function will only work for files within a single
#'  directory. \strong{It will not work correctly where files are arranged in
#'  sub-directories!}
#'
#'@family Manual Vetting Functions
#'
#'@param sonobat_output Character, a \strong{full path} to a text file output of
#'  the directory created from SonoBat > SonoVet > Export > Use Current Layout
#'@param directory Character, a \strong{full path} to the folder containg the
#'  files you wish to subset.
#'@param percentage Number. What proportion of files do you wish to extract.
#'  Defaults to 0.1 - 10 percent.
#'@param move_files Logical vector, defaults to \code{TRUE}. If \code{TRUE} the
#'  function will \strong{move} selected files into a new directory. If
#'  \code{FALSE} the function will \strong{copy} files into a new directory.
#'
#'@return A folder within the file folder filled with a percentage of high
#'  frequency files moved or copied.
#'
#'@examples
#'\dontrun{
#' high_frequency_extractor("C:/Folder/Files_Folder/sonobat_output.txt", "C:/Folder/Files_Folder", percentage = 0.1, move_files = TRUE)
#'}
#'@export
high_frequency_extractor <- function (sonobat_output, directory, percentage = 0.1, move_files = TRUE) {
  dataset <- read.table(sonobat_output, header = T, sep="\t", comment.char = "")
  dataset <- dataset[dataset$HiF %in% "1", ]
  subset <- dplyr::sample_frac(dataset, percentage)
  if(!dir.exists(paste(directory,"/Duplicates_HiF_Manual_Vet",sep=""))) # Does 5% folder exist?
    dir.create(paste(directory,"/Duplicates_HiF_Manual_Vet",sep="")) # If not then create it.
  if(isTRUE(move_files)) {
    file.rename(paste(directory, "/", subset$Filename, sep=""), paste(directory,"/Duplicates_HiF_Manual_Vet/", subset$Filename, sep=""))
  } else {
    file.copy(paste(directory, "/", subset$Filename, sep=""), paste(directory,"/Duplicates_HiF_Manual_Vet",sep=""))
  }
  message("File Selection Completed!")
}
