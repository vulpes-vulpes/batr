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
    message("Create new data file")
  } else if (action == "Add") {
    message("Add files to existing")
  } else if (action == "Update") {
    message("Update new files")
  } else {
    stop("Invalid action given, please specifiy an action (\"New\", \"Add\" or \"Update\") and try again. See ?read_wavs for more information")
  }
}