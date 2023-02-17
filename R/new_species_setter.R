#' Save a Species List into an RData File
#'
#' Saves a species list into a batr RData file so that you don't have to specify
#' it each time you run a function. If no species list is specified functions
#' will ask for it.
#'
#' @param species_list A vector list of species codes that correspond to those
#'   in the GUANO metadata for the files in the data set
#' @param data_path Character. Path to an RData file to add activity data to.
#'   Optional, a save location will be created before data are saved.
#'
#' @return An RData file containing a list of species for the project, along
#'   with other data if added to an existing RData file
#' @export
#'
#' @family import tools
#'
#' @examples \dontrun{set_species_list(c("Epfu", "Labo"), "C:/Folder/Folder/Data.RData")}
set_species_list <- function(species_list, data_path) {
  if (is.vector(species_list)) {
    message("Vector found, proceeding.")
  } else {
    stop("Species list provided is not a vector, please check and try again.")
  } # Check if the object provided is a vector, and exit if not.
  .check_data_path(data_path)
  .save_to_RDATA(species_list, data_path)
}

#' Show a Species List that has been saved into an RData File
#'
#' Reveals a Species List that has been saved into an existing Rdata file.
#'
#' @param data_path Character. Path to an RData file to add activity data to.
#'   Optional, a save location will be created before data are saved.
#'
#' @return A vector of the saved Species List, if one exists.
#' @export
#'
#' @family import tools
#'
#' @examples \dontrun{set_species_list("C:/Folder/Folder/Data.RData")}
show_species_list <- function(data_path) {
  .check_data_path(data_path)
  load(data_path)
  if (exists(species_list)) {
    return(species_list)
  } else {
   "Species List not found." 
  }
} # Reveal a set species list if present


