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
#' @family species tools
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
#' @family species tools
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
} # Return a Complete Species List From an RData File

#' Returns a data frame listing all unique species identifications included in a
#' dataset in an RData file
#'
#' @param data_path Character. Path to an RData file to add activity data to.
#'   Optional, a save location will be created before data are saved.
#'
#' @return A data frame containing all unique species identifications.
#' @export
#'
#' @family species tools
#'
#' @examples \dontrun{set_species_list("C:/Folder/Folder/Data.RData")}
list_all_species <- function(data_path) {
  .check_data_path(data_path)
  load(data_path)
  if (exists("observations")) {
  all_species <- unique(observations$Species)
  } else {
    stop("No observations found, please check RData file.")
  }
  if (length(all_species) > 0) {
    assign("all_species", as.data.frame(all_species), envir=globalenv())
  } else {
    stop("No species found, please check the data and try again.")
  }
} # Reveal a set species list if present
