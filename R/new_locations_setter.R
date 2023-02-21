#' Save a Location List into an RData File
#'
#' Saves a location list into a batr RData file so that you don't have to specify it
#' each time you run a function. If no location list is specified functions will ask
#' for it.
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
#' @family location tools
#'
#' @examples \dontrun{set_location_list(c("location1", "location2"), "C:/Folder/Folder/Data.RData")}
set_location_list <- function(location_list, data_path) {
  if (is.vector(location_list)) {
    message("Vector found, proceeding.")
  } else {
    stop("Species list provided is not a vector, please check and try again.")
  } # Check if the object provided is a vector, and exit if not.
  .check_data_path(data_path)
  .save_to_RDATA(location_list, data_path)
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
#' @family location tools
#'
#' @examples \dontrun{set_species_list("C:/Folder/Folder/Data.RData")}
show_location_list <- function(data_path) {
  .check_data_path(data_path)
  load(data_path)
  if (exists("location_list")) {
    return(location_list)
  } else {
   "Location List not found." 
  }
} # Reveal a set species list if present

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
#' @family location tools
#'
#' @examples \dontrun{set_species_list("C:/Folder/Folder/Data.RData")}
list_all_locations <- function(data_path) {
  .check_data_path(data_path)
  load(data_path)
  if (exists("observations")) {
  all_locations <- unique(observations$Location)
  } else {
    stop("No observations found, please check RData file.")
  }
  if (length(all_locations) > 0) {
    assign("all_locations", as.data.frame(all_locations), envir=globalenv())
  } else {
    stop("No locations found, please check the data and try again.")
  }
} # Reveal a set species list if present

.location_requester <- function(data_path) {
  message("Please supply a list of locations separated by commas (e.g. \"Location 1, Location 2\")).")
  location_list <- readline(prompt = "List:")
  location_list <- as.vector(unlist(strsplit(location_list, ", ")))
  set_location_list(location_list, data_path)
  .save_to_RDATA(location_list, data_path)
  return(location_list) # Function to request a list of locations for subsetting: returns a vector of locations and adds to the specified RData file
}
