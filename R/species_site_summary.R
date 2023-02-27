#' Species Site Summary Table
#'
#' Returns a table summarizing the total number of observations by site and
#' location.
#'
#' @param data_path
#'
#' @param all_species Logical vector. Default is \code{false}, if \code{true}
#'   creates empty columns for species in species_list that are not present in
#'   the dataset.
#'
#' @return A data frame of observations summarized by location and species.
#' @export
#'
#' @examples \dontrun{summary_table("C:/Folder/Folder/Data.RData")}
summary_table <- function(data_path,
                          all_species = FALSE) {
  .check_data_path(data_path)
  dataset <- .location_subsetter(data_path)
  dataset <- .species_subsetter(data_path, dataset)
  load(data_path)
  # Create Summary Table:
  species_site_summary <- as.data.frame.matrix(table(dataset$Location,dataset$Species)) # Create table of Species by Location
  species_site_summary <- cbind(Location = rownames(species_site_summary), species_site_summary) # Fix row names to column
  rownames(species_site_summary) <- NULL # with above
  # Create column for each Ontario species in case they were not recorded
  if (isTRUE(all_species)) {
    if (!exists("species_list")) {
      .species_requester(data_path)
    }
    for (species in species_list) {
      if(!species %in% colnames(species_site_summary)) {species_site_summary[,species] <- 0}
    }
  }
  
  if(isTRUE(all_myotis)) {
    
  }
  
  #species_site_summary$`All Myotis` <- rowSums(species_site_summary[, c("Mylu", "Myle", "Myse", "Mysp")])
  #species_site_summary$Year = substr(species_site_summary$Location, 1, 4)
  #species_site_summary$Location <- substring(species_site_summary$Location, 6, 1000000L)
  myNumCols <- which(unlist(lapply(species_site_summary, is.numeric)))
  species_site_summary[(nrow(species_site_summary) + 1), myNumCols] <- colSums(species_site_summary[, myNumCols], na.rm=TRUE)
  x <- as.numeric(length(species_site_summary[,1]))
  species_site_summary[x,1] = "Totals"
  #colnames(species_site_summary)[colnames(species_site_summary) == 'Location2'] <- 'Location'
  #species_site_summary <- species_site_summary[c("Location", "Epfu", "Labo", "Laci", "Lano", "Myle", "Mylu", "Myse", "Mysp", "All Myotis", "Pesu")]
  assign(paste("species_site_summary", sep = ""), species_site_summary, envir=globalenv())
}


