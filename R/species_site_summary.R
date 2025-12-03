#' Species Site Summary Table
#'
#' Returns a table summarizing the total number of observations by site and
#' species.
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data.
#' @param species_list Character vector. Species codes to include in the summary.
#'   If \code{NULL} (default), includes all species found in the dataset.
#'   If specified, filters to only those species and adds empty columns for
#'   any species in the list that don't appear in the data.
#' @param location_list Character vector. Location names to include in the summary.
#'   If \code{NULL} (default), includes all locations found in the dataset.
#'   If specified, filters to only those locations and adds empty rows for
#'   any locations in the list that don't appear in the data.
#'
#' @return A data frame of observations summarized by location and species,
#'   with a "Totals" row at the bottom.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Include all species and locations found in data
#' summary <- summary_table("C:/Folder/Folder/Data.RData")
#' 
#' # Include only specific species
#' summary <- summary_table("C:/Folder/Folder/Data.RData", 
#'                           species_list = c("Epfu", "Lano"))
#' 
#' # Include only specific locations
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'                           location_list = c("Site1", "Site2"))
#' 
#' # Filter by both species and locations
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'                           species_list = c("Epfu", "Lano"),
#'                           location_list = c("Site1", "Site2"))
#' }
summary_table <- function(data_path, 
                          species_list = NULL,
                          location_list = NULL) {
  # Validate inputs
  .check_data_path(data_path)
  
  if (!is.null(species_list) && (!is.character(species_list) || length(species_list) == 0)) {
    stop("species_list must be a character vector with at least one element")
  }
  
  if (!is.null(location_list) && (!is.character(location_list) || length(location_list) == 0)) {
    stop("location_list must be a character vector with at least one element")
  }
  
  # Load data
  load(data_path)
  
  if (!exists("observations")) {
    stop("No 'observations' object found in ", data_path)
  }
  
  # Use observations as dataset
  dataset <- observations
  
  # Remove rows with NA species
  dataset <- dataset[!is.na(dataset$Species), ]
  
  if (nrow(dataset) == 0) {
    stop("No valid observations found after removing NA values")
  }
  
  # Warn about missing requested items and filter
  if (!is.null(location_list)) {
    available_locations <- unique(dataset$Location)
    missing_req_locations <- setdiff(location_list, available_locations)
    if (length(missing_req_locations) > 0) {
      message(sprintf("Note: %d location(s) not found in data: %s", 
                      length(missing_req_locations),
                      paste(missing_req_locations, collapse = ", ")))
    }
    # Keep only observations for requested locations
    dataset <- dataset[dataset$Location %in% location_list, ]
  }
  
  if (!is.null(species_list)) {
    available_species <- unique(dataset$Species)
    missing_req_species <- setdiff(species_list, available_species)
    if (length(missing_req_species) > 0) {
      message(sprintf("Note: %d species not found in data: %s",
                      length(missing_req_species),
                      paste(missing_req_species, collapse = ", ")))
    }
    # Keep only observations for requested species
    dataset <- dataset[dataset$Species %in% species_list, ]
  }
  
  if (nrow(dataset) == 0) {
    warning("No observations match the specified filters")
    # Return empty table with requested structure
    empty_df <- data.frame(Location = character(0))
    if (!is.null(species_list)) {
      for (sp in species_list) {
        empty_df[[sp]] <- integer(0)
      }
    }
    return(empty_df)
  }
  
  # Create summary table
  species_site_summary <- as.data.frame.matrix(table(dataset$Location, dataset$Species))
  species_site_summary <- cbind(Location = rownames(species_site_summary), species_site_summary)
  rownames(species_site_summary) <- NULL
  
  # Add rows for locations in location_list that aren't in the data
  if (!is.null(location_list)) {
    missing_locations <- setdiff(location_list, species_site_summary$Location)
    if (length(missing_locations) > 0) {
      # Create empty rows for missing locations
      for (loc in missing_locations) {
        new_row <- data.frame(Location = loc, stringsAsFactors = FALSE)
        for (col in setdiff(names(species_site_summary), "Location")) {
          new_row[[col]] <- 0
        }
        species_site_summary <- rbind(species_site_summary, new_row)
      }
    }
    
    # Reorder rows to match location_list order
    species_site_summary <- species_site_summary[match(location_list, species_site_summary$Location), , drop = FALSE]
  }
  
  # Add columns for species in species_list that aren't in the data
  if (!is.null(species_list)) {
    for (species in species_list) {
      if (!species %in% colnames(species_site_summary)) {
        species_site_summary[[species]] <- 0
      }
    }
    
    # Reorder columns: Location first, then species_list order
    col_order <- c("Location", species_list)
    species_site_summary <- species_site_summary[, col_order, drop = FALSE]
  }
  
  # Add totals row
  numeric_cols <- which(vapply(species_site_summary, is.numeric, logical(1)))
  totals_row <- colSums(species_site_summary[, numeric_cols, drop = FALSE], na.rm = TRUE)
  species_site_summary <- rbind(species_site_summary, c(Location = "Totals", as.list(totals_row)))
  
  # Convert numeric columns back to numeric (rbind coerces to character)
  for (col in numeric_cols) {
    species_site_summary[[col]] <- as.numeric(species_site_summary[[col]])
  }
  
  return(species_site_summary)
}
