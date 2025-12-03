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
#'   species_list = c("Epfu", "Lano")
#' )
#'
#' # Include only specific locations
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'   location_list = c("Site1", "Site2")
#' )
#'
#' # Filter by both species and locations
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'   species_list = c("Epfu", "Lano"),
#'   location_list = c("Site1", "Site2")
#' )
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
  
  original_count <- nrow(dataset)

  # Identify and report rows with NA species or location
  na_species <- is.na(dataset$Species)
  na_location <- is.na(dataset$Location)
  na_both <- na_species & na_location
  
  n_na_species <- sum(na_species & !na_location)
  n_na_location <- sum(na_location & !na_species)
  n_na_both <- sum(na_both)
  
  if (n_na_species > 0 || n_na_location > 0 || n_na_both > 0) {
    message(sprintf(
      "Removing %d observation(s) with missing data: %s%s%s",
      sum(na_species | na_location),
      if (n_na_species > 0) sprintf("%d with NA species", n_na_species) else "",
      if (n_na_location > 0 && n_na_species > 0) ", " else "",
      if (n_na_location > 0) sprintf("%d with NA location", n_na_location) else "",
      if (n_na_both > 0) sprintf(" (%d with both NA)", n_na_both) else ""
    ))
  }
  
  # Remove rows with NA species or location
  dataset <- dataset[!na_species & !na_location, ]

  if (nrow(dataset) == 0) {
    stop("No valid observations found after removing NA values")
  }

  # Warn about missing requested items and filter
  if (!is.null(location_list)) {
    available_locations <- unique(dataset$Location)
    missing_req_locations <- setdiff(location_list, available_locations)
    if (length(missing_req_locations) > 0) {
      message(sprintf(
        "Note: %d location(s) not found in data: %s",
        length(missing_req_locations),
        paste(missing_req_locations, collapse = ", ")
      ))
    }
    # Keep only observations for requested locations
    dataset <- dataset[dataset$Location %in% location_list, ]
  }

  if (!is.null(species_list)) {
    available_species <- unique(dataset$Species)
    missing_req_species <- setdiff(species_list, available_species)
    if (length(missing_req_species) > 0) {
      message(sprintf(
        "Note: %d species not found in data: %s",
        length(missing_req_species),
        paste(missing_req_species, collapse = ", ")
      ))
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

  # Create summary table using table()
  count_table <- table(dataset$Location, dataset$Species)
  species_site_summary <- as.data.frame.matrix(count_table)
  species_site_summary$Location <- rownames(species_site_summary)
  rownames(species_site_summary) <- NULL

  # Move Location column to first position
  species_site_summary <- species_site_summary[, c("Location", setdiff(names(species_site_summary), "Location")), drop = FALSE]

  # Determine final locations and species to include
  if (!is.null(location_list)) {
    final_locations <- location_list
  } else {
    final_locations <- unique(species_site_summary$Location)
  }

  if (!is.null(species_list)) {
    final_species <- species_list
  } else {
    final_species <- setdiff(names(species_site_summary), "Location")
  }

  # Build complete table with all requested locations and species (vectorized approach)
  complete_table <- data.frame(Location = final_locations, stringsAsFactors = FALSE)

  # Add each species column efficiently
  for (sp in final_species) {
    if (sp %in% names(species_site_summary)) {
      # Merge existing counts using match for vectorized lookup
      idx <- match(complete_table$Location, species_site_summary$Location)
      complete_table[[sp]] <- ifelse(is.na(idx), 0L, species_site_summary[[sp]][idx])
      complete_table[[sp]][is.na(complete_table[[sp]])] <- 0L
    } else {
      # Species not in data, add column of zeros
      complete_table[[sp]] <- 0L
    }
  }

  # Add totals row efficiently (single rbind operation, maintain integer type)
  totals <- colSums(complete_table[, -1, drop = FALSE], na.rm = TRUE)
  totals_row <- data.frame(Location = "Totals", as.list(totals), stringsAsFactors = FALSE)
  complete_table <- rbind(complete_table, totals_row)

  return(complete_table)
}
