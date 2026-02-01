#' Species Site Summary Table
#'
#' Returns a table summarizing the total number of observations by site and
#' species.
#'
#' @family Summary FunctionsSummary Functions
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
#' @param include_totals Logical. If \code{TRUE} (default), adds a "Totals" row
#'   at the bottom summarizing counts across all locations.
#' @param species_names Named character vector for custom species labels. Names should be
#'   species codes from the data, values should be display names. If NULL (default),
#'   species codes are used as column headers.
#' @param sort Logical. If \code{TRUE}, sorts locations and species alphabetically
#'   when no explicit list is provided. Defaults to \code{FALSE}.
#'
#' @return A data frame of observations summarized by location and species,
#'   optionally with a "Totals" row at the bottom.
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
#'
#' # Without totals row
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'   include_totals = FALSE
#' )
#'
#' # With alphabetical sorting
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'   sort = TRUE
#' )
#'
#' # With custom species labels
#' species_labels <- c(
#'   "Epfu" = "Big Brown Bat",
#'   "Lano" = "Silver-haired Bat"
#' )
#' summary <- summary_table("C:/Folder/Folder/Data.RData",
#'   species_list = c("Epfu", "Lano"),
#'   species_names = species_labels
#' )
#' }
summary_table <- function(data_path,
                          species_list = NULL,
                          location_list = NULL,
                          include_totals = TRUE,
                          species_names = NULL,
                          sort = FALSE) {
  # Validate and load data
  dataset <- .load_and_validate_summary_data(data_path, species_list, location_list)

  # Clean data
  dataset <- .clean_summary_dataset(dataset)

  # Filter data
  dataset <- .filter_summary_dataset(dataset, species_list, location_list)

  # Build summary table
  result_table <- .build_summary_table(dataset, species_list, location_list, sort)

  # Add totals row if requested
  if (include_totals) {
    result_table <- .add_totals_row(result_table)
  }

  # Apply custom species labels to column names if provided
  if (!is.null(species_names)) {
    species_cols <- setdiff(names(result_table), "Location")
    if (length(species_cols) > 0) {
      mapped_labels <- .species_label_map(species_cols, species_names)
      names(result_table)[match(species_cols, names(result_table))] <- unname(mapped_labels)
    }
  }

  return(result_table)
}

# Helper Functions ------------------------------------------------------------

#' Load and validate data for summary table
#' @keywords internal
.load_and_validate_summary_data <- function(data_path, species_list, location_list) {
  .validate_rdata_path(data_path)

  if (!is.null(species_list) && (!is.character(species_list) || length(species_list) == 0)) {
    stop("species_list must be a character vector with at least one element")
  }

  if (!is.null(location_list) && (!is.character(location_list) || length(location_list) == 0)) {
    stop("location_list must be a character vector with at least one element")
  }

  load(data_path)

  if (!exists("observations")) {
    stop("No 'observations' object found in ", data_path)
  }

  return(observations)
}

#' Clean dataset by removing NA values
#' @keywords internal
.clean_summary_dataset <- function(dataset) {
  na_species <- is.na(dataset$Species)
  na_location <- is.na(dataset$Location)
  na_both <- na_species & na_location

  n_na_species <- sum(na_species & !na_location)
  n_na_location <- sum(na_location & !na_species)
  n_na_both <- sum(na_both)

  if (n_na_species > 0 || n_na_location > 0 || n_na_both > 0) {
    parts <- character()
    if (n_na_species > 0) parts <- c(parts, sprintf("%d with NA species", n_na_species))
    if (n_na_location > 0) parts <- c(parts, sprintf("%d with NA location", n_na_location))
    if (n_na_both > 0) parts <- c(parts, sprintf("(%d with both NA)", n_na_both))

    message(sprintf(
      "Removing %d observation(s) with missing data: %s",
      sum(na_species | na_location),
      paste(parts, collapse = ", ")
    ))
  }

  dataset <- dataset[!na_species & !na_location, ]

  if (nrow(dataset) == 0) {
    stop("No valid observations found after removing NA values")
  }

  return(dataset)
}

#' Filter dataset by species and location lists
#' @keywords internal
.filter_summary_dataset <- function(dataset, species_list, location_list) {
  # Filter by location
  if (!is.null(location_list)) {
    available_locations <- unique(dataset$Location)
    missing_locations <- setdiff(location_list, available_locations)
    if (length(missing_locations) > 0) {
      message(sprintf(
        "Note: %d location(s) not found in data: %s",
        length(missing_locations),
        paste(missing_locations, collapse = ", ")
      ))
    }
    dataset <- dataset[dataset$Location %in% location_list, ]

    # Early return if no locations match
    if (nrow(dataset) == 0) {
      warning("No observations match the specified location filter")
      return(.create_empty_summary_table(species_list))
    }
  }

  # Filter by species
  if (!is.null(species_list)) {
    available_species <- unique(dataset$Species)
    missing_species <- setdiff(species_list, available_species)
    if (length(missing_species) > 0) {
      message(sprintf(
        "Note: %d species not found in data: %s",
        length(missing_species),
        paste(missing_species, collapse = ", ")
      ))
    }
    dataset <- dataset[dataset$Species %in% species_list, ]

    # Early return if no species match
    if (nrow(dataset) == 0) {
      warning("No observations match the specified species filter")
      return(.create_empty_summary_table(species_list))
    }
  }

  return(dataset)
}

#' Build summary table from filtered dataset
#' @keywords internal
.build_summary_table <- function(dataset, species_list, location_list, sort = FALSE) {
  # Handle empty dataset
  if (nrow(dataset) == 0) {
    return(dataset)
  }

  # Create count table
  count_table <- table(dataset$Location, dataset$Species)
  summary_df <- as.data.frame.matrix(count_table)
  summary_df$Location <- rownames(summary_df)
  rownames(summary_df) <- NULL

  # Move Location to first column
  summary_df <- summary_df[, c("Location", setdiff(names(summary_df), "Location")), drop = FALSE]

  # Determine final locations and species
  if (!is.null(location_list)) {
    final_locations <- location_list
  } else if (sort) {
    final_locations <- sort(unique(summary_df$Location))
  } else {
    final_locations <- unique(summary_df$Location)
  }

  if (!is.null(species_list)) {
    final_species <- species_list
  } else if (sort) {
    final_species <- sort(setdiff(names(summary_df), "Location"))
  } else {
    final_species <- setdiff(names(summary_df), "Location")
  }

  # Build complete table with all requested locations and species
  complete_table <- data.frame(Location = final_locations, stringsAsFactors = FALSE)

  for (sp in final_species) {
    if (sp %in% names(summary_df)) {
      idx <- match(complete_table$Location, summary_df$Location)
      complete_table[[sp]] <- ifelse(is.na(idx), 0L, summary_df[[sp]][idx])
      complete_table[[sp]][is.na(complete_table[[sp]])] <- 0L
    } else {
      complete_table[[sp]] <- 0L
    }
  }

  return(complete_table)
}

#' Add totals row to summary table
#' @keywords internal
.add_totals_row <- function(summary_table) {
  if (nrow(summary_table) == 0) {
    return(summary_table)
  }

  totals <- colSums(summary_table[, -1, drop = FALSE], na.rm = TRUE)
  totals_row <- data.frame(Location = "Totals", as.list(totals), stringsAsFactors = FALSE)
  summary_table <- rbind(summary_table, totals_row)

  return(summary_table)
}

#' Create empty summary table with requested structure
#' @keywords internal
.create_empty_summary_table <- function(species_list) {
  empty_df <- data.frame(Location = character(0))
  if (!is.null(species_list)) {
    for (sp in species_list) {
      empty_df[[sp]] <- integer(0)
    }
  }
  return(empty_df)
}
