#' Manual Vetting Summary
#'
#' Summarizes manual vetting progress and accuracy by analyzing files marked
#' with "Y" in the \code{User.Manually.Vetted} field. Reports the percentage of files
#' vetted and the agreement rate between automatic and manual species identification.
#'
#' @family Summary Functions
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data with manual vetting information.
#' @param stratify_by Character. Controls table structure. One of:
#'   \itemize{
#'     \item \code{"species"} (default): One row per species across all sites
#'     \item \code{"species_and_site"}: One row per species-site combination
#'   }
#' @param species_list Character vector. Species codes to include in the summary
#'   (based on Species.Auto.ID). If \code{NULL} (default), includes all species
#'   found in Species.Auto.ID.
#' @param location_list Character vector. Location names to include in the summary.
#'   If \code{NULL} (default), includes all locations. Only used when
#'   \code{stratify_by = "species_and_site"}.
#'
#' @return A data frame with vetting statistics. Columns depend on \code{stratify_by}:
#'
#'   When \code{stratify_by = "species"}:
#'   \describe{
#'     \item{Species}{Species code from Species.Auto.ID}
#'     \item{Total_Files}{Total number of files for this species (based on Auto.ID)}
#'     \item{Vetted_Count}{Number of files with "Y" in User.Manually.Vetted}
#'     \item{Match_Count}{Number where Species.Auto.ID equals Species.Manual.ID}
#'     \item{Percent_Vetted}{Percentage of files vetted (0-100, 2 decimal places)}
#'     \item{Percent_Match}{Percentage matching among vetted files (0-100, 2 decimal places)}
#'   }
#'
#'   When \code{stratify_by = "species_and_site"}:
#'   \describe{
#'     \item{Species}{Species code from Species.Auto.ID}
#'     \item{Location}{Site/location name}
#'     \item{Total_Files}{Total files for this species-site combination (based on Auto.ID)}
#'     \item{Vetted_Count}{Number vetted}
#'     \item{Match_Count}{Number matching}
#'     \item{Percent_Vetted}{Percentage vetted}
#'     \item{Percent_Match}{Percentage matching among vetted files}
#'   }
#'
#'   Results are sorted alphabetically by Species (then Location if stratified).
#'   A "Totals" row is appended at the bottom.
#'
#' @details
#' The function summarizes vetting progress based on \code{Species.Auto.ID} (the automatic
#' species identification). Files are grouped by their Auto.ID, not by manual corrections.
#' For example, if a file has Auto.ID = "Myle" and Manual.ID = "Mysp", it counts as a
#' "Myle" file that has been vetted but does not match. Only species that appear in
#' Species.Auto.ID will appear in the summary table.
#'
#' The function validates that the \code{User.Manually.Vetted} field exists and contains
#' at least some "Y" values. Only files with \code{User.Manually.Vetted == "Y"} are
#' included in vetting and matching calculations.
#'
#' Observations with NA values in \code{Species} or \code{Location} are automatically
#' removed before calculations, ensuring consistency with \code{summary_table()}. A
#' message will report how many observations were removed, if any.
#'
#' When using \code{stratify_by = "species_and_site"}, the function requires either
#' a \code{Location_label} or \code{Location} column. If only \code{Location} exists,
#' \code{Location_label} will be automatically created by replacing underscores with
#' spaces for cleaner display. If you don't have any location data, use
#' \code{stratify_by = "species"}.
#'
#' Match percentage represents agreement between automatic and manual species ID
#' among vetted files only. Files with NA in \code{Species.Manual.ID} count as
#' "not matching" in both the count and percentage calculations. For example,
#' if you have 23 vetted files where 9 have matching Auto.ID and Manual.ID, and
#' 14 have NA in Manual.ID, the result will be: Match_Count = 9, Percent_Match = 39.13
#' (9 out of 23). If \code{Species.Auto.ID} is NA for a vetted file (edge case),
#' a warning is issued and those files are excluded from match calculations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Summary by species across all sites
#' vet_stats <- manual_vet_summary("data/project.RData")
#'
#' # Summary by species and site
#' vet_stats <- manual_vet_summary(
#'     "data/project.RData",
#'     stratify_by = "species_and_site"
#' )
#'
#' # Filter to specific species
#' vet_stats <- manual_vet_summary(
#'     "data/project.RData",
#'     species_list = c("Epfu", "Labo", "Mylu")
#' )
#'
#' # Filter to specific species and sites
#' vet_stats <- manual_vet_summary(
#'     "data/project.RData",
#'     stratify_by = "species_and_site",
#'     species_list = c("Epfu", "Labo"),
#'     location_list = c("Site1", "Site2")
#' )
#' }
manual_vet_summary <- function(data_path,
                               stratify_by = c("species", "species_and_site"),
                               species_list = NULL,
                               location_list = NULL) {
    # Validate stratify_by argument
    stratify_by <- match.arg(stratify_by)

    # Validate and load data
    dataset <- .validate_vet_summary_data(data_path, species_list, location_list, stratify_by)

    # Clean NA values (consistent with summary_table)
    dataset <- .clean_vet_summary_dataset(dataset)

    # Check User.Manually.Vetted field
    .check_manually_vetted_field(dataset)

    # Check for NA in Species.Auto.ID among vetted files
    .check_species_auto_id(dataset)

    # Filter dataset
    dataset <- .filter_vetted_dataset(dataset, species_list, location_list)

    # Calculate statistics based on stratification
    if (stratify_by == "species") {
        result_table <- .calculate_species_vet_stats(dataset)
    } else {
        # Ensure Location_label exists for stratification
        dataset <- .ensure_location_label(dataset)
        result_table <- .calculate_species_site_vet_stats(dataset)
    }

    # Add totals row
    result_table <- .add_vet_totals_row(result_table, stratify_by)

    return(result_table)
}

# Helper Functions ------------------------------------------------------------

#' Validate data for vetting summary
#' @keywords internal
.validate_vet_summary_data <- function(data_path, species_list, location_list, stratify_by) {
    .validate_rdata_path(data_path)

    if (!is.null(species_list) && (!is.character(species_list) || length(species_list) == 0)) {
        stop("species_list must be a character vector with at least one element")
    }

    if (!is.null(location_list) && (!is.character(location_list) || length(location_list) == 0)) {
        stop("location_list must be a character vector with at least one element")
    }

    # Load data
    env <- new.env()
    load(data_path, envir = env)

    if (!"observations" %in% ls(env)) {
        stop("RData file must contain an 'observations' object")
    }

    dataset <- env$observations

    # Check required columns (Location_label is optional, checked later if needed)
    required_cols <- c("Species", "Species.Auto.ID", "Species.Manual.ID")
    missing_cols <- setdiff(required_cols, colnames(dataset))

    if (length(missing_cols) > 0) {
        stop("Dataset is missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    return(dataset)
}

#' Clean dataset by removing NA values in Species and Location
#' @keywords internal
.clean_vet_summary_dataset <- function(dataset) {
    na_species <- is.na(dataset$Species)

    # Only check Location if it exists (it's optional for species stratification)
    has_location <- "Location" %in% colnames(dataset)
    na_location <- if (has_location) is.na(dataset$Location) else rep(FALSE, nrow(dataset))

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

    # Remove only rows with NA in Species or Location (if Location exists)
    dataset <- dataset[!na_species & !na_location, ]

    if (nrow(dataset) == 0) {
        stop("No valid observations found after removing NA values")
    }

    return(dataset)
}

#' Check User.Manually.Vetted field exists and has data
#' @keywords internal
.check_manually_vetted_field <- function(dataset) {
    if (!"User.Manually.Vetted" %in% colnames(dataset)) {
        stop(
            "Column 'User.Manually.Vetted' not found in dataset.\n",
            "This function requires a 'User.Manually.Vetted' field to identify vetted files."
        )
    }

    vetted_count <- sum(dataset$User.Manually.Vetted == "Y", na.rm = TRUE)

    if (vetted_count == 0) {
        warning(
            "No files marked with 'Y' in User.Manually.Vetted field.\n",
            "All vetting metrics will be zero or NA."
        )
    }

    invisible(NULL)
}

#' Check for NA in Species.Auto.ID among vetted files
#' @keywords internal
.check_species_auto_id <- function(dataset) {
    vetted_files <- dataset$User.Manually.Vetted == "Y"
    vetted_files[is.na(vetted_files)] <- FALSE

    if (any(vetted_files)) {
        na_auto_id <- is.na(dataset$Species.Auto.ID[vetted_files])

        if (any(na_auto_id)) {
            n_na <- sum(na_auto_id)
            warning(
                n_na, " vetted file(s) have NA in Species.Auto.ID (edge case).\n",
                "These files will be excluded from match percentage calculations."
            )
        }
    }

    invisible(NULL)
}

#' Ensure Location_label column exists
#' @keywords internal
.ensure_location_label <- function(dataset) {
    if ("Location_label" %in% colnames(dataset)) {
        return(dataset)
    }

    # Try to create from Location column
    if ("Location" %in% colnames(dataset)) {
        message("Creating Location_label from Location column (replacing underscores with spaces)")
        dataset$Location_label <- .clean_location_label(dataset$Location)
        return(dataset)
    }

    # Neither Location_label nor Location exists
    stop(
        "stratify_by = 'species_and_site' requires either 'Location_label' or 'Location' column.\n",
        "Neither column found in dataset. Use stratify_by = 'species' if location data is not available."
    )
}

#' Filter dataset by species and location
#' @keywords internal
.filter_vetted_dataset <- function(dataset, species_list, location_list) {
    if (!is.null(species_list)) {
        dataset <- dataset[dataset$Species.Auto.ID %in% species_list, ]
    }

    if (!is.null(location_list)) {
        # Check for Location_label or Location
        has_location_label <- "Location_label" %in% colnames(dataset)
        has_location <- "Location" %in% colnames(dataset)

        if (!has_location_label && !has_location) {
            warning(
                "location_list specified but neither Location_label nor Location column found in dataset.\n",
                "Ignoring location_list filter."
            )
        } else {
            # Filter by Location_label if it exists, otherwise by Location
            location_col <- if (has_location_label) "Location_label" else "Location"
            dataset <- dataset[dataset[[location_col]] %in% location_list, ]
        }
    }

    if (nrow(dataset) == 0) {
        stop("No data remaining after applying filters")
    }

    return(dataset)
}

#' Calculate vetting statistics by species
#' @keywords internal
.calculate_species_vet_stats <- function(dataset) {
    # Get unique species from Species.Auto.ID sorted alphabetically
    species_codes <- sort(unique(dataset$Species.Auto.ID[!is.na(dataset$Species.Auto.ID)]))

    results <- lapply(species_codes, function(sp) {
        sp_data <- dataset[dataset$Species.Auto.ID == sp & !is.na(dataset$Species.Auto.ID), ]

        total_files <- nrow(sp_data)

        # Count vetted files (User.Manually.Vetted == "Y")
        vetted_mask <- sp_data$User.Manually.Vetted == "Y"
        vetted_mask[is.na(vetted_mask)] <- FALSE
        vetted_count <- sum(vetted_mask)

        percent_vetted <- if (total_files > 0) {
            round((vetted_count / total_files) * 100, 2)
        } else {
            0
        }

        # Calculate matches among vetted files
        if (vetted_count > 0) {
            vetted_data <- sp_data[vetted_mask, ]

            # Only consider files where Auto.ID is not NA (edge case check)
            # Files with NA in Manual.ID count as "not matching"
            valid_for_matching <- !is.na(vetted_data$Species.Auto.ID)

            if (sum(valid_for_matching) > 0) {
                valid_data <- vetted_data[valid_for_matching, ]

                # Count matches (NA in Manual.ID counts as not matching)
                matches <- !is.na(valid_data$Species.Manual.ID) &
                    (valid_data$Species.Auto.ID == valid_data$Species.Manual.ID)
                match_count <- sum(matches, na.rm = TRUE)
                percent_match <- round((match_count / sum(valid_for_matching)) * 100, 2)
            } else {
                match_count <- 0
                percent_match <- NA_real_
            }
        } else {
            match_count <- 0
            percent_match <- NA_real_
        }

        data.frame(
            Species = sp,
            Total_Files = total_files,
            Vetted_Count = vetted_count,
            Match_Count = match_count,
            Percent_Vetted = percent_vetted,
            Percent_Match = percent_match,
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, results)
}

#' Calculate vetting statistics by species and site
#' @keywords internal
.calculate_species_site_vet_stats <- function(dataset) {
    # Get unique species-location combinations from Species.Auto.ID, sorted
    # Filter out NA values in Species.Auto.ID
    valid_data <- dataset[!is.na(dataset$Species.Auto.ID), ]
    combos <- unique(valid_data[, c("Species.Auto.ID", "Location_label")])
    names(combos)[1] <- "Species"
    combos <- combos[order(combos$Species, combos$Location_label), ]

    results <- lapply(seq_len(nrow(combos)), function(i) {
        sp <- combos$Species[i]
        loc <- combos$Location_label[i]

        sp_loc_data <- dataset[dataset$Species.Auto.ID == sp & dataset$Location_label == loc & !is.na(dataset$Species.Auto.ID), ]

        total_files <- nrow(sp_loc_data)

        # Count vetted files
        vetted_mask <- sp_loc_data$User.Manually.Vetted == "Y"
        vetted_mask[is.na(vetted_mask)] <- FALSE
        vetted_count <- sum(vetted_mask)

        percent_vetted <- if (total_files > 0) {
            round((vetted_count / total_files) * 100, 2)
        } else {
            0
        }

        # Calculate matches among vetted files
        if (vetted_count > 0) {
            vetted_data <- sp_loc_data[vetted_mask, ]

            # Only consider files where Auto.ID is not NA (edge case check)
            # Files with NA in Manual.ID count as "not matching"
            valid_for_matching <- !is.na(vetted_data$Species.Auto.ID)

            if (sum(valid_for_matching) > 0) {
                valid_data <- vetted_data[valid_for_matching, ]

                # Count matches (NA in Manual.ID counts as not matching)
                matches <- !is.na(valid_data$Species.Manual.ID) &
                    (valid_data$Species.Auto.ID == valid_data$Species.Manual.ID)
                match_count <- sum(matches, na.rm = TRUE)
                percent_match <- round((match_count / sum(valid_for_matching)) * 100, 2)
            } else {
                match_count <- 0
                percent_match <- NA_real_
            }
        } else {
            match_count <- 0
            percent_match <- NA_real_
        }

        data.frame(
            Species = sp,
            Location = loc,
            Total_Files = total_files,
            Vetted_Count = vetted_count,
            Match_Count = match_count,
            Percent_Vetted = percent_vetted,
            Percent_Match = percent_match,
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, results)
}

#' Add totals row to vetting summary
#' @keywords internal
.add_vet_totals_row <- function(summary_table, stratify_by) {
    if (nrow(summary_table) == 0) {
        return(summary_table)
    }

    # Calculate totals
    total_files <- sum(summary_table$Total_Files, na.rm = TRUE)
    vetted_count <- sum(summary_table$Vetted_Count, na.rm = TRUE)
    match_count <- sum(summary_table$Match_Count, na.rm = TRUE)

    percent_vetted <- if (total_files > 0) {
        round((vetted_count / total_files) * 100, 2)
    } else {
        0
    }

    percent_match <- if (vetted_count > 0) {
        round((match_count / vetted_count) * 100, 2)
    } else {
        NA_real_
    }

    # Create totals row
    if (stratify_by == "species") {
        totals_row <- data.frame(
            Species = "Totals",
            Total_Files = total_files,
            Vetted_Count = vetted_count,
            Match_Count = match_count,
            Percent_Vetted = percent_vetted,
            Percent_Match = percent_match,
            stringsAsFactors = FALSE
        )
    } else {
        totals_row <- data.frame(
            Species = "Totals",
            Location = "",
            Total_Files = total_files,
            Vetted_Count = vetted_count,
            Match_Count = match_count,
            Percent_Vetted = percent_vetted,
            Percent_Match = percent_match,
            stringsAsFactors = FALSE
        )
    }

    summary_table <- rbind(summary_table, totals_row)
    rownames(summary_table) <- NULL

    return(summary_table)
}
