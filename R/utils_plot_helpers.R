#' Adaptive date breaks and label cleaning helpers
#' @keywords internal
.adaptive_date_breaks <- function(min_date, max_date, date_label = "%b") {
  span_days <- as.integer(max_date - min_date)

  breaks <- if (span_days <= 14) {
    seq(min_date, max_date, by = "2 days")
  } else if (span_days <= 90) {
    seq(min_date, max_date, by = "1 week")
  } else if (span_days <= 540) {
    seq(min_date, max_date, by = "1 month")
  } else if (span_days <= 1095) {
    seq(min_date, max_date, by = "3 months")
  } else {
    seq(min_date, max_date, by = "6 months")
  }

  if (length(breaks) == 0) {
    breaks <- c(min_date, max_date)
  }

  label_format <- if (identical(date_label, "%b")) {
    if (span_days <= 90) {
      "%d %b"
    } else {
      "%b %Y"
    }
  } else {
    date_label
  }

  list(
    breaks = breaks,
    labels = scales::label_date(label_format)
  )
}

#' Clean location labels for display
#' @keywords internal
.clean_location_label <- function(x) {
  gsub("_", " ", x)
}

#' Build species label mapping for plots
#' @keywords internal
.species_label_map <- function(species_codes, species_names = NULL) {
  species_codes <- unique(as.character(species_codes))
  if (length(species_codes) == 0) {
    return(setNames(character(0), character(0)))
  }

  if (is.null(species_names)) {
    spec_labels <- species_codes
  } else {
    spec_labels <- species_names[species_codes]
    spec_labels[is.na(spec_labels)] <- species_codes[is.na(spec_labels)]
  }

  names(spec_labels) <- species_codes
  spec_labels
}

#' Create a labeller for species facets
#' @keywords internal
.species_labeller <- function(species_codes, species_names = NULL) {
  spec_labels <- .species_label_map(species_codes, species_names)
  function(x) {
    out <- spec_labels[x]
    out[is.na(out)] <- x[is.na(out)]
    unname(out)
  }
}

#' Load plot data from RData file
#'
#' Loads observation data from an RData file with optional filtering by species
#' and location.
#'
#' @param data_path Character. Path to an RData file with observations data.
#' @param species_list Character vector. Species codes to filter by (optional).
#' @param location_list Character vector. Location names to filter by (optional).
#'
#' @return Data frame of observations, optionally filtered.
#'
#' @keywords internal
.load_plot_data <- function(data_path, species_list = NULL, location_list = NULL) {
  .validate_rdata_path(data_path)

  load(data_path)

  if (!exists("observations")) {
    stop("No 'observations' object found in ", data_path)
  }

  # Remove NA values
  observations <- observations[!is.na(observations$Species) & !is.na(observations$Location), ]

  if (nrow(observations) == 0) {
    stop("No valid observations found in dataset")
  }

  # Treat "all" as NULL
  if (identical(species_list, "all")) {
    species_list <- NULL
  }
  if (identical(location_list, "all")) {
    location_list <- NULL
  }

  # Filter by species
  if (!is.null(species_list)) {
    observations <- observations[observations$Species %in% species_list, ]
    if (nrow(observations) == 0) {
      stop("No observations found for species: ", paste(species_list, collapse = ", "))
    }
  }

  # Filter by location
  if (!is.null(location_list)) {
    observations <- observations[observations$Location %in% location_list, ]
    if (nrow(observations) == 0) {
      stop("No observations found for locations: ", paste(location_list, collapse = ", "))
    }
  }

  return(observations)
}

#' Load gap data from RData file
#'
#' Loads active_dates (recorder uptime) data from an RData file with optional
#' filtering by location.
#'
#' @param data_path Character. Path to an RData file with active_dates data.
#' @param location_list Character vector. Location names to filter by (optional).
#'
#' @return Data frame of active_dates, optionally filtered.
#'
#' @keywords internal
.load_gap_data <- function(data_path, location_list = NULL) {
  .validate_rdata_path(data_path)

  load(data_path)

  if (!exists("active_dates")) {
    stop("Log file data missing. Please run import_logs() first.")
  }

  # Filter by location if specified
  if (!is.null(location_list)) {
    active_dates <- active_dates[active_dates$Location %in% location_list, ]
    if (nrow(active_dates) == 0) {
      stop("No log data found for specified locations: ", paste(location_list, collapse = ", "))
    }
  }

  return(active_dates)
}

#' Consistent theme for batr plots
#'
#' Provides a consistent ggplot2 theme for all batr visualizations.
#'
#' @param text_size Numeric. Base text size in points (default: 10).
#'
#' @return A ggplot2 theme object.
#'
#' @keywords internal
.batr_theme <- function(text_size = 10) {
  ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(size = text_size)
    )
}

#' Calculate Gap Rectangles for Plotting
#'
#' Identifies periods when recorders were inactive based on log file data
#' and converts them to rectangles for ggplot2 geom_rect.
#'
#' @param active_dates Data frame with Date, Location, Log_Count columns.
#'
#' @return Data frame with xmin, xmax, ymin, ymax, Location columns for geom_rect.
#'
#' @keywords internal
.plot_gap_calculator <- function(active_dates) {
  # Ensure standard data.frame to avoid data.table method issues
  active_dates <- as.data.frame(active_dates)

  failed_dates <- active_dates[which(active_dates$Log_Count == 0), ]
  failed_dates <- failed_dates[order(failed_dates$Location), ]
  failed_dates <- failed_dates[!duplicated(failed_dates), ]
  failed_dates$Gaps <- 0
  failed_dates[1, 4] <- 1
  row <- 2
  maximum_row <- length(failed_dates$Gaps) + 1

  while (row < maximum_row) {
    rowdown <- row - 1
    if (failed_dates[row, 1] - failed_dates[rowdown, 1] == 1) {
      failed_dates[row, 4] <- failed_dates[rowdown, 4]
    } else {
      failed_dates[row, 4] <- failed_dates[rowdown, 4] + 1
    }
    row <- row + 1
  }

  maximum_fail <- max(failed_dates$Gaps)
  gap_list <- failed_dates
  gap_list$Date <- NULL
  gap_list$Log_Count <- NULL
  gap_list <- unique(gap_list)
  fail <- 1
  gap_list$xmin <- 0

  while (fail < (maximum_fail + 1)) {
    gap_list[fail, 3] <- min(failed_dates[failed_dates$Gaps == fail, 1])
    fail <- fail + 1
  }
  gap_list$xmin <- as.Date(gap_list$xmin, origin = "1970-01-01")

  fail <- 1
  gap_list$xmax <- 0
  while (fail < (maximum_fail + 1)) {
    gap_list[fail, 4] <- max(failed_dates[failed_dates$Gaps == fail, 1])
    fail <- fail + 1
  }
  gap_list$xmax <- as.Date(gap_list$xmax, origin = "1970-01-01")
  gap_list$ymax <- Inf
  gap_list$ymin <- 0
  gap_list$Gaps <- NULL

  return(gap_list)
}
