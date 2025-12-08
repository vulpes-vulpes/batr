# ============================================================================
# Activity Plotting Functions
# ============================================================================
# Consolidated from plot_nightly_activity.R and update_Plots.R

#' Plot Activity for a Species at Each Site
#'
#' Creates a plot of nightly activity for a chosen species at all sites (or
#' a subset of locations if specified).
#'
#' @family Activity Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data.
#' @param species Character. Four-letter species code to plot (e.g., "Epfu").
#' @param location_list Character vector. Location names to include in the plot.
#'   If \code{NULL} (default), includes all locations with data for this species.
#' @param monitoring_start Character. Monitoring start date (e.g., "2019-01-01").
#'   If \code{NULL}, uses the earliest date in the dataset.
#' @param monitoring_end Character. Monitoring end date (e.g., "2019-12-31").
#'   If \code{NULL}, uses the latest date in the dataset.
#' @param gaps Logical. If \code{TRUE}, adds grey blocks indicating periods when
#'   recorders were inactive (requires log file data loaded via \code{import_logs}).
#'   Defaults to \code{FALSE}.
#' @param y_scale Character. Y-axis scaling: "free_y" (default) for independent
#'   scales per location, or "fixed" for matched scales.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 15.9.
#' @param height Numeric. Plot height in cm. Default is 8.43.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#' @param date_label Character. Date format for x-axis labels. Default is "%b"
#'   (abbreviated month name).
#' @param date_breaks Character. Optional break specification (e.g., "1 month").
#'   If \code{NULL} (default), adaptive breaks are used based on timespan.
#' @param title Logical. If \code{TRUE}, adds plot title. Default is \code{FALSE}.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot for all locations
#' species_daily_site_plot("data.RData", species = "Mylu")
#'
#' # Plot for specific locations
#' species_daily_site_plot("data.RData",
#'   species = "Epfu",
#'   location_list = c("Site1", "Site2")
#' )
#'
#' # With monitoring gaps and custom date range
#' species_daily_site_plot("data.RData",
#'   species = "Lano",
#'   monitoring_start = "2023-05-01",
#'   monitoring_end = "2023-10-31",
#'   gaps = TRUE
#' )
#' }
#' @export
species_daily_site_plot <- function(data_path,
                                    species,
                                    location_list = NULL,
                                    monitoring_start = NULL,
                                    monitoring_end = NULL,
                                    gaps = FALSE,
                                    y_scale = "free_y",
                                    save_path = NULL,
                                    width = 15.9,
                                    height = 8.43,
                                    text_size = 10,
                                    date_label = "%b",
                                    date_breaks = NULL,
                                    title = FALSE) {
  # Load and filter data
  dataset <- .load_plot_data(data_path, species_list = species, location_list = location_list)

  # Load gap data if requested
  active_dates <- NULL
  if (gaps) {
    active_dates <- .load_gap_data(data_path)
  }

  # Prepare activity data
  counts <- .prepare_activity_data(dataset, species, location_list)

  # Calculate monitoring period
  period <- .calculate_monitoring_period(
    counts, monitoring_start, monitoring_end, active_dates
  )

  # Build x-axis scale (adaptive breaks unless user supplies date_breaks)
  adaptive_breaks <- if (is.null(date_breaks)) {
    .adaptive_date_breaks(
      min_date = as.Date(period$start),
      max_date = as.Date(period$end),
      date_label = date_label
    )
  } else {
    NULL
  }

  x_scale <- if (is.null(date_breaks)) {
    ggplot2::scale_x_date(
      limits = c(as.Date(period$start), as.Date(period$end)),
      breaks = adaptive_breaks$breaks,
      labels = adaptive_breaks$labels
    )
  } else {
    ggplot2::scale_x_date(
      limits = c(as.Date(period$start), as.Date(period$end)),
      date_breaks = date_breaks,
      labels = scales::label_date(date_label)
    )
  }

  # Create base plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = counts,
      mapping = ggplot2::aes(x = Night, y = Count),
      stat = "identity",
      fill = "black"
    ) +
    ggplot2::scale_y_continuous(name = "Observations per Night") +
    x_scale +
    ggplot2::facet_wrap(~Location, ncol = 1, scales = y_scale, strip.position = "top") +
    ggplot2::geom_hline(yintercept = 0) +
    .batr_theme(text_size)

  # Add gaps if requested
  if (gaps && !is.null(active_dates)) {
    plot <- .add_gap_overlay(plot, active_dates)
  }

  # Add title if requested
  if (title) {
    plot <- plot + ggplot2::ggtitle(paste("Total Activity of", species, "by Site"))
  }

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}

# ============================================================================
# Additional Activity Plotting Functions
# ============================================================================

#' Plot Monitoring Effort by Site
#'
#' Creates a plot showing recorder uptime at each site, highlighting periods
#' when recorders were inactive.
#'
#' @family Activity Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   log file data (must have \code{active_dates} object from \code{import_logs}).
#' @param location_list Character vector. Location names to include in the plot.
#'   If \code{NULL} (default), includes all locations with log data.
#' @param monitoring_start Character. Monitoring start date (e.g., "2019-01-01").
#'   If \code{NULL}, uses the earliest date in the log data.
#' @param monitoring_end Character. Monitoring end date (e.g., "2019-12-31").
#'   If \code{NULL}, uses the latest date in the log data.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 25.
#' @param height Numeric. Plot height in cm. Default is 20.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#' @param date_label Character. Date format for x-axis labels. Default is "%b".
#' @param date_breaks Character. Optional break specification (e.g., "1 month").
#'   If \code{NULL} (default), adaptive breaks are used based on timespan.
#' @param title Logical. If \code{TRUE} (default), adds plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot for all locations
#' monitoring_effort_plot("data.RData")
#'
#' # Plot for specific locations
#' monitoring_effort_plot("data.RData",
#'   location_list = c("Site1", "Site2")
#' )
#' }
#' @export
monitoring_effort_plot <- function(data_path,
                                   location_list = NULL,
                                   monitoring_start = NULL,
                                   monitoring_end = NULL,
                                   save_path = NULL,
                                   width = 25,
                                   height = 20,
                                   text_size = 10,
                                   date_label = "%b",
                                   date_breaks = NULL,
                                   title = TRUE) {
  # Load gap data
  active_dates <- .load_gap_data(data_path)

  # Filter by location if specified
  if (!is.null(location_list)) {
    active_dates <- active_dates[active_dates$Location %in% location_list, ]
    if (nrow(active_dates) == 0) {
      stop(
        "No log data found for specified locations: ",
        paste(location_list, collapse = ", ")
      )
    }
  }

  # Calculate gaps
  gap_list <- .plot_gap_calculator(active_dates)
  
    # Clean location labels for display
    gap_list$Location_label <- gsub("_", " ", gap_list$Location)

  # Filter gaps by location if specified
  if (!is.null(location_list)) {
    gap_list <- gap_list[gap_list$Location %in% location_list, ]
  }

  # Calculate monitoring period
  if (is.null(monitoring_start)) {
    monitoring_start <- min(gap_list$xmin, na.rm = TRUE)
  }
  if (is.null(monitoring_end)) {
    monitoring_end <- max(gap_list$xmax, na.rm = TRUE)
  }

  # Build x-axis scale (adaptive breaks unless user supplies date_breaks)
  adaptive_breaks <- if (is.null(date_breaks)) {
    .adaptive_date_breaks(
      min_date = as.Date(monitoring_start),
      max_date = as.Date(monitoring_end),
      date_label = date_label
    )
  } else {
    NULL
  }

  x_scale <- if (is.null(date_breaks)) {
    ggplot2::scale_x_date(
      limits = c(as.Date(monitoring_start), as.Date(monitoring_end)),
      breaks = adaptive_breaks$breaks,
      labels = adaptive_breaks$labels
    )
  } else {
    ggplot2::scale_x_date(
      limits = c(as.Date(monitoring_start), as.Date(monitoring_end)),
      date_breaks = date_breaks,
      labels = scales::label_date(date_label)
    )
  }

  # Create plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = gap_list,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, alpha = 0.9),
      show.legend = FALSE
    ) +
    x_scale +
      ggplot2::facet_wrap(~Location_label, ncol = 1, scales = "fixed", strip.position = "top") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      text = ggplot2::element_text(size = text_size)
    )

  if (title) {
    plot <- plot + ggplot2::ggtitle("Monitor Uptime by Site")
  }

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}

#' Monthly Activity Plot
#'
#' \code{monthly_activity_plot} creates a plot showing average nightly activity
#' by month and species.
#'
#' @family Activity Plots
#'
#' @param species_night_site Data frame with Night, Location, Species, Count columns.
#' @param monthly_active_nights Data frame with monthly survey effort.
#' @param exclude_species Character vector of species codes to exclude.
#' @param species_codes Character vector of species codes.
#' @param species_names Character vector of species names matching codes.
#'
#' @return A ggplot2 plot object.
#'
#' @export
monthly_activity_plot <- function(species_night_site, monthly_active_nights, exclude_species = NULL,
                                  species_codes = c("Epfu", "Labo", "Laci", "Lano", "Myle", "Mylu", "Myse", "Mysp", "Mysp_all", "Pesu"),
                                  species_names = c(
                                    "Big Brown Bat", "Eastern Red Bat", "Hoary Bat", "Silver-haired Bat",
                                    "Eastern Small-footed Myotis", "Little Brown Myotis", "Northern Myotis",
                                    "Unidentified Myotis", "Combined Myotis", "Tri-colored Bat"
                                  )) {
  monthly_species <- reshape2::dcast(
    species_night_site,
    lubridate::year(Night) + Location + Species ~ lubridate::month(Night),
    value.var = "Count",
    fun.aggregate = sum
  )
  colnames(monthly_species)[4:length(monthly_species)] <- paste("Observations", colnames(monthly_species)[4:length(monthly_species)], sep = "_")
  names(monthly_species)[names(monthly_species) == "lubridate::year(Night)"] <- "Year"
  colnames(monthly_active_nights)[3:(length(monthly_active_nights) - 1)] <- paste("SurveyEffort", colnames(monthly_active_nights)[3:(length(monthly_active_nights) - 1)], sep = "_")

  data <- merge(monthly_species, monthly_active_nights)
  species_start <- 4
  active_start <- 3

  for (month in month.name) {
    if (any(names(data) == paste("Observations_", as.integer(factor(month, levels = month.name)), sep = ""))) {
      data$mean <- data[, colnames(monthly_species)[species_start]] / data[, colnames(monthly_active_nights)[active_start]]
      names(data)[names(data) == "mean"] <- month
      species_start <- species_start + 1
      active_start <- active_start + 1
    }
  }

  data <- data[, -grep("Observation", colnames(data))]
  data <- data[, -grep("SurveyEffort", colnames(data))]
  data <- data[, -grep("Total_Nights", colnames(data))]

  if (!is.null(exclude_species)) {
    data <- data[!data$Species %in% exclude_species, ]
  }

  data_melt <- reshape2::melt(data, id = c("Year", "Location", "Species"))
  names(data_melt)[names(data_melt) == "variable"] <- "Month"
  names(data_melt)[names(data_melt) == "value"] <- "x"

  spec.labs <- species_names
  names(spec.labs) <- species_codes

  ggplot2::ggplot() +
    ggplot2::geom_bar(data = data_melt, mapping = ggplot2::aes(x = Month, y = x), stat = "identity") +
    ggplot2::facet_wrap(~Species,
      ncol = 1, scales = "free", strip.position = "top",
      labeller = ggplot2::labeller(Species = spec.labs)
    ) +
    ggplot2::scale_y_continuous(name = "Mean Nightly Observations") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0, face = "bold"),
      text = ggplot2::element_text(size = 25),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(face = "bold")
    )
}

# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Load and validate observation data for plotting
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

#' Load gap data from log files
#' @keywords internal
.load_gap_data <- function(data_path) {
  load(data_path)

  if (!exists("active_dates")) {
    stop("Log file data missing. Please run import_logs() first.")
  }

  return(active_dates)
}

#' Adaptive date breaks for varying monitoring windows
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

#' Prepare activity count data for plotting
#' @keywords internal
.prepare_activity_data <- function(dataset, species, location_list) {
  # Aggregate counts by night, species, location
  counts <- aggregate(
    dataset$Species,
    list(
      Night = dataset$Night,
      Species = dataset$Species,
      Location = dataset$Location
    ),
    FUN = length
  )
  names(counts)[names(counts) == "x"] <- "Count"

  return(counts)
}

#' Calculate monitoring period dates
#' @keywords internal
.calculate_monitoring_period <- function(data, monitoring_start, monitoring_end, active_dates) {
  if (is.null(monitoring_start)) {
    if (!is.null(active_dates)) {
      gap_list <- .plot_gap_calculator(active_dates)
      monitoring_start <- min(gap_list$xmin, na.rm = TRUE)
    } else {
      monitoring_start <- min(data$Night, na.rm = TRUE)
    }
  }

  if (is.null(monitoring_end)) {
    if (!is.null(active_dates)) {
      gap_list <- .plot_gap_calculator(active_dates)
      monitoring_end <- max(gap_list$xmax, na.rm = TRUE)
    } else {
      monitoring_end <- max(data$Night, na.rm = TRUE)
    }
  }

  list(start = monitoring_start, end = monitoring_end)
}

#' Add gap overlay to existing plot
#' @keywords internal
.add_gap_overlay <- function(plot, active_dates) {
  gap_list <- .plot_gap_calculator(active_dates)

  plot +
    ggplot2::geom_rect(
      data = gap_list,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = 0.9),
      show.legend = FALSE
    )
}

#' Consistent theme for batr plots
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
