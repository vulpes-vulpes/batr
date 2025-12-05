#' Plot Timing of First Observations Relative to Sunset
#'
#' Creates a plot showing the first nightly observation of a species relative
#' to local sunset at each site. Useful for phenology and emergence timing analysis.
#'
#' @family Timing Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data.
#' @param species Character. Four-letter species code (e.g., "Epfu").
#' @param timezone Character. Timezone for calculating sunset times in TZ format
#'   (e.g., "America/New_York", "America/Toronto"). See \code{OlsonNames()}.
#' @param location_list Character vector. Location names to include in the plot.
#'   If \code{NULL} (default), includes all locations with data for this species.
#' @param monitoring_start Character. Monitoring start date (e.g., "2019-01-01").
#'   If \code{NULL}, uses the earliest date in the dataset.
#' @param monitoring_end Character. Monitoring end date (e.g., "2019-12-31").
#'   If \code{NULL}, uses the latest date in the dataset.
#' @param gaps Logical. If \code{TRUE}, adds grey blocks indicating periods when
#'   recorders were inactive. Defaults to \code{FALSE}.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 15.9.
#' @param height Numeric. Plot height in cm. Default is 8.43.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#' @param date_label Character. Date format for x-axis labels. Default is "%b".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot for all locations
#' first_observations_plot("data.RData",
#'   species = "Epfu",
#'   timezone = "America/Toronto"
#' )
#'
#' # Plot for specific locations
#' first_observations_plot("data.RData",
#'   species = "Mylu",
#'   timezone = "America/New_York",
#'   location_list = c("Site1", "Site2")
#' )
#' }
#' @export
first_observations_plot <- function(data_path,
                                    species,
                                    timezone,
                                    location_list = NULL,
                                    monitoring_start = NULL,
                                    monitoring_end = NULL,
                                    gaps = FALSE,
                                    save_path = NULL,
                                    width = 15.9,
                                    height = 8.43,
                                    text_size = 10,
                                    date_label = "%b") {
  # Validate timezone
  .validate_timezone(timezone)

  # Load and filter data
  dataset <- .load_plot_data(data_path, species_list = species, location_list = location_list)

  # Convert to data.table and filter to evening observations only
  dataset <- data.table::as.data.table(dataset)
  dataset <- dataset[, .(Night, Species, Location, Latitude, Longitude, Timestamp)]
  dataset[, Time_PM := data.table::fifelse(lubridate::hour(Timestamp) < 12, NA_real_, as.numeric(Timestamp))]
  dataset <- dataset[!is.na(Time_PM)]
  dataset[, Time_PM := as.POSIXct(Time_PM, origin = "1970-01-01")]

  # Prepare for suncalc
  data.table::setnames(dataset,
    old = c("Night", "Latitude", "Longitude"),
    new = c("date", "lat", "lon")
  )

  # Get first observation per night per location using data.table aggregation
  dataset <- dataset[, .(Time_PM = min(Time_PM), lat = lat[1], lon = lon[1]),
    by = .(date, Location)
  ]

  # Calculate monitoring period
  if (is.null(monitoring_start)) {
    monitoring_start <- min(dataset$date)
  }
  if (is.null(monitoring_end)) {
    monitoring_end <- max(dataset$date)
  }

  # Store location coordinates
  locations <- dataset[!duplicated(dataset$Location), c("Location", "lat", "lon")]

  # Pad dataset to include all dates in monitoring period
  dataset <- padr::pad(dataset,
    by = "date",
    start_val = as.Date(monitoring_start),
    end_val = as.Date(monitoring_end),
    group = "Location"
  )

  # Merge coordinates back
  dataset <- merge(dataset, locations, by = "Location", all.x = TRUE, suffixes = c("", ".loc"))
  dataset$lat <- ifelse(is.na(dataset$lat), dataset$lat.loc, dataset$lat)
  dataset$lon <- ifelse(is.na(dataset$lon), dataset$lon.loc, dataset$lon)
  dataset$lat.loc <- NULL
  dataset$lon.loc <- NULL

  # Calculate sunset times
  dataset$sunset <- suncalc::getSunlightTimes(data = dataset, keep = "sunset", tz = timezone)$sunset

  # Extract time of day (remove date component)
  dataset$sunset_time <- as.POSIXct(format(dataset$sunset, format = "%H:%M:%S"), format = "%H:%M:%S")
  dataset$ob_time <- as.POSIXct(format(dataset$Time_PM, format = "%H:%M:%S"), format = "%H:%M:%S")
  dataset$date <- as.Date(dataset$date)

  # Load and process gap data if requested
  gap_data <- NULL
  if (gaps) {
    tryCatch(
      {
        active_dates <- .load_gap_data(data_path)

        if (!is.null(active_dates) && nrow(active_dates) > 0) {
          gap_data <- .plot_gap_calculator(active_dates)

          # Filter by locations if specified
          if (!is.null(location_list)) {
            gap_data <- gap_data[gap_data$Location %in% location_list, ]
          }

          # Filter gaps to monitoring period
          if (!is.null(gap_data) && nrow(gap_data) > 0) {
            gap_data <- gap_data[gap_data$xmax >= as.Date(monitoring_start) &
              gap_data$xmin <= as.Date(monitoring_end), ]
          }

          # Set y-axis limits for gaps if data exists
          if (!is.null(gap_data) && nrow(gap_data) > 0) {
            # Calculate limits based on actual data with fallback
            y_max <- if (any(!is.na(dataset$ob_time))) {
              max(dataset$ob_time, na.rm = TRUE)
            } else {
              as.POSIXct("23:59:59", format = "%H:%M:%S")
            }

            y_min <- if (any(!is.na(dataset$sunset_time))) {
              min(dataset$sunset_time, na.rm = TRUE)
            } else {
              as.POSIXct("18:00:00", format = "%H:%M:%S")
            }

            gap_data$ymax <- y_max
            gap_data$ymin <- y_min
          }
        } else {
          warning("No gap data found in ", data_path, ". Skipping gap overlay.")
        }
      },
      error = function(e) {
        warning("Failed to load gap data: ", e$message, ". Skipping gap overlay.")
      }
    )
  }

  # Create plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dataset,
      mapping = ggplot2::aes(x = date, y = sunset_time, group = Location)
    ) +
    ggplot2::geom_point(
      data = dataset,
      mapping = ggplot2::aes(x = date, y = ob_time)
    ) +
    ggplot2::scale_x_date(
      limits = c(as.Date(monitoring_start), as.Date(monitoring_end)),
      breaks = scales::pretty_breaks(),
      date_breaks = "1 month",
      date_labels = date_label
    ) +
    ggplot2::ylab("Time") +
    ggplot2::facet_wrap(~Location, ncol = 2, scales = "fixed", strip.position = "top") +
    .batr_theme(text_size)

  # Add gaps if requested
  if (!is.null(gap_data) && nrow(gap_data) > 0) {
    plot <- plot +
      ggplot2::geom_rect(
        data = gap_data,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = 0.9),
        show.legend = FALSE
      )
  }

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}

# ============================================================================
# Helper Functions (shared with plot_activity.R)
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
