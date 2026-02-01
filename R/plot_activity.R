# ============================================================================
# Activity Plotting Functions
# ============================================================================
# Consolidated from plot_nightly_activity.R and update_Plots.R

#' Plot Species Activity Faceted by Species
#'
#' Creates a plot of nightly activity for multiple species, with each species
#' shown in its own facet panel. Designed for single-site or aggregated data.
#'
#' @family Activity Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data.
#' @param species Character vector. Species codes to include in the plot.
#'   If \code{NULL}, includes all species in the dataset.
#' @param location_list Character vector. Location names to filter/include.
#'   If \code{NULL} (default), includes all locations.
#' @param species_names Named character vector for custom species labels. Names should be
#'   species codes from the data, values should be display names. If NULL (default),
#'   species codes are used as labels.
#' @param monitoring_start Character. Monitoring start date (e.g., "2019-01-01").
#'   If \code{NULL}, uses the earliest date in the dataset.
#' @param monitoring_end Character. Monitoring end date (e.g., "2019-12-31").
#'   If \code{NULL}, uses the latest date in the dataset.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 15.
#' @param height Numeric. Plot height in cm. Default is 20.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#' @param date_label Character. Date format for x-axis labels. Default is "\%b \%Y".
#' @param date_breaks Character. Break specification (e.g., "1 month").
#'   Default is "1 month".
#' @param facet_cols Integer. Number of facet columns. Default is 1.
#' @param y_scale Character. Y-axis scaling: "free_y" (default) for independent
#'   scales per species, or "fixed" for matched scales.
#' @param plot_title Character. Optional plot title.
#' @param plot_subtitle Character. Optional plot subtitle.
#' @param plot_caption Character. Optional plot caption.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot all species at a site
#' species_activity_facet_plot("data.RData",
#'   location_list = "Site1"
#' )
#'
#' # Plot specific species
#' species_activity_facet_plot("data.RData",
#'   species = c("Epfu", "Mylu", "Lano"),
#'   location_list = "Site1"
#' )
#' }
#' @export
species_activity_facet_plot <- function(data_path,
                                        species = NULL,
                                        location_list = NULL,
                                        monitoring_start = NULL,
                                        monitoring_end = NULL,
                                        species_names = NULL,
                                        save_path = NULL,
                                        width = 15,
                                        height = 20,
                                        text_size = 10,
                                        date_label = "%b %Y",
                                        date_breaks = "1 month",
                                        facet_cols = 1,
                                        y_scale = "free_y",
                                        plot_title = NULL,
                                        plot_subtitle = NULL,
                                        plot_caption = NULL) {
  # Load and filter data
  dataset <- .load_plot_data(data_path, species_list = species, location_list = location_list)

  # Filter by monitoring period if specified
  if (!is.null(monitoring_start)) {
    dataset <- dataset[dataset$Night >= as.Date(monitoring_start), ]
  }
  if (!is.null(monitoring_end)) {
    dataset <- dataset[dataset$Night <= as.Date(monitoring_end), ]
  }

  # Aggregate counts by Night and Species
  obs_plot <- dataset %>%
    dplyr::group_by(Night, Species) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  # Create species labeller
  species_labeller <- .species_labeller(unique(obs_plot$Species), species_names)

  # Create plot
  plot <- ggplot2::ggplot(obs_plot, ggplot2::aes(x = Night, y = Count)) +
    ggplot2::geom_bar(stat = "identity", fill = "black") +
    ggplot2::facet_wrap(~Species,
      ncol = facet_cols,
      scales = y_scale,
      strip.position = "top",
      labeller = ggplot2::labeller(Species = species_labeller)
    ) +
    ggplot2::scale_y_continuous(name = "Observations per Night") +
    ggplot2::scale_x_date(date_labels = date_label, date_breaks = date_breaks) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      text = ggplot2::element_text(size = text_size),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle, caption = plot_caption)

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}

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
#' @param date_label Character. Date format for x-axis labels. Default is "\%b"
#'   (abbreviated month name).
#' @param date_breaks Character. Optional break specification (e.g., "1 month").
#'   If \code{NULL} (default), adaptive breaks are used based on timespan.
#' @param facet_cols Integer. Number of facet columns. Default is 2.
#' @param facet_rows Integer. Number of facet rows. Default is NULL (all rows).
#' @param title Logical. If \code{TRUE}, adds plot title. Default is \code{FALSE}.
#' @param plot_title Character. Optional plot title.
#' @param plot_subtitle Character. Optional plot subtitle.
#' @param plot_caption Character. Optional plot caption.
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
                                    facet_cols = 2,
                                    facet_rows = NULL,
                                    title = FALSE,
                                    plot_title = NULL,
                                    plot_subtitle = NULL,
                                    plot_caption = NULL) {
  # Load and filter data
  dataset <- .load_plot_data(data_path, species_list = species, location_list = location_list)

  # Load gap data if requested (warn and skip on failure)
  active_dates <- NULL
  if (gaps) {
    active_dates <- tryCatch(
      .load_gap_data(data_path, location_list = location_list),
      error = function(e) {
        warning("Failed to load gap data: ", e$message, ". Skipping gap overlay.")
        NULL
      }
    )
  }

  # Prepare activity data
  counts <- .prepare_activity_data(dataset, species, location_list)
  counts$Location_label <- .clean_location_label(counts$Location)

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
    ggplot2::facet_wrap(~Location_label, ncol = facet_cols, nrow = facet_rows, scales = y_scale, strip.position = "top") +
    ggplot2::geom_hline(yintercept = 0) +
    .batr_theme(text_size) +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle, caption = plot_caption)

  # Add gaps if requested
  if (gaps && !is.null(active_dates)) {
    plot <- .add_gap_overlay(plot, active_dates)
  }

  # Add title if requested
  if (title && is.null(plot_title)) {
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
#' @param date_label Character. Date format for x-axis labels. Default is "\%b".
#' @param date_breaks Character. Optional break specification (e.g., "1 month").
#'   If \code{NULL} (default), adaptive breaks are used based on timespan.
#' @param facet_cols Integer. Number of facet columns. Default is 1.
#' @param facet_rows Integer. Number of facet rows. Default is \code{NULL} (all rows).
#' @param title Logical. If \code{TRUE} (default), adds plot title.
#' @param plot_title Character. Optional plot title.
#' @param plot_subtitle Character. Optional plot subtitle.
#' @param plot_caption Character. Optional plot caption.
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
                                   facet_cols = 1,
                                   facet_rows = NULL,
                                   title = TRUE,
                                   plot_title = NULL,
                                   plot_subtitle = NULL,
                                   plot_caption = NULL) {
  # Load gap data with location filtering
  active_dates <- tryCatch(
    data.table::as.data.table(.load_gap_data(data_path, location_list = location_list)),
    error = function(e) {
      warning("Failed to load gap data: ", e$message, ". Skipping plot.")
      data.table::data.table()
    }
  )

  # Calculate gaps (skip if no data)
  gap_list <- if (nrow(active_dates) > 0) {
    data.table::as.data.table(.plot_gap_calculator(as.data.frame(active_dates)))
  } else {
    data.table::data.table()
  }

  # Clean location labels for display
  if (nrow(gap_list) > 0) {
    gap_list[, Location_label := .clean_location_label(Location)]

    # Filter gaps by location if specified
    if (!is.null(location_list)) {
      gap_list <- gap_list[Location %in% location_list]
    }
  }

  # Calculate monitoring period
  if (nrow(gap_list) == 0) {
    if (is.null(monitoring_start)) {
      monitoring_start <- Sys.Date()
    }
    if (is.null(monitoring_end)) {
      monitoring_end <- monitoring_start
    }
  } else {
    if (is.null(monitoring_start)) {
      monitoring_start <- min(gap_list$xmin, na.rm = TRUE)
    }
    if (is.null(monitoring_end)) {
      monitoring_end <- max(gap_list$xmax, na.rm = TRUE)
    }
  }

  # Clip gap rectangles to monitoring period for proper display
  if (nrow(gap_list) > 0) {
    gap_list[, xmin := pmax(as.Date(xmin), as.Date(monitoring_start))]
    gap_list[, xmax := pmin(as.Date(xmax), as.Date(monitoring_end))]
    # Remove gaps that are entirely outside the monitoring period
    gap_list <- gap_list[xmin <= xmax]

    # If clipping removed all gaps, create an empty plot-compatible structure
    if (nrow(gap_list) == 0 && !is.null(location_list)) {
      gap_list <- data.table::data.table(
        Location = location_list,
        Location_label = .clean_location_label(location_list),
        xmin = as.Date(monitoring_start),
        xmax = as.Date(monitoring_start),
        ymin = 0,
        ymax = 1
      )
    }
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
    ggplot2::facet_wrap(~Location_label, ncol = facet_cols, nrow = facet_rows, scales = "fixed", strip.position = "top") +
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

  if (title && is.null(plot_title)) {
    plot <- plot + ggplot2::ggtitle("Monitor Uptime by Site")
  }

  plot <- plot + ggplot2::labs(title = plot_title, subtitle = plot_subtitle, caption = plot_caption)

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}

#' Monthly Activity Plot
#'
#' Creates a plot showing average nightly activity by month and species.
#' Requires data loaded with both observation and log file data (active_dates).
#'
#' @family Activity Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data and active_dates (from import_logs).
#' @param species Character vector. Species codes to include in the plot.
#'   If \code{NULL}, includes all species in the dataset.
#' @param location_list Character vector. Location names to include.
#'   If \code{NULL} (default), includes all locations.
#' @param monitoring_start Character. Monitoring start date (e.g., "2019-01-01").
#'   If \code{NULL}, uses the earliest date in the dataset.
#' @param monitoring_end Character. Monitoring end date (e.g., "2019-12-31").
#'   If \code{NULL}, uses the latest date in the dataset.
#' @param exclude_species Character vector of species codes to exclude.
#' @param species_names Named character vector for custom species labels. Names should be
#'   species codes from the data, values should be display names. If NULL (default),
#'   species codes are used as labels.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 20.
#' @param height Numeric. Plot height in cm. Default is 25.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#'
#' @return A ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' # Plot all species
#' monthly_activity_plot("data.RData")
#'
#' # Plot specific species with custom names
#' species_labels <- c(
#'   "Epfu" = "Big Brown Bat",
#'   "Mylu" = "Little Brown Myotis"
#' )
#' monthly_activity_plot("data.RData",
#'   species = c("Epfu", "Mylu"),
#'   species_names = species_labels
#' )
#' }
#' @export
monthly_activity_plot <- function(data_path,
                                  species = NULL,
                                  location_list = NULL,
                                  monitoring_start = NULL,
                                  monitoring_end = NULL,
                                  exclude_species = NULL,
                                  species_names = NULL,
                                  save_path = NULL,
                                  width = 20,
                                  height = 25,
                                  text_size = 10) {
  # Load observation data
  dataset <- .load_plot_data(data_path, species_list = species, location_list = location_list)

  # Filter by monitoring period if specified
  if (!is.null(monitoring_start)) {
    dataset <- dataset[dataset$Night >= as.Date(monitoring_start), ]
  }
  if (!is.null(monitoring_end)) {
    dataset <- dataset[dataset$Night <= as.Date(monitoring_end), ]
  }

  # Prepare activity data by species and night
  activity_data <- .prepare_activity_data(dataset, species, location_list)

  # Load active_dates to normalize by effort
  active_dates <- tryCatch(
    .load_gap_data(data_path, location_list = location_list),
    error = function(e) {
      warning("Failed to load active_dates. Using raw observation counts instead of normalized values.")
      NULL
    }
  )

  # Cast to monthly format
  monthly_species <- reshape2::dcast(
    activity_data,
    lubridate::year(Night) + Location + Species ~ lubridate::month(Night),
    value.var = "Count",
    fun.aggregate = sum,
    fill = 0
  )

  # Rename columns
  colnames(monthly_species)[4:length(monthly_species)] <- paste("Obs", colnames(monthly_species)[4:length(monthly_species)], sep = "_")
  names(monthly_species)[names(monthly_species) == "lubridate::year(Night)"] <- "Year"

  # If we have active_dates, create monthly effort data
  if (!is.null(active_dates)) {
    monthly_effort <- reshape2::dcast(
      active_dates,
      lubridate::year(Date) + Location ~ lubridate::month(Date),
      value.var = "Log_Count",
      fun.aggregate = function(x) sum(x > 0), # Count days recorded
      fill = 0
    )
    colnames(monthly_effort)[3:length(monthly_effort)] <- paste("Effort", colnames(monthly_effort)[3:length(monthly_effort)], sep = "_")
    names(monthly_effort)[names(monthly_effort) == "lubridate::year(Date)"] <- "Year"

    # Merge observation and effort data
    data <- merge(monthly_species, monthly_effort, by = c("Year", "Location"), all.x = TRUE)

    # Calculate normalized values (observations per recording day)
    obs_start <- 4
    effort_start <- 4 + (length(monthly_species) - 3)

    for (month in 1:12) {
      obs_col <- paste("Obs", month, sep = "_")
      effort_col <- paste("Effort", month, sep = "_")

      if (obs_col %in% names(data) && effort_col %in% names(data)) {
        data[[month.name[month]]] <- ifelse(data[[effort_col]] > 0,
          data[[obs_col]] / data[[effort_col]],
          NA
        )
      }
    }
  } else {
    # Without effort data, just use raw counts
    data <- monthly_species
    for (month in 1:12) {
      col_name <- paste("Obs", month, sep = "_")
      if (col_name %in% names(data)) {
        data[[month.name[month]]] <- data[[col_name]]
      }
    }
  }

  # Remove temporary columns
  data <- data[, !grepl("Obs_|Effort_", names(data))]

  # Filter excluded species
  if (!is.null(exclude_species)) {
    data <- data[!data$Species %in% exclude_species, ]
  }

  # Melt to long format
  month_cols <- month.name[1:12]
  available_months <- month_cols[month_cols %in% names(data)]

  if (length(available_months) == 0) {
    stop("No monthly data available for plotting")
  }

  data_melt <- reshape2::melt(
    data,
    id.vars = c("Year", "Location", "Species"),
    measure.vars = available_months,
    variable.name = "Month",
    value.name = "MeanObservations"
  )

  # Ensure Month is ordered correctly
  data_melt$Month <- factor(data_melt$Month, levels = month.name, ordered = TRUE)

  # Create species labeller
  unique_species <- sort(unique(data_melt$Species))
  species_labeller <- .species_labeller(unique_species, species_names)

  # Create plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = data_melt,
      mapping = ggplot2::aes(x = Month, y = MeanObservations),
      stat = "identity",
      fill = "black"
    ) +
    ggplot2::facet_wrap(~Species,
      ncol = 1, scales = "free", strip.position = "top",
      labeller = ggplot2::labeller(Species = species_labeller)
    ) +
    ggplot2::scale_y_continuous(
      name = if (is.null(active_dates)) "Observations" else "Mean Observations per Recording Day"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0, face = "bold"),
      text = ggplot2::element_text(size = text_size),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(face = "bold")
    )

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}

# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Load gap data from log files
#' @keywords internal
#' Prepare activity count data for plotting
#' @keywords internal
.prepare_activity_data <- function(dataset, species, location_list) {
  dt <- data.table::as.data.table(dataset)
  counts <- dt[, list(Count = .N), by = list(Night, Species, Location)]
  data.table::setorder(counts, Night, Location, Species)
  return(as.data.frame(counts))
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
  gap_list <- data.table::as.data.table(.plot_gap_calculator(active_dates))
  gap_list[, Location_label := gsub("_", " ", Location)]

  plot +
    ggplot2::geom_rect(
      data = gap_list,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = 0.9),
      show.legend = FALSE
    ) +
    ggplot2::scale_alpha(guide = "none")
}
