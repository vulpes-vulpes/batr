# ============================================================================
# Activity Plotting Functions
# ============================================================================
# Consolidated from plot_nightly_activity.R and update_Plots.R

#' Plot Activity for a Species at Each Site Within a Project
#'
#' \code{species_daily_site_plot} creates a plot of nightly activity for a chosen
#' species at all sites within a project.
#'
#' @family Activity Plots
#'
#' @param data_path Character. Path to an existing RData file.
#' @param species Character: the four letter species code for the species you
#'  wish to plot. E.g. "Epfu".
#' @param monitoring_start Character, the date the monitoring began (e.g.
#'  "2019-01-01"), leave as NULL to use the earliest data point as the
#'  monitoring start.
#' @param monitoring_end Character, the date the monitoring ceased (e.g.
#'  "2019-01-01"), leave as NULL to use the latest data point as the monitoring
#'  end
#' @param gaps Boolean. Set true to add grey blocks to the plot to indicate
#'  periods in which the recorder was not active (requires log files to be
#'  loaded first using \code{import_logs}. Default is \code{FALSE}.)
#' @param save.directory Character: if provided a .png image of the plot will be
#'  saved in the folder specified. Defaults to \code{NULL}: no output saved.
#' @param y_scale Character. Determines whether scales on the y-axis are matched
#'  or free. Defaults to "free_y" for unmatched axis, set to "fixed" for matched
#'  axis.
#' @param width Number. The width in cm of the plot if saved to file. Default is
#'  25 cm.
#' @param height Number. The height of the plot if saved to file. Default is 20
#'  cm.
#' @param text_size Numeric: adjusts the size of text in the figure.
#' @param date_label Date value: adjusts the formatting of the month labels on
#'  the y-axis. See https://www.statmethods.net/input/dates.html for formatting.
#' @param title Logical vector. If \code{FALSE} (default) title is omitted.
#'
#' @return A plot as an object in the current environment, and a saved image if
#'  selected.
#'
#' @examples
#' \dontrun{
#' species_daily_site_plot(species_night_site_projectname, "Project Name", "Mylu", "2019-01-01", "2019-12-31")
#' }
#' @export
#'
#'
species_daily_site_plot <- function(data_path, species, monitoring_start = NULL,
                                    monitoring_end = NULL, gaps = FALSE,
                                    save_directory = NULL, y_scale = "free_y",
                                    width = 15.9, height = 8.43, text_size = 10,
                                    date_label = "%b", title = FALSE) {
  # Subset to selected species
  # species_subset <- dataset[which(dataset$Species==species),]
  # if (!is.null(survey_year)) {
  #  species_subset <- species_subset[which(lubridate::year(species_subset$Night)==survey_year),]
  # }
  # Import data
  .validate_rdata_path(data_path) # Check the data path provided
  # load(data_path) # Load the data path
  dataset <- .location_subsetter(data_path) # Offer to subset locations
  dataset <- dataset[which(dataset$Species == species), ] # subset the species
  # Create count table
  counts <- aggregate(dataset$Species, list(
    Night = dataset$Night, Species = dataset$Species, Location = dataset$Location, Latitude = dataset$Latitude, Longitude = dataset$Longitude
  ), FUN = length) # Create intial table
  names(counts)[names(counts) == "x"] <- "Count"
  # Create gap table if needed
  load(data_path)
  if (isTRUE(gaps)) {
    if (!exists("active_dates")) {
      stop("Log file data are missing, please run import_logs and try again.")
    } else {
      gap_list <- .plot_gap_calculator(active_dates)
    }
  }
  # Create monitoring start / end if needed
  if (isTRUE(gaps)) {
    if (is.null(monitoring_start)) {
      monitoring_start <- min(gap_list$xmin)
    }
    if (is.null(monitoring_end)) {
      monitoring_end <- max(gap_list$xmax)
    }
  } else {
    if (is.null(monitoring_start)) {
      monitoring_start <- min(dataset$Night)
    }
    if (is.null(monitoring_end)) {
      monitoring_end <- max(dataset$Night)
    }
  }
  # Create Plot
  species_daily_site_plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = counts, mapping = ggplot2::aes(x = Night, y = Count), stat = "identity", fill = "black") +
    ggplot2::scale_y_continuous(name = "Observations per Night") +
    ggplot2::scale_x_date(limits = c(as.Date(monitoring_start), as.Date(monitoring_end)), breaks = scales::pretty_breaks(), date_breaks = "1 month", date_labels = date_label) +
    ggplot2::facet_wrap(
      ~Location,
      ncol = 1, scales = y_scale, strip.position = "top"
    ) + # , labeller=location_labeller) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(size = text_size)
    ) +
    if (isTRUE(gaps)) {
      gap_list <- batr:::.plot_gap_calculator(active_dates)
      ggplot2::geom_rect(
        data = gap_list,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = 0.9),
        show.legend = FALSE
      )
    }
  if (isTRUE(title)) {
    species_daily_site_plot <- species_daily_site_plot + ggplot2::ggtitle(paste("Total Activity of ", species, " by Site", sep = ""))
  }
  # Save plot to a specified folder if requested
  if (!is.null(save_directory)) {
    ggplot2::ggsave(paste(save_directory, "/", species, "_daily_site_plot.png", sep = ""), width = width, height = height, units = "cm")
  }
  # Save plot to a specified folder if requested
  # if (!is.null(save_directory)) {
  #  ggplot2::ggsave(paste(save_directory, "/", species, "_daily_site_plot_", project_name, ".png", sep = ""), width = width, height = height, units = "cm")
  # }
  # Save plot to environment
  # assign(paste(species, "_daily_site_plot_", project_name, sep = ""), species_site_aggregated_plot, envir=globalenv())
  # Plot plot
  return(species_daily_site_plot)
}

# ============================================================================
# Additional Activity Plotting Functions (from update_Plots.R)
# ============================================================================

#' Plot of Monitoring Effort by Site
#'
#' \code{monitoring_effort_plot} creates a plot showing monitor uptime at each
#' site within the project.
#'
#' @family Activity Plots
#'
#' @param gaps Object: a gaps data frame from log files.
#' @param project_name Character (deprecated).
#' @param monitoring_start Character, monitoring start date.
#' @param monitoring_end Character, monitoring end date.
#' @param survey_year Character (deprecated).
#' @param save_directory Character: directory to save plot.
#' @param title Logical. If TRUE (default), adds title.
#' @param width Number. Width in cm.
#' @param height Number. Height in cm.
#' @param text_size Numeric: text size.
#' @param date_label Date format string.
#'
#' @return A ggplot2 plot object.
#'
#' @export
monitoring_effort_plot <- function(gaps, project_name, monitoring_start, monitoring_end,
                                   survey_year = NULL, save_directory = NULL, title = TRUE,
                                   width = 25, height = 20, text_size = 8, date_label = "%b") {
  monitoring_effort_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = gaps,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, alpha = 0.9),
      show.legend = FALSE
    ) +
    ggplot2::scale_x_date(
      limits = c(as.Date(monitoring_start), as.Date(monitoring_end)),
      breaks = scales::pretty_breaks(),
      date_breaks = "1 month",
      date_labels = date_label
    ) +
    ggplot2::facet_wrap(~Location, ncol = 1, scales = "fixed", strip.position = "top") +
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

  if (isTRUE(title)) {
    monitoring_effort_plot <- monitoring_effort_plot +
      ggplot2::ggtitle("Total Monitor Uptime by Site")
  }

  if (!is.null(save_directory)) {
    ggplot2::ggsave(
      paste(save_directory, "/monitoring_uptime_plot_", project_name, ".png", sep = ""),
      width = width, height = height, units = "cm"
    )
  }

  return(monitoring_effort_plot)
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
