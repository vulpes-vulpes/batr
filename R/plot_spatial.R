# ============================================================================
# Spatial Plotting Functions
# ============================================================================

#' Plot Monitoring Location Map
#'
#' Creates a simple scatter plot showing the geographic coordinates of
#' monitoring locations.
#'
#' @family Location Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data with Latitude and Longitude columns.
#' @param location_list Character vector. Location names to include in the plot.
#'   If \code{NULL} (default), includes all locations in the dataset.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 15.
#' @param height Numeric. Plot height in cm. Default is 12.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#' @param point_size Numeric. Size of location points. Default is 3.
#' @param point_color Character. Color of location points. Default is "darkblue".
#' @param plot_title Character. Optional plot title. Default is "Monitoring Locations".
#' @param plot_subtitle Character. Optional plot subtitle.
#' @param plot_caption Character. Optional plot caption.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot all locations
#' location_map_plot("data.RData")
#'
#' # Plot specific locations
#' location_map_plot("data.RData",
#'   location_list = c("Site1", "Site2")
#' )
#' }
#' @export
location_map_plot <- function(data_path,
                              location_list = NULL,
                              save_path = NULL,
                              width = 15,
                              height = 12,
                              text_size = 10,
                              point_size = 3,
                              point_color = "darkblue",
                              plot_title = "Monitoring Locations",
                              plot_subtitle = NULL,
                              plot_caption = NULL) {
  # Load data
  dataset <- .load_plot_data(data_path, species_list = NULL, location_list = location_list)

  # Check for coordinate columns
  if (!("Latitude" %in% names(dataset)) || !("Longitude" %in% names(dataset))) {
    stop("Data must contain Latitude and Longitude columns")
  }

  # Get unique locations with coordinates
  unique_locs <- unique(dataset[, c("Location", "Latitude", "Longitude")])
  unique_locs <- unique_locs[!is.na(unique_locs$Latitude) & !is.na(unique_locs$Longitude), ]

  if (nrow(unique_locs) == 0) {
    stop("No valid coordinates found in data")
  }

  # Create plot
  plot <- ggplot2::ggplot(unique_locs, ggplot2::aes(x = Longitude, y = Latitude)) +
    ggplot2::geom_point(size = point_size, color = point_color) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption,
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}
