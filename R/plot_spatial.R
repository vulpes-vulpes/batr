# ============================================================================
# Spatial Plotting Functions
# ============================================================================

#' Plot Monitoring Location Map
#'
#' Creates a scatter plot showing the geographic coordinates of
#' monitoring locations, optionally with a background basemap.
#'
#' @family Location Plots
#'
#' @param data_path Character. Path to an existing RData file containing
#'   observation data with Latitude and Longitude columns.
#' @param location_list Character vector. Location names to include in the plot.
#'   If \code{NULL} (default), includes all locations in the dataset.
#' @param basemap Logical. If \code{TRUE}, adds a background basemap using ggspatial.
#'   Requires the ggspatial package to be installed. Default is \code{FALSE}.
#' @param basemap_type Character. Type of map tiles to use. Common options include:
#'   \itemize{
#'     \item \code{"osm"} - OpenStreetMap (default)
#'     \item \code{"osm-bw"} - OpenStreetMap black and white
#'     \item \code{"stamenterrain"} - Stamen terrain with topographic relief
#'     \item \code{"stamenwatercolor"} - Stamen watercolor (artistic)
#'     \item \code{"stamentoner"} - Stamen toner (high contrast)
#'     \item \code{"stamentonerlite"} - Stamen toner lite (minimal)
#'     \item \code{"cartolight"} - CartoDB light
#'     \item \code{"cartodark"} - CartoDB dark
#'   }
#'   See \code{rosm::osm.types()} for the complete list of available types.
#' @param zoom Numeric or NULL. Zoom level for map tiles (higher = more detail).
#'   If NULL (default), zoom is calculated automatically. Typical range is 5-18.
#' @param save_path Character. Full file path to save plot as PNG. If \code{NULL}
#'   (default), plot is not saved.
#' @param width Numeric. Plot width in cm. Default is 15.
#' @param height Numeric. Plot height in cm. Default is 12.
#' @param text_size Numeric. Text size for plot labels. Default is 10.
#' @param point_size Numeric. Size of location points. Default is 3.
#' @param point_color Character. Color of location points. Default is "red".
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
#' # Plot with basemap
#' location_map_plot("data.RData",
#'   basemap = TRUE,
#'   basemap_type = "osm"
#' )
#'
#' # Plot specific locations with terrain basemap
#' location_map_plot("data.RData",
#'   location_list = c("Site1", "Site2"),
#'   basemap = TRUE,
#'   basemap_type = "stamenterrain"
#' )
#' }
#' @export
location_map_plot <- function(data_path,
                              location_list = NULL,
                              basemap = FALSE,
                              basemap_type = "osm",
                              zoom = NULL,
                              save_path = NULL,
                              width = 15,
                              height = 12,
                              text_size = 10,
                              point_size = 3,
                              point_color = "red",
                              plot_title = "Monitoring Locations",
                              plot_subtitle = NULL,
                              plot_caption = NULL) {
  # Load data
  dataset <- .load_plot_data(data_path, location_list = location_list)

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

  # Create plot with or without basemap
  if (basemap) {
    # Check if ggspatial package is available
    if (!requireNamespace("ggspatial", quietly = TRUE)) {
      warning("Package 'ggspatial' is required for basemap functionality. Install with install.packages('ggspatial'). Falling back to simple plot.")
      basemap <- FALSE
    }
  }

  if (basemap) {
    # Create plot with basemap using ggspatial
    tryCatch(
      {
        # Check if sf package is available
        if (!requireNamespace("sf", quietly = TRUE)) {
          warning("Package 'sf' is required for basemap functionality. Install with install.packages('sf'). Falling back to simple plot.")
          basemap <<- FALSE
        } else {
          # Convert to sf object with explicit CRS
          unique_locs_sf <- sf::st_as_sf(
            unique_locs,
            coords = c("Longitude", "Latitude"),
            crs = 4326 # WGS84
          )

          # Calculate bbox and expand it
          bbox <- sf::st_bbox(unique_locs_sf)

          # Expand by 0.5 degrees or 20% of extent, whichever is larger
          lon_extent <- bbox["xmax"] - bbox["xmin"]
          lat_extent <- bbox["ymax"] - bbox["ymin"]
          lon_expand <- max(0.5, lon_extent * 0.2)
          lat_expand <- max(0.5, lat_extent * 0.2)

          bbox["xmin"] <- bbox["xmin"] - lon_expand
          bbox["xmax"] <- bbox["xmax"] + lon_expand
          bbox["ymin"] <- bbox["ymin"] - lat_expand
          bbox["ymax"] <- bbox["ymax"] + lat_expand

          # If zoom not specified, calculate based on extent
          if (is.null(zoom)) {
            extent <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
            zoom <- if (extent < 0.5) 12 else if (extent < 2) 10 else if (extent < 5) 8 else 7
          }

          plot <- ggplot2::ggplot() +
            ggspatial::annotation_map_tile(
              type = basemap_type,
              zoom = zoom,
              progress = "none"
            ) +
            ggplot2::geom_sf(
              data = unique_locs_sf,
              size = point_size,
              color = point_color
            ) +
            ggplot2::coord_sf(
              xlim = c(bbox["xmin"], bbox["xmax"]),
              ylim = c(bbox["ymin"], bbox["ymax"]),
              crs = 4326,
              expand = FALSE
            ) +
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

          # Add labels if multiple locations
          if (nrow(unique_locs) > 1) {
            if (requireNamespace("ggrepel", quietly = TRUE)) {
              # Extract coordinates for labeling
              coords <- as.data.frame(sf::st_coordinates(unique_locs_sf))
              coords$Location <- unique_locs$Location

              plot <- plot +
                ggrepel::geom_label_repel(
                  data = coords,
                  ggplot2::aes(x = X, y = Y, label = Location),
                  size = text_size / 3,
                  fontface = "bold",
                  box.padding = 0.5,
                  point.padding = 0.3,
                  min.segment.length = 0
                )
            } else {
              plot <- plot +
                ggplot2::geom_sf_text(
                  data = unique_locs_sf,
                  ggplot2::aes(label = Location),
                  nudge_y = 0.05,
                  size = text_size / 3,
                  fontface = "bold"
                )
            }
          }
        }
      },
      error = function(e) {
        warning("Failed to create basemap: ", e$message, ". Falling back to simple plot.")
        basemap <<- FALSE
      }
    )
  }

  if (!basemap) {
    # Create simple plot without basemap
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

    # Add labels if multiple locations
    if (nrow(unique_locs) > 1) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        plot <- plot +
          ggrepel::geom_label_repel(
            ggplot2::aes(label = Location),
            size = text_size / 3,
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.3,
            min.segment.length = 0
          )
      } else {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = Location),
            vjust = -1,
            size = text_size / 3,
            fontface = "bold"
          )
      }
    }
  }

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = plot, width = width, height = height, units = "cm")
  }

  return(plot)
}
