#' batr: Imports GUANO Annotated Files and Creates Summary Data and Plots.
#'
#' A collection of tools to import and use GUANO metadata in annotated WAV files
#' of bat recordings. The package depends on comprehensive metadata being
#' present in the files. The package extracts this data to a text file for
#' future loading. It calculates summary data and basic plots.
#'
#'
#' @name batr
"_PACKAGE"

# Global variable bindings for data.table and NSE patterns
# Suppresses R CMD check notes about "no visible binding for global variable"
utils::globalVariables(c(
    # Column names used in data.table operations
    "Location", "Location_label", "Species", "Night", "Timestamp",
    "Latitude", "Longitude", "Time_PM", "Time",
    "Species.Auto.ID", "Species.Manual.ID", "File.Name",
    "DATE", "night", "update",
    # Plotting variables
    "xmin", "xmax", "ymin", "ymax",
    "lat", "lon", "sunset_time", "ob_time",
    "Count", "Month", "x",
    "X", "Y", # Coordinates from sf::st_coordinates for ggrepel
    # Variables from loaded RData files
    "observations", "active_dates"
))
