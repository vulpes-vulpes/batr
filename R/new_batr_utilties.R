#' Save or update an object in an RData file
#'
#' Saves an R object to an RData file, either creating a new file or updating
#' an existing one. If the file exists, the object will be added or updated
#' while preserving other objects in the file.
#'
#' @param obj The object to save.
#' @param data_path Character. Path to the .RData file.
#' @param overwrite Logical. If \code{TRUE} (default), overwrites existing
#'   objects with the same name. If \code{FALSE}, keeps existing objects
#'   and only adds new ones.
#'
#' @return Invisible NULL. Called for its side effect of saving to file.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(x = 1:10)
#' .save_to_rdata(my_data, "output.RData")
#' }
.save_to_rdata <- function(obj, data_path, overwrite = TRUE) {
  # Validate inputs
  if (missing(obj)) {
    stop("'obj' is required")
  }

  if (missing(data_path) || !is.character(data_path) || length(data_path) != 1) {
    stop("'data_path' must be a single character string")
  }

  if (!grepl("\\.RData$|\\.rda$", data_path, ignore.case = TRUE)) {
    stop("'data_path' must have .RData or .rda extension")
  }

  # Get the name of the object
  obj_name <- deparse(substitute(obj))

  # If file doesn't exist, simply save the object
  if (!file.exists(data_path)) {
    assign(obj_name, obj, envir = environment())
    save(list = obj_name, file = data_path, envir = environment())
    message(sprintf("Created new file: %s with object '%s'", data_path, obj_name))
    return(invisible(NULL))
  }

  # Load existing data into temporary environment
  temp_env <- new.env()
  load(file = data_path, envir = temp_env)

  # Check if object already exists
  if (obj_name %in% ls(temp_env)) {
    if (overwrite) {
      message(sprintf("Updating existing object '%s' in %s", obj_name, data_path))
    } else {
      message(sprintf("Keeping existing object '%s' in %s (overwrite=FALSE)", obj_name, data_path))
      return(invisible(NULL))
    }
  } else {
    message(sprintf("Adding new object '%s' to %s", obj_name, data_path))
  }

  # Add or update the object in the environment
  if (overwrite || !obj_name %in% ls(temp_env)) {
    assign(obj_name, obj, envir = temp_env)
  }

  # Save all objects from the environment
  save(list = ls(temp_env), file = data_path, envir = temp_env)

  return(invisible(NULL))
}

.check_data_path <- function(data_path = NULL, action = FALSE, object = NULL) {
  is_rdata_file <- (sub(".*\\.", "", data_path) == "RData")
  file_exists <- file.exists(data_path)
  if (action == FALSE && is.null(object)) {
    if (is_rdata_file == TRUE && file_exists == TRUE) {
      return(data_path)
    } else if (is_rdata_file == TRUE && file_exists == FALSE) {
      warning("Specified .RData file does not exist.")
      return(data_path)
    } else {
      stop("Invalid data_path specified. Please supply a valid .RDATA file or.")
    }
  }
  if (!is.null(object)) {
    if (is_rdata_file == TRUE && file_exists == TRUE) {
      load(data_path)
      if (exists(object)) {
        overwrite <- readline(prompt = paste("Object", object, "already exists in this file. Overwrite? Y/N?"))
        if (overwrite == "Y" || overwrite == "y") {
          return(data_path)
        } else {
          stop("No overwrite selected. Exiting, no files saved.")
        }
      } else {
        return(data_path)
      }
    } else if (is_rdata_file == TRUE && file_exists == FALSE) {
      return(data_path)
    }
  }
}

.plot_gap_calculator <- function(active_dates) {
  failed_dates <- active_dates[which(active_dates$Log_Count == 0), ]
  failed_dates <- failed_dates[order(failed_dates$Location), ]
  failed_dates <- failed_dates[!duplicated(failed_dates), ]
  # failed_dates$Active <- NULL
  failed_dates$Gaps <- 0 # Create Gaps Column
  failed_dates[1, 4] <- 1 # Set first row to 1
  row <- 2 # Set iterator to second row
  maximum_row <- length(failed_dates$Gaps) + 1 # Calculate maximum
  while (row < maximum_row) { # While loop to add consecutive labels based on date differences
    rowdown <- row - 1 # Create value for row - 1
    if (failed_dates[row, 1] - failed_dates[rowdown, 1] == 1) { # If dates are consecutive, set same as row above
      failed_dates[row, 4] <- failed_dates[rowdown, 4]
    } else { # Else add one
      failed_dates[row, 4] <- failed_dates[rowdown, 4] + 1
    }
    row <- row + 1 # Iterate row
  } # While loop to iterate gap numbers
  maximum_fail <- max(failed_dates$Gaps) # Calculate number of gaps
  gap_list <- failed_dates # Copy dates without recordings to a new data frame
  gap_list$Date <- NULL # Remove unnecessary column
  gap_list$Log_Count <- NULL # Remove unnecessary column
  gap_list <- unique(gap_list) # Remove duplicate rows
  fail <- 1 # Create fail variable
  gap_list$xmin <- 0 # Create empty column for xmax
  while (fail < (maximum_fail + 1)) {
    gap_list[fail, 3] <- min(failed_dates[failed_dates$Gaps == fail, 1])
    fail <- fail + 1
  } # Compute end date for each break
  gap_list$xmin <- as.Date(gap_list$xmin, origin = "1970-01-01") # Convert to date
  fail <- 1 # Reset fail variable
  gap_list$xmax <- 0 # Create empty column for xmin
  while (fail < (maximum_fail + 1)) {
    gap_list[fail, 4] <- max(failed_dates[failed_dates$Gaps == fail, 1])
    fail <- fail + 1
  } # Compute start date for each break
  gap_list$xmax <- as.Date(gap_list$xmax, origin = "1970-01-01") # Convert to date
  gap_list$ymax <- Inf
  gap_list$ymin <- 0
  gap_list$Gaps <- NULL
  return(gap_list)
}
