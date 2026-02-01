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

#' Validate RData file path
#'
#' Simple validation that a path has the correct extension and optionally exists.
#'
#' @param path Character. Path to validate.
#' @param must_exist Logical. If \code{TRUE}, checks that file exists.
#'
#' @return The validated path (invisibly).
#'
#' @keywords internal
.validate_rdata_path <- function(path, must_exist = TRUE) {
  if (is.null(path) || !is.character(path) || length(path) != 1) {
    stop("data_path must be a single character string")
  }

  if (!grepl("\\.RData$|\\.rda$", path, ignore.case = TRUE)) {
    stop("data_path must have .RData or .rda extension")
  }

  if (must_exist && !file.exists(path)) {
    stop("RData file not found: ", path)
  }

  invisible(path)
}
