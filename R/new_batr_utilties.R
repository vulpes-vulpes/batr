.save_to_RDATA <- function (obj, data_path) {
  .dummy <- NULL
  overwrite <- T
  if (!file.exists(data_path)) save(.dummy, file = data_path)
  
  old_e <- new.env()
  new_e <- new.env()
  
  load(file = data_path, envir = old_e)
  
  name_obj <- deparse(substitute(obj))   # get the name of the object
  
  # new_e[[name_obj]] <- get(name_obj)     # use this only outside a function
  new_e[[name_obj]] <- obj
  
  # merge object from old environment with the new environment
  # ls(old_e) is a character vector of the object names
  if (overwrite) {
    # the old variables take precedence over the new ones
    invisible(sapply(ls(new_e), function(x)
      assign(x, get(x, envir = new_e), envir = old_e)))
    # And finally we save the variables in the environment
    save(list = ls(old_e), file = data_path, envir = old_e)
  } else {
    invisible(sapply(ls(old_e), function(x)
      assign(x, get(x, envir = old_e), envir = new_e)))
    # And finally we save the variables in the environment
    save(list = ls(new_e), file = data_path, envir = new_e)
  }
}

.check_data_path <- function (data_path = NULL, action = FALSE) {
  if (is.null(data_path) && action == "New") {
    save_path <- readline(prompt="No 'data_path' specified. Please input path to save data file (e.g. \"C/user/directory\"):")
    if (!dir.exists(save_path)) {
      stop("Directory does not exist. Exiting, file not saved!.")
    } else {
      filename <- readline(prompt="Please input your desired filename:")
      data_path <- paste(save_path, "/", filename, ".RData", sep = "")
    } # If action is New and no data path exists ask for data path and filename
  } else if (is.null(data_path) && action != "New") {
    stop("No file path specified, please specify a path and try again.")
  } # Exit if data path isn't specified for non New actions
  else if (!file.exists(data_path) && action == FALSE) {
    stop("Specified file does not exist, please try again."
    ) # If no action and file doesn't exist then end
  } else if (sub('.*\\.', '', data_path) != "RData" && action == FALSE) {
    stop("Invalid data_path specified. Please supply a valid .RDATA file.") # If no action and file not valid then end
  } else if ((sub('.*\\.', '', data_path) == "RData") && action == "New"){
    overwrite <- readline(prompt="Overwrite existing data file? Y/N: ")
    if (overwrite == "Y" | overwrite == "y") {
      data_path <- data_path
    } else {
      stop("No overwrite selected. Exiting, no files saved.")
    } # If data path is valid but action is new check about overwriting
  } else if (sub('.*\\.', '', data_path) != "RData"){
    stop("Invalid data_path specified. Check that a valid .RDATA file is specified, or leave blank to create a new file.")
  }
  return(data_path)
}
