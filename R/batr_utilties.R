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

.check_data_path <- function (data_path, action) {
  if (is.null(data_path)) {
    save_path <- readline(prompt="No 'data_path' specified. Please input path to save data file (e.g. \"C/user/directory\"):")
    if (!dir.exists(save_path)) {
      stop("Error! Directory does not exist. Exiting, file not saved!.")
    } else {
      filename <- readline(prompt="Please input your desired filename:")
      data_path <- paste(save_path, "/", filename, ".RData", sep = "")
    }
  } else if (sub('.*\\.', '', data_path) != "RData"){
      stop("Error! Invalid data_path specified. Check that a valid .RDATA file is specified, or leave blank to create a new file.")
  } else if ((sub('.*\\.', '', data_path) == "RData") && action == "New"){
      overwrite <- readline(prompt="Overwrite existing data file? Y/N: ")
      if (overwrite == "Y" | overwrite == "y") {
        data_path <- data_path
      } else {
        stop("No overwrite selected. Exiting, no files saved.")
      }
  }
  return(data_path)
}
