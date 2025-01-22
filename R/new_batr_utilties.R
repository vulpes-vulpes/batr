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

.location_subsetter <- function(data_path, dataset = NULL) {
  load(data_path)
  if (!is.null(dataset)) {
    observations <- dataset
  } else {
    observations <- observations} # Check if a subsetted dataset already exists and use this for further subsetting if not
  if (exists("location_list")) {
    message("Location list found, using to subset (clear saved list to change).")
    dataset <- observations[observations$Location %in% location_list,]
    return(dataset) # Check if there is a location list saved in the RData file and use to subset if so
  } else {
    message("No location list specified, would you like to set one? Select y to set a location list, select no to proceed with all locations included.")
    setlist <- readline(prompt = "y/n:")
    if (setlist == "y" | setlist == "Y") {
      location_list <- .location_requester(data_path)
      dataset <- observations[observations$Location %in% location_list,]
      return(dataset)
    } else if (setlist == "n" | setlist == "N") {
      message("Proceeding with all available locations.")
      dataset <- observations
      return(dataset)
    } else {
      stop("Input not recognised, please try again.")
    } # If there is not an existing location list, ask whether one is needed and call a function to create and then subset if so, use all locations if no
  }
}

.species_subsetter <- function(data_path, dataset = NULL) {
  load(data_path)
  if (!is.null(dataset)) {
    observations <- dataset
  } else {
    observations <- observations} # Check if a subsetted dataset already exists and use this for further subsetting if not
  if (exists("species_list")) {
    message("Species list found, using to subset (clear saved list to change).")
    dataset <- observations[observations$Species %in% species_list,] 
    return(dataset) # Check if there is a species list saved in the RData file and use to subset if so
  } else {
    message("No species list specified, would you like to set one? Select y to set a species list, select no to proceed with all species included.")
    setlist <- readline(prompt = "y/n:")
    if (setlist == "y" | setlist == "Y") {
      species_list <- .species_requester(data_path)
      dataset <- observations[observations$Species %in% species_list,]
      return(dataset)
    } else if (setlist == "n" | setlist == "N") {
      message("Proceeding with all available species.")
      dataset <- observations
      return(dataset)
    } else {
      stop("Input not recognised, please try again.")
    } # If there is not an existing species list, ask whether one is needed and call a function to create and then subset if so, use all species if no
  }
}

.monitoring_start_finder <- function(dataset) {
  monitoring_start <- min(dataset$Night)
  return(monitoring_start)
}

.monitoring_end_finder <- function(dataset) {
  monitoring_end <- max(dataset$Night)
  return(monitoring_end)
}

.plot_gap_calculator <- function(active_dates) {
  failed_dates <- active_dates[which(active_dates$Log_Count==0),]
  failed_dates <- failed_dates[order(failed_dates$Location),]
  failed_dates <- failed_dates[!duplicated(failed_dates), ]
  #failed_dates$Active <- NULL
  failed_dates$Gaps <- 0 # Create Gaps Column
  failed_dates[1,4] <- 1 # Set first row to 1
  row <- 2 # Set iterator to second row
  maximum_row <- length(failed_dates$Gaps) + 1 # Calculate maximum
  while (row < maximum_row) { # While loop to add consecutive labels based on date differences
    rowdown <- row - 1 # Create value for row - 1
    if (failed_dates[row,1] - failed_dates[rowdown,1] == 1) { #If dates are consecutive, set same as row above
      failed_dates [row,4] <- failed_dates[rowdown,4]
    } else { # Else add one
      failed_dates[row,4] <- failed_dates[rowdown,4] + 1
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
  while (fail < (maximum_fail+1)) {
    gap_list[fail,3] <- min(failed_dates[failed_dates$Gaps == fail,1])
    fail <- fail + 1
  } # Compute end date for each break
  gap_list$xmin <- as.Date(gap_list$xmin, origin = "1970-01-01") # Convert to date
  fail <- 1 # Reset fail variable
  gap_list$xmax <- 0 # Create empty column for xmin
  while (fail < (maximum_fail+1)) {
    gap_list[fail,4] <- max(failed_dates[failed_dates$Gaps == fail,1])
    fail <- fail + 1
  } # Compute start date for each break
  gap_list$xmax <- as.Date(gap_list$xmax, origin = "1970-01-01") # Convert to date
  gap_list$ymax <- Inf
  gap_list$ymin <- 0
  gap_list$Gaps <- NULL
  return(gap_list)
}
