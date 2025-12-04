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
