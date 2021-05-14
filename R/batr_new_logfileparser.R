read_logs <- function(log_path, data_path = NULL, monitoring_start = NULL, monitoring_end = NULL) {
  
  data_path <- .check_data_path(data_path)
  
  ###
  ### Import Wildlife Acoustics log files and convert to single neat data.frame for further processing
  ###
  
  WA_file_list <- list.files(log_path, recursive = T, pattern = "*.txt", full.names = T) # Creates a list of files in the folder
  if (length(WA_file_list) > 1) {
    message("Wildlife Acoustics summary files found. Reading files:")
    logs <- data.table::rbindlist(pbapply::pbsapply(WA_file_list, data.table::fread, simplify = FALSE, select = c("DATE", "TIME")),
                    use.names = TRUE, idcol = "FileName", fill = T)
    names(logs)[names(logs) == 'DATE'] <- 'Date' # Fix name of date column
    logs <- subset(logs, Date != "DATE")
    logs$Date = lubridate::ymd(logs$Date) # Convert dates from factor to 'dates'
    message("Extracting location names:")
    logs$Location <- pbapply::pbsapply(strsplit(pbapply::pbsapply(strsplit(logs$FileName, split="/"), tail, n=1),"-"), `[`, 1)
    active_dates <- as.data.frame(table(logs$Date,logs$Location)) # Create table of dates
    colnames(active_dates) <- c("Date","Location","Log_Count") # Rename columns sensibly
    active_dates$Date = lubridate::ymd(active_dates$Date) # Convert dates to dates again
    rm(logs)
  #  Sites <- as.data.frame(unique(active_dates$Location))
  #  colnames(Sites) <- c("Location")
  #  date_range <- as.data.frame(rep(seq(as.Date(monitoring_start),as.Date(monitoring_end), by = 1), each = length(Sites$Location)))
  #  colnames(date_range) <- c("Date")
  #  Sites <- do.call("rbind", replicate(length(unique(date_range$Date)), Sites, simplify = FALSE))
  #  date_range <- cbind(date_range, Sites)
  #  active_dates <- merge(date_range, active_dates, all.x = T)
  #  active_dates[is.na(active_dates)] <- 0
  # active_dates <- active_dates[order(active_dates$Location),]
  #  active_dates$Active <- ifelse(active_dates$Log_Count == 0, "N", "Y")
  #  active_dates$Location <- sub("_", " ", active_dates$Location)
  }
  rm(WA_file_list)
  
  ###
  ### Inport Anabat Swift log files and convert to a single neat data.frame for further processing
  ###
  
  swift_file_list <- list.files(log_path, recursive = T, pattern = "*.csv") # Creates a list of files in the folder
  if (length(swift_file_list) > 1) {
    message("Anabat Swift log files found. Reading files:")
    output <- as.data.frame(swift_file_list)
    output$Date <- strptime(gsub(".*/", "", output$swift_file_list), "log %Y-%m-%d")
    output$Location <- sub("\\/.*", "", output$swift_file_list)
    output$Log_Count <- NA
    output$Active <- NA
    row_iterator <- 1
    for (file in output$swift_file_list){
      file_data <- read.csv(paste(log_path, "/", file, sep = ""))
      
      mic_fails <- length(grep("Status: Check microphone", file_data[,3]))
      files <- length(grep("FILE", file_data[,2]))
      recording <- any(grepl("Status: Recording now", file_data[,3]))
      
      if (mic_fails > 1 & files < 10) {
        output[row_iterator, 5] <- "N"
      } else if (mic_fails > 1 | isFALSE(recording)) {
        output[row_iterator, 5] <- "N"
      } else {
        output[row_iterator, 5] <- "Y"
      }
      
      row_iterator <- row_iterator + 1
      #output[nrow(output)+1,] <- c(as.character(strptime(gsub(".*/", "", file), "log %Y-%m-%d")), sub("\\/.*", "", file), 0, any(grepl("Status: Check microphone", file_data[,3])))
      #rm(file_data)
    }
    #sites2 <- as.data.frame(unique(output$Location))
    #colnames(sites2) <- c("Location")
    #date_range2 <- as.data.frame(rep(seq(as.Date(monitoring_start),as.Date(monitoring_end), by = 1), each = length(sites2$Location)))
    #colnames(date_range2) <- c("Date")
    #sites2 <- do.call("rbind", replicate(length(unique(date_range2$Date)), sites2, simplify = FALSE))
    #date_range2 <- cbind(date_range2, sites2)
    output$Date <- as.Date(output$Date)
    #output <- merge(date_range2, output, all.x = T)
    output[is.na(output)] <- "N"
    #output$Microphone.Failure[output$Microphone.Failure == 0] <- TRUE
    #output$Active <- ifelse(output[,4]==F, "Y", "N")
    #output$Microphone.Failure <- NULL
    output$swift_file_list <- NULL
    output$Log_Count <- ifelse(output[,4]=="Y", 1, 0)
    output$Active <- NULL
  }
  if (exists("active_dates") & exists("output")) {
    active_dates <- rbind(active_dates, output)
  } else if (!exists("active_dates") & exists("output")) {
    active_dates <- output
  } else {
    active_dates <- active_dates
  }
  
  ### Add Gap Dates
  
  monitoring_start <- if (is.null(monitoring_start)) {monitoring_start <- min(unique(active_dates$Date))}
  monitoring_end <- if (is.null(monitoring_end)) {monitoring_end <- max(unique(active_dates$Date))}
  
  sites <- as.data.frame(unique(active_dates$Location))
  colnames(sites) <- c("Location")
  date_range <- as.data.frame(rep(seq(as.Date(monitoring_start),as.Date(monitoring_end), by = 1), each = length(sites$Location)))
  colnames(date_range) <- c("Date")
  sites <- do.call("rbind", replicate(length(unique(date_range$Date)), sites, simplify = FALSE))
  date_range <- cbind(date_range, sites)
  # output$Date <- as.Date(output$Date)
  active_dates <- merge(date_range, active_dates, all.x = T)
  active_dates$Log_Count[is.na(active_dates$Log_Count)] <- 0
  
  
  # active_dates <- subset(active_dates, active_dates$Log_Count > 0)
  .save_to_RDATA(active_dates, data_path)
}