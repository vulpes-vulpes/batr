#'Read GUANO Metadata From Files
#'
#'\code{GUANO_reader} reads available GUANO metadata embedded in a folder of bat
#'acoustic. wav files processed by SonoBat. It creates a text file output of
#'this metadata in the parent folder of the files that can be loaded for further
#'analyses by \code{GUANO_loader}.
#'
#'@section Note: This function will take a long time for large datasets. Folders
#'  containing tens of thousands of files may take several hours to read!
#'  However, once complete this action does not need to repeated unless any
#'  changes are made to the original files. For this reason it is recommended to
#'  complete all manual vetting before proceeding.
#'
#'@family Import Functions
#'
#'@param folderpath Character, a path to the folder containing the files you
#'  wish to read the metadata from.
#'@param project_name Character, a brief and relevant reference phrase that will be used
#'  to name the text file.
#'
#'@return A textfile in the parent folder of the files folder.
#'
#'@examples
#'\dontrun{
#' GUANO.reader("C:/Folder/Folder/File_Folder", "Project_NA")
#'}
#'@export
GUANO_reader <- function(folderpath, project_name) {
  data <- guano::read.guano.dir(folderpath, recursive = TRUE) # Extract GUANO metadata
  write.table(data, paste(folderpath, "/", project_name, ".txt", sep = ""), sep="\t", row.names = F) # Save to text file
  message("Data Extraction Complete!")
}

#'Load Previously Read GUANO Metadata From Text File
#'
#'\code{GUANO_loader} imports a text file of GUANO metadata created by
#'\code{GUANO-reader} and created a number of dataframes to provide summaries
#'and facilitate further analyses.
#'
#'@family Import Functions
#'
#'@param file Character, a \strong{full path} to a text file created by
#'  GUANO.reader.
#'@param dataset_name Character, a brief and relevant reference phrase that will
#'  be appended to the function outputs for identification.
#'@param town.locaion Numeric. Selects the format used to record location in
#'  file metadata. \strong{1:} (default) location recorded in 'SB.Town' field.
#'  \strong{2:} location recored in 'Town' field - legacy from older versions of
#'  SonoBat Data Wizard. \strong{3:} location recorded in 'SB.Location' -
#'  required for some TRCA data.
#'
#'@return A series of dataframes summarising metadata from the files and
#'  facilitating further analyses and plots
#'
#'@examples
#'\dontrun{
#' GUANO.loader("C:/Folder/Folder/Project_Name.txt", "Project_Name")
#'}
#'@export
GUANO_loader <- function (file, project_name, town.location = 1) {

  # Import the textfile of raw data for the folder:
  data <- read.table(file, header = T, sep="\t")

  # Convert the Timestamp column to Date-Time format:
  data$Timestamp <- as.POSIXct(data$Timestamp, tz = "EST", "%Y-%m-%d %H:%M:%S")

  # Create a Date column
  data$Date <- as.Date(data$Timestamp, tz = "EST")

  # Create Night column, modifying AM recordings to previous date
  data <- within(data, {
    Night = ifelse(lubridate::hour(data$Timestamp) > 11, data$Date, data$Date - 1)
  })
  data$Night <- as.Date(data$Night,origin = "1970-01-01")

  # Create new column with manual ID if available, or Sonobat ID if not
  data = within(data, {
    Species = ifelse(is.na(data$Species.Manual.ID), as.character(data$Species.Auto.ID), as.character(data$Species.Manual.ID))
  })
  data <- data[which(data$Species=="Epfu"|data$Species=="Labo"|data$Species=="Laci"|data$Species=="Lano"|data$Species=="Myle"|data$Species=="Mylu"|data$Species=="Myse"|data$Species=="Mysp"|data$Species=="Pesu"),]

  # Merge Town and SB|Town columns to account for the way different versions of SonoBat name these columns!
  data$Location <- sapply(strsplit(as.character(data$File.Name),"-"), `[`, 1)
  if (town.location == 1) {
    data$Location2 <- as.factor(data$SB.Town)
  } else if (town.location == 2) {
    data <- dplyr::mutate(data, Location2 = as.factor(ifelse(is.na(Town),as.character(SB.Town), as.character(Town))))
  } else if (town.location == 3)
    data$Location2 <- as.factor(data$SB.Location)
  # Change column names as required:
  #colnames(data)[colnames(data)=="Town"] <- "Location"
  colnames(data)[colnames(data)=="Loc.Position.Lat"] <- "Latitude"
  colnames(data)[colnames(data)=="Loc.Position.Lon"] <- "Longitude"
  colnames(data)[colnames(data)=="File.Name"] <- "Filename"

  # Fix wandering location data for Swifts:
  swift_files <- data[data$Model == 'Swift',] # Subset files recorded with Anabat Swift
  location_list <- unique(swift_files$Location) # Find locations
  for (Location in location_list) {
    location_subset           <- swift_files[swift_files$Location == Location,] # Subset once more by location
    location_subset$Latitude  <- location_subset[1,"Latitude"] # Replace all lat values with first instance
    location_subset$Longitude <- location_subset[1,"Longitude"] # Replace all lon values with first instance
    if (!exists("swift_files_mod")) {
      swift_files_mod <- location_subset
    } else {
      swift_files_mod <- rbind(swift_files_mod, location_subset)
    } # Create modified data.frame if it doesn't exist, or apend if it does
  }
  data <- data[!(data$Model == 'Swift'),] # Remove modified rows
  if (exists("swift_files_mod")) {
    data <- rbind(data, swift_files_mod) # Replace removed rows with modified rows
  }

  # Create new dataframe with only select columns, and remove rows without species ID
  clean_data <- subset(data, select=c(Timestamp, Night, Location, Location2, Species, Latitude, Longitude, Filename))
  clean_data <- clean_data[!is.na(clean_data$Species),] # Remove blank rows
  clean_data$Loc_Year <- paste(lubridate::year(clean_data$Night), clean_data$Location2, sep = " ")

  # Suspended code to creat dataset of only PM observations:
  # raw_data$Time <- strftime(raw_data$Date_Time, format="%H:%M:%S", tz = "GMT")
  # raw_data$Time_PM <- ifelse(raw_data$Time<"12:00:00",NA,raw_data$Time)
  # raw_data <<- raw_data[order(raw_data$Date_Time),]
  # dataset_PM <<- raw_data[!is.na(raw_data$Time_PM),]

  # Output datasets to environment:
  assign(paste("clean_data_", project_name, sep = ""), clean_data, envir=globalenv())
  assign(paste("raw_data_", project_name, sep = ""), data, envir=globalenv())

  # Create Summary Table:
  species_site_summary <- as.data.frame.matrix(table(clean_data$Loc_Year,clean_data$Species)) # Create table of Species by Location
  species_site_summary <- cbind(Location = rownames(species_site_summary), species_site_summary) # Fix row names to column
  rownames(species_site_summary) <- NULL # with above
  # Create column for each Ontario species in case they were not recorded
  if(!"Epfu" %in% colnames(species_site_summary)) { species_site_summary$Epfu <- 0}
  if(!"Labo" %in% colnames(species_site_summary)) { species_site_summary$Labo <- 0}
  if(!"Laci" %in% colnames(species_site_summary)) { species_site_summary$Laci <- 0}
  if(!"Lano" %in% colnames(species_site_summary)) { species_site_summary$Lano <- 0}
  if(!"Myle" %in% colnames(species_site_summary)) { species_site_summary$Myle <- 0}
  if(!"Mylu" %in% colnames(species_site_summary)) { species_site_summary$Mylu <- 0}
  if(!"Myse" %in% colnames(species_site_summary)) { species_site_summary$Myse <- 0}
  if(!"Mysp" %in% colnames(species_site_summary)) { species_site_summary$Mysp <- 0}
  if(!"Pesu" %in% colnames(species_site_summary)) { species_site_summary$Pesu <- 0}
  species_site_summary$`All Myotis` <- rowSums(species_site_summary[, c("Mylu", "Myle", "Myse", "Mysp")])
  species_site_summary$Year = substr(species_site_summary$Location, 1, 4)
  species_site_summary$Location <- substring(species_site_summary$Location, 6, 1000000L)
  myNumCols <- which(unlist(lapply(species_site_summary, is.numeric)))
  species_site_summary[(nrow(species_site_summary) + 1), myNumCols] <- colSums(species_site_summary[, myNumCols], na.rm=TRUE)
  x <- as.numeric(length(species_site_summary[,1]))
  species_site_summary[x,1] = "Totals"
  #colnames(species_site_summary)[colnames(species_site_summary) == 'Location2'] <- 'Location'
  #species_site_summary <- species_site_summary[c("Location", "Epfu", "Labo", "Laci", "Lano", "Myle", "Mylu", "Myse", "Mysp", "All Myotis", "Pesu")]
  assign(paste("species_site_summary_", project_name, sep = ""), species_site_summary, envir=globalenv())

  # Create table for further uses:
  species_night_site <- aggregate(clean_data$Species, list(
    Night = clean_data$Night, Species = clean_data$Species, Location = clean_data$Location2, Latitude = clean_data$Latitude, Longitude = clean_data$Longitude), FUN=length) # Create intial table
  names(species_night_site)[names(species_night_site) == "x"] <- "Count"

  # Create a subset with all Myotis bats to plot as one:
  Myotis_subset <- species_night_site[which(species_night_site$Species== "Mylu" | species_night_site$Species== "Myle" | species_night_site$Species== "Myse" |species_night_site$Species==  "Mysp" ),]
  if(nrow(Myotis_subset)>0) {
    Myotis_subset <- aggregate(Count~Night+Location+Latitude+Longitude,data=Myotis_subset,FUN=sum)
    Myotis_subset$Species <- "Mysp_all"
    species_night_site <- rbind(Myotis_subset, species_night_site)
  }

  species_night_site$Day_of_Year <- lubridate::yday(species_night_site$Night)
  assign(paste("species_night_site_", project_name, sep = ""), species_night_site, envir=globalenv())
}

#'Import and Interpret Wildlife Acoustics and Anabat Log Files
#'
#'\code{log_file_parser} automatically ingests the logfiles created by Wildife
#'Acoustics SM3 and SM4, and Anabat Swift acoustic monitoring units. It
#'identifies, by site, whether a recorder was active on each night in the
#'monitoring period, creates a data frame of 'gaps' for use in plots, and
#'calcualtes uptime for each monitor.
#'
#'@section Note: To work as expected, log files must be renamed following the
#'  conventions outlined below!
#'
#'  \strong{Wildlife Acoustics:} These files should be renamed so that each
#'  filename begins with the site name followed by a hyphen and any additional
#'  information (e.g. date) that is desired in the file name. For example:
#'  "site_1-2019.txt". The hyphen and all characters that follow it will be
#'  disregarded. The site name should match the site name assigned in GUANO
#'  metadata so that the log file data is appropriately matched with the
#'  acoustic data.
#'
#'  \strong{Anabat Swift:} These fiels do not need to be renamed, but files for
#'  each site should be placed in their own sub-folder. The folder name should
#'  match the site name assinged in the GUANO metadata of the corrisponding .wav
#'  files so they can be matched.
#'
#'@family Import Functions
#'
#'@param path Character, a path to a parent directory containing all the
#'  relevant log files, following the filename convention described in Note.
#'@param dataset_name Character, a brief and relevant reference phrase that will
#'  be used to name the text file.
#'@param monitoring_start Character, the date the monitoring began (e.g.
#'  "2019-01-01")
#'@param monitoring_end Character, the date the monitoring ceased (e.g.
#'  "2019-01-01")
#'
#'@return A series of dataframes summarising activity and uptimes from the files
#'  and facilitating further analyses and plots
#'
#'@examples
#'\dontrun{
#' log_file_parser("C:/Folder/Folder/Log_Files_Folder", "Project_Name", "2019-01-01", "2019-12-31")
#'}
#'@export
log_file_parser <- function(path, dataset_name, monitoring_start, monitoring_end) {

  ###
  ### Import Wildlife Acoustics log files and convert to single neat data.frame for further processing
  ###

  file_list <- list.files(path, recursive = T, pattern = "*.txt") # Creates a list of files in the folder
  if (length(file_list) > 0.5) {
    for (file in file_list){
      # if the merged dataset doesn't exist, create it
      if (!exists("logs")){
        logs <- read.table(textConnection(gsub(",", "\t", readLines(paste(path, "/", file, sep = "")))), header=TRUE, sep="\t", comment.char = "")
        logs <- subset(logs, select = c(DATE, TIME))
        logs$Location <- sapply(strsplit(file,"-"), `[`, 1)
        #if (recursive == T) {
        #  logs$Location <- sapply(strsplit(logs$Location,'/'), `[`, 2)
        #}
      }
      # if the merged dataset does exist, append to it
      if (exists("logs")){
        temp <- read.table(textConnection(gsub(",", "\t", readLines(paste(path, "/", file, sep = "")))), header=TRUE, sep="\t", comment.char = "")
        temp <- subset(temp, select = c(DATE, TIME))
        temp$Location <- sapply(strsplit(file,"-"), `[`, 1)
        #if(recursive == T) {
        #  temp$Location <- sapply(strsplit(temp$Location,'/'), `[`, 2)
        #}
        logs<-rbind(logs, temp)
      }
      rm(temp)
    } # Loop to read and neatly arrange log files
    names(logs)[names(logs) == 'DATE'] <- 'Date' # Fix name of date column
    logs$Date = lubridate::ymd(logs$Date) # Convert dates from factor to 'dates'
    active_dates <- as.data.frame(table(logs$Date,logs$Location)) # Create table of dates
    colnames(active_dates) <- c("Date","Location","Log_Count") # Rename columns sensibly
    active_dates$Date = lubridate::ymd(active_dates$Date) # Convert dates to dates again
    Sites <- as.data.frame(unique(active_dates$Location))
    colnames(Sites) <- c("Location")
    date_range <- as.data.frame(rep(seq(as.Date(monitoring_start),as.Date(monitoring_end), by = 1), each = length(Sites$Location)))
    colnames(date_range) <- c("Date")
    Sites <- do.call("rbind", replicate(length(unique(date_range$Date)), Sites, simplify = FALSE))
    date_range <- cbind(date_range, Sites)
    active_dates <- merge(date_range, active_dates, all.x = T)
    active_dates[is.na(active_dates)] <- 0
    active_dates <- active_dates[order(active_dates$Location),]
    active_dates$Active <- ifelse(active_dates$Log_Count == 0, "N", "Y")
    active_dates$Location <- sub("_", " ", active_dates$Location)
  }

  ###
  ### Inport Anabat Swift log files and convert to a single neat data.frame for further processing
  ###

  file_list2 <- list.files(path, recursive = T, pattern = "*.csv") # Creates a list of files in the folder
  if (length(file_list2) > 1) {
    output <- as.data.frame(file_list2)
    output$Date <- strptime(gsub(".*/", "", output$file_list2), "log %Y-%m-%d")
    output$Location <- sub("\\/.*", "", output$file_list2)
    output$Log_Count <- NA
    output$Active <- NA
    row_iterator <- 1
    for (file in output$file_list2){
      file_data <- read.csv(paste(path, "/", file, sep = ""))
      
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
    sites2 <- as.data.frame(unique(output$Location))
    colnames(sites2) <- c("Location")
    date_range2 <- as.data.frame(rep(seq(as.Date(monitoring_start),as.Date(monitoring_end), by = 1), each = length(sites2$Location)))
    colnames(date_range2) <- c("Date")
    sites2 <- do.call("rbind", replicate(length(unique(date_range2$Date)), sites2, simplify = FALSE))
    date_range2 <- cbind(date_range2, sites2)
    output$Date <- as.Date(output$Date)
    output <- merge(date_range2, output, all.x = T)
    output[is.na(output)] <- "N"
    #output$Microphone.Failure[output$Microphone.Failure == 0] <- TRUE
    #output$Active <- ifelse(output[,4]==F, "Y", "N")
    #output$Microphone.Failure <- NULL
    output$file_list2 <- NULL
    output$Log_Count <- ifelse(output[,4]=="Y", 1, 0)
  }
  if (exists("active_dates") & exists("output")) {
    active_dates <- rbind(active_dates, output)
  } else if (!exists("active_dates") & exists("output")) {
    active_dates <- output
  } else {
    active_dates <- active_dates
  }

  active_dates <- active_dates[order(active_dates$Location),]

  ###
  ### Monthly Active Date Outputter
  ###

  active_subset <- active_dates[active_dates$Active %in% c("Y","P"), ]
  #monthly_active_nights <- data.frame(table(active_subset$Location, lubridate::month(active_subset$Date)))
  monthly_active_nights <- reshape2::dcast(active_subset, lubridate::year(Date) + Location ~ lubridate::month(Date))
  colnames(monthly_active_nights)[1] <- "Year"
  n <- ncol(monthly_active_nights)
  monthly_active_nights$Total_Nights <- rowSums(monthly_active_nights[,3:n])
  assign(paste("monthly_active_nights_", dataset_name, sep = ""), monthly_active_nights, envir=globalenv())
  assign(paste("active_dates_", dataset_name, sep = ""), active_dates, envir=globalenv())

  ###
  ### Gaps caculator - calculates dates when recorders weren't active, and outputs as a table that can be used to overlay data on bar chart
  ###

  failed_dates <- active_dates[which(active_dates$Log_Count==0),]
  failed_dates$Active <- NULL
  failed_dates$Gaps <- 0 # Create Gaps Column
  failed_dates[1,4] <- 1 # Set first row to 1
  row <- 2 # Set iterator to second row
  maximum_row <- length(failed_dates$Gaps) + 1 # Calculate maximum
  while (row < maximum_row) { # While loop to add consequtive labels based on date differneces
    rowdown <- row - 1 # Create value for row - 1
    if (failed_dates[row,1] - failed_dates[rowdown,1] == 1) { #If dates are consecutive, set same as row above
      failed_dates [row,4] <- failed_dates[rowdown,4]
    } else { # Else add one
      failed_dates[row,4] <- failed_dates[rowdown,4] + 1
    }
    row <- row + 1 # Interate row
  } # While loop to iterate gap numbers
  maximum_fail <- max(failed_dates$Gaps) # Calculate number of gaps
  activity <- failed_dates # Copy dates without recordings to a new data frame
  activity$Date <- NULL # Remove unnecessary column
  activity$Log_Count <- NULL # Remove unnecessary column
  activity <- unique(activity) # Remove duplicate rows
  fail <- 1 # Create fail variable
  activity$xmin <- 0 # Create empty column for xmax
  while (fail < (maximum_fail+1)) {
    activity[fail,3] <- min(failed_dates[failed_dates$Gaps == fail,1])
    fail <- fail + 1
  } # Compute end date for each break
  activity$xmin <- as.Date(activity$xmin, origin = "1970-01-01") # Convert to date
  fail <- 1 # Reset fail variable
  activity$xmax <- 0 # Create empty column for xmin
  while (fail < (maximum_fail+1)) {
    activity[fail,4] <- max(failed_dates[failed_dates$Gaps == fail,1])
    fail <- fail + 1
  } # Compute start date for each break
  activity$xmax <- as.Date(activity$xmax, origin = "1970-01-01") # Convert to date
  activity$ymax <- Inf
  activity$ymin <- 0
  activity$Gaps <- NULL
  assign(paste("Gaps_", dataset_name, sep = ""),activity,.GlobalEnv)

  ###
  ### Uptime calculations - calculates real and total uptimes for all locations and outpues as a dataframe to the global environment
  ###

  uptimes <- unique(active_dates$Location) # Create list of locations included
  uptimes <- as.data.frame(uptimes) # Convert list of locations to dataframe
  row <- 1 # Set row to 1
  rowmax <- length(uptimes$uptimes) + 1 # Calcualte maximum row
  uptimes$Failed_Nights <- 0 # Create column for the number of nights active
  while (row < rowmax) {
    uptimes[row,2] <- sum(failed_dates$Location == uptimes[row,1])
    row <- row + 1
  } # Loop to calcualte uptimes for each location on the list
  uptimes$Total_Up <- length(seq(as.Date(monitoring_start),as.Date(monitoring_end), by = 1)) # Set monitoring period boundaries to calculate total uptime
  uptimes$Real_Up <- length(seq(min(active_dates[,1]),max(active_dates[,1]), by = 1)) # Create range of dates detector was active for
  uptimes[, "Total_Uptime"] <- ((uptimes[, "Total_Up"] - uptimes[,"Failed_Nights"]) / uptimes[, "Total_Up"])*100 # Calculate the percentages
  uptimes[, "Real_Uptime"] <- ((uptimes[, "Real_Up"] - uptimes[,"Failed_Nights"]) / uptimes[, "Total_Up"])*100 # Calculate the percentages
  uptimes$Real_Up <- NULL # Remove superfluous columns
  uptimes$Total_Up <- NULL # Remove superfluous columns
  assign(paste("uptimes", dataset_name, sep = ""),uptimes,.GlobalEnv) # Calculate uptime for monitoring period
}
