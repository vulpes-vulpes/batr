path <- "/Users/toby/Documents/0 - Inbox/Bat_Test_Dataset"
new <- T
update <- F
verbose_observations <- F


read_wavs <- function(path, new = F, update = F, verbose_observations = F, species_list = c("Epfu", "Labo", "Laci", "Lano", "Myle", "Mylu", "Myse", "Mysp", "Pesu")) {

  ###
  ### Load / Create / Update Observations Table:
  ###
  
  new_2nd_choice = "N"
  update_2nd_choice = "x"
  
  ## If there is an existing files and no update, load existing observations into the environment:

  if (new == F && update == F) {
    if (file.exists(paste(path, "/BAM_Data.RData", sep = ""))) {
      load_observations(path)
    } else {
      new_2nd_choice <- readline(prompt="No 'BAM_Data.RData' found. Would you like to create one? Y/N: ")
      if (new_2nd_choice == "N" | new_2nd_choice == "n") {
        print("No observations available, exiting wihtout output.")
      }
    }
  }

  ## If new and no update, create and load observations file:
  
  if (new == T && update == F | new_2nd_choice == "Y" | new_2nd_choice == "y") {
    if (file.exists(paste(path, "/BAM_Data.RData", sep = ""))) {
      rebuild <- readline(prompt="Observations files already exists for this dataset. Would you like to completely rebuild the observations table? Y/N: ")
      if (rebuild == "Y" | rebuild == "y") {
        new_observations(path)
      } else if (rebuild == "N" | rebuild == "n") {
        update_2nd_choice <- readline(prompt="Update observations table instead? Y/N: ")
      } else {
        print("Invalid Response")
        exit()
      }
      if (update_2nd_choice == "N" | update_2nd_choice == "n") {
        print("No updates, loading existing observations table.")  
        load_observations(path)
      }
    } else {
      new_observations(path)
    }
  }

  ## Is observations exists but updates are true:
  
  if (update == T | update_2nd_choice == "Y" | update_2nd_choice == "y") {
    update_observations(path)
  }

}  
  
  
  ###
  ### Process Observations 
  ###
  
  # Convert the Timestamp column to Date-Time format:
  observations$Timestamp <- as.POSIXct(observations$Timestamp, tz = "EST", "%Y-%m-%d %H:%M:%S")
  
  # Create Night column, modifying AM recordings to previous date
  observations <- within(observations, {
    Night = ifelse(lubridate::hour(observations$Timestamp) > 11, 
                   as.Date(observations$Timestamp, tz = "EST"), 
                   as.Date(observations$Timestamp, tz = "EST") - 1)
  })
  observations$Night <- as.Date(observations$Night, origin = "1970-01-01")
  
  # Create new column with manual ID if available, or automatic ID if not
  observations = within(observations, {
    Species = ifelse(is.na(observations$Species.Manual.ID), as.character(observations$Species.Auto.ID), as.character(observations$Species.Manual.ID))
  })
  
  # Drop non species IDs:
  
  observations <- observations[observations$Species %in% species_list, ]
  
  # Rename size and modified date columns:
  
  names(observations)[36:37] <- c("File.Size","File.Modified")
  
  # Drop unnecessary columns and reorder:
  
  observations <- observations[-c(1, 35)]
  observations <- observations[c(1, 22, 36:37, 32:33, 10, 12, 11, 14:18, 23:26, 29:31, 4:9, 19:21, 27:28, 3, 34:35, 2)] # FIXME: This will only work with this exact metadata, should reorder by names!!!
  
  if (verbose_observations == F) {
    observations <- observations[-c(10:34)]
  }

  assign("observations", observations, envir=globalenv())
  

## FIXME: Generating a new data frame creates one fewer columns than loading a saved one!!

read_wavs(path, new = F, update = F)
read_wavs(path, new = T, update = F)
read_wavs(path, new = F, update = T)


