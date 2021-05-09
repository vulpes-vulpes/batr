one_site_short_report <- function(data_path, project_name, species_list) {
    rmarkdown::render(input = system.file("rmd", "One_Site_Short_Report.Rmd", package = "batr"), 
                    output_file = paste0("/Users/toby/Documents/0 - Inbox/Bat_Test_Dataset/Test_" , project_name, ".pdf"),
                    params = list(project = project_name, data = data_path))
}

multi_site_short_report <- function(data_path, project_name, species_list) {
  rmarkdown::render(input = system.file("rmd", "One_Site_Short_Report.Rmd", package = "batr"), 
                    output_file = paste0("/Users/toby/Documents/0 - Inbox/Bat_Test_Dataset/Test_" , project_name, ".pdf"),
                    params = list(project = project_name, data = data_path))
}

.map4report <- function(observations, labels = T) {
  locations <- as.data.frame(cbind(unique(observations$Location), unique(observations$Longitude), unique(observations$Latitude)))
  colnames(locations) <- c("Location", "Longitude","Latitude")
  locations$Latitude <- as.numeric(locations$Latitude)
  locations$Longitude <- as.numeric(locations$Longitude) 
   
  map <- ggmap::qmplot(x = Longitude, y = Latitude, data = locations, maptype = "toner-lite", extent = "normal") + 
         if (labels == T) {   
            ggrepel::geom_text_repel(data = locations,
             ggplot2::aes(label = Location),
             size=3)
         }
  return(map)
}

#'Create quick summary table
#'
#'@importFrom data.table %chin%
#'
#'@param observations Observations data frame. 
#'@param active_dates Optional active_dates data frame
#'
#'@return A quick summary table of total species observations and survey effort.
#'
.quick_summary <- function (observations, active_dates = NULL) {
  observations_s <- data.table::setDT(observations)[Species %chin% species]
  observations_s$Loc_Year <- paste(lubridate::year(observations_s$Night), observations_s$Location, sep = " ")
  quick_summary <- as.data.frame.matrix(table(observations_s$Loc_Year, observations_s$Species)) # Create table of Species by Location
  quick_summary <- cbind(Location = rownames(quick_summary), quick_summary) # Fix row names to column
  rownames(quick_summary) <- NULL # with above
  headers <- c("Location", species)
  missing <- setdiff(headers, names(quick_summary)) 
  quick_summary[missing] <- 0                    
  quick_summary <- quick_summary[headers]
  quick_summary$Year = substr(quick_summary$Location, 1, 4)
  quick_summary$Location <- substring(quick_summary$Location, 6, 1000000L)
  quick_summary$Monitoring_Nights <- sapply(quick_summary$Location, function(l) nrow(active_dates[active_dates$Location==l,]))
  
  #div <- sapply(quick_summary$Location, function(l) nrow(active_dates[active_dates$Location==l,])) # , quick_summary$Location)
  #div <- as.data.frame(div)
  
  #averages <- as.data.frame(mapply('/', quick_summary[species], div))
  
  return(quick_summary)
}