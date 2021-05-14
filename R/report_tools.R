one_site_short_report <- function(data_path, project_name, species_list) {
    rmarkdown::render(input = system.file("rmd", "One_Site_Short_Report.Rmd", package = "batr"), 
                    output_file = paste0("/Users/toby/Documents/0 - Inbox/Bat_Test_Dataset/Test_" , project_name, ".pdf"),
                    params = list(project = project_name, data = data_path))
}

#'Create Multi Site Report
#'
#'@param RData file
#'
#'@return A PDF report. 
#'
#'@examples
#'\dontrun{
#' Insert Example!
#'}
#'@export
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
            suppressWarnings(ggplot2::aes(label = Location)),
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
.quick_summary <- function (observations, active_dates = NULL, species = NULL) {
  if (!is.null(species)) { observations_s <- data.table::setDT(observations)[Species %chin% species] } else {observations_s <- observations}
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

.speces_site_plot <- function(observations, active_dates, species = NULL, sites = NULL, text_size = 8, date_label = "%b") {
  
  if (!is.null(species)) { observations_s <- data.table::setDT(observations)[Species %chin% species] }
  if (!is.null(sites)) {observations_s <- data.table::setDT(observations_s)[Location %chin% sites] }
  if (!is.null(sites)) {active_dates_s <- data.table::setDT(active_dates)[Location %chin% sites] }
  
  
  species_night_site <- aggregate(observations_s$Species, list(
    Night = observations_s$Night, Species = observations_s$Species, Location = observations_s$Location, Latitude = observations_s$Latitude, Longitude = observations_s$Longitude), FUN=length) # Create intial table
  names(species_night_site)[names(species_night_site) == "x"] <- "Count"
  
  
  active_dates_s <- active_dates_s[order(active_dates_s$Location),]
  
  failed_dates <- active_dates_s[which(active_dates_s$Log_Count==0),]
  #failed_dates$Active <- NULL
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
    butthead <- failed_dates[failed_dates$Gaps == fail,1]
    activity[fail,3] <- min(butthead$Date)
    fail <- fail + 1
  } # Compute end date for each break
  activity$xmin <- as.Date(activity$xmin, origin = "1970-01-01") # Convert to date
  fail <- 1 # Reset fail variable
  activity$xmax <- 0 # Create empty column for xmin
  while (fail < (maximum_fail+1)) {
    butthead <- failed_dates[failed_dates$Gaps == fail,1]
    activity[fail,4] <- max(butthead$Date)
    fail <- fail + 1
  } # Compute start date for each break
  activity$xmax <- as.Date(activity$xmax, origin = "1970-01-01") # Convert to date
  activity$ymax <- Inf
  activity$ymin <- 0
  activity$Gaps <- NULL
  #assign(paste("Gaps_", dataset_name, sep = ""),activity,.GlobalEnv)
  
  
  # Create labeller to provide more verbose labels on the plot facets
  species.labs <- c("Big Brown Bat", "Eastern Red Bat", "Hoary Bat", "Silver-haired Bat", "Myotis Spp.",
                    "Tri-colored Bat", "Eastern Small-footed Myotis", "Little Brown Myotis", "Northern Myotis",
                    "All Myotis Combined")
  names(species.labs) <- c("Epfu", "Labo", "Laci", "Lano", "Mysp", "Pesu", "Myle", "Mylu", "Myse", "Mysp_all")
  
  # Create plot
  species_site_plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = species_night_site, mapping = ggplot2::aes(x = Night, y = Count, fill = Location), stat = "identity") + #, fill = "black") +
    ggplot2::scale_y_continuous(name = "Calls per Night", breaks = scales::pretty_breaks(n = 2)) +
    ggplot2::scale_x_date(name = ggplot2::element_blank(), limits = c(as.Date(min(observations_s$Night)),as.Date(max(observations_s$Night))), breaks = scales::pretty_breaks(), date_breaks = "1 month", date_labels =  date_label) +
    #date_breaks = "1 month", date_labels =  "%b %Y") + # limits = c(as.Date("2017-05-01."),as.Date("2017-11-30")),
    #ggplot2::ggtitle("Total Activity of Bats") +
    ggplot2::geom_rect(data=activity, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha=0.9),
                       show.legend = FALSE) + # Add gaps if available
    ggplot2::facet_grid(
      Species~Location, scales = "free_y",
      labeller = ggplot2::labeller(Species = species.labs)) +
    ggplot2::geom_hline(yintercept=0) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(size=text_size),
      legend.position = "none"
    ) 

  return(species_site_plot)
}


