one_site_short_report <- function(data_path, project_name, species_list) {
    rmarkdown::render(input = system.file("rmd", "One_Site_Short_Report.Rmd", package = "batr"), 
                    output_file = paste0("/Users/toby/Documents/0 - Inbox/Bat_Test_Dataset/Test_" , project_name, ".pdf"),
                    params = list(project = project_name, data = data_path))
}

.map4report <- function(observations) {
  locations <- as.data.frame(cbind(unique(observations$Longitude), unique(observations$Latitude)))
  colnames(locations) <- c("Longitude","Latitude")
  map <- ggmap::qmplot(Longitude, Latitude, data = locations, maptype = "toner-lite", extent = "normal")
  return(map)
}
