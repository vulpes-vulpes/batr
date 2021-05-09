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

