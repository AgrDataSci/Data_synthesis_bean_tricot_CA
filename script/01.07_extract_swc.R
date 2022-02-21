library(terra)
library(readr)


tricot_data <- readr::read_csv("data/red_beans_data.csv")
head(tricot_data)

data_path <- ("--Put source location here.../swc_fr")
swc_filename <- "w001001x.adf"

extract_point <- function(.month_to_extract, .coordinates_df){
  
  month_folder <- paste0("swc_fr_", .month_to_extract)
  
  print(month_folder)
  
  raster_file_path <- paste(data_path, month_folder, swc_filename, sep = "/")
  
  raster_to_read <- terra::rast(x = raster_file_path)
  
  swc_data <- terra::extract(x = raster_to_read, .coordinates_df, factors = F)[, 2]
  
  return(swc_data)
  
}


mean_swc <- vector(length = nrow(tricot_data), mode = "numeric")

for(i in 1:nrow(tricot_data)){
  
  print(i)
  months_to_extract <- lubridate::month(seq.Date(as.Date(tricot_data$pdate[i]), as.Date(tricot_data$hdate[i]), by = "month"))
  mean_swc_plot <- vector(length = length(months_to_extract))
  
  for (j in 1:length(mean_swc_plot)){
    print(j)
    mean_swc_plot[[j]] <- extract_point(months_to_extract[j], data.frame("x" = tricot_data$lon[i], "y" = tricot_data$lat[i]))
  }
  mean_swc[[i]] <- mean(mean_swc_plot)
  
  
}

write_csv(data.frame("mean_swc" = mean_swc), file = "--Put target location here.../mean_swc.csv")




