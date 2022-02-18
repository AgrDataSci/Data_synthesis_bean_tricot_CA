library(raster)
library(stringr)
library(readr)
library(sf)
library(dplyr)

s_vars <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc", "ocd", "ocs")

data_path <- "data/raw/soilgrids/"

depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm")

dt <- read_csv("data/red_beans_data.csv")

dt_coords <- data.frame("x" = dt$lon, "y" = dt$lat)

#Project trial coordinates to homolosine projection to match soilgrids data
proj_coords <- st_as_sf(x = dt_coords, coords = c("x", "y"), crs = 4326)

proj_coords

proj_coords <- st_transform(proj_coords, crs = "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs")

process_soil_data <- function(.var){
  
  if(.var %in% c("bdod", "nitrogen")){
    convers_factor <- 100
  }
  else
  {
    convers_factor <- 10
  }
  
  
  var_0_5_files <- stringr::str_subset(string = list.files(data_path, full.names = TRUE), 
                                         pattern = paste0(.var, "_0-5cm"))
  
  var_5_15_files <- stringr::str_subset(string = list.files(data_path, full.names = TRUE), 
                                        pattern = paste0(.var, "_5-15cm"))
  
  var_15_30_files <- stringr::str_subset(string = list.files(data_path, full.names = TRUE), 
                                         pattern = paste0(.var, "_15-30cm"))
  
  var_30_60_files <- stringr::str_subset(string = list.files(data_path, full.names = TRUE), 
                                         pattern = paste0(.var, "_30-60cm"))
  
  #0-15
  
  ca_var_0_5_l <- lapply(var_0_5_files, raster::raster)
  
  #message(paste("Processing layer: "), .var)
  
  ca_var_0_5_r <- Reduce(raster::merge, ca_var_0_5_l)
  
  ca_var_0_5_r <- ca_var_0_5_r / convers_factor
  
  #5-15
  
  ca_var_5_15_l <- lapply(var_5_15_files, raster::raster)
  
  ca_var_5_15_r <- Reduce(raster::merge, ca_var_5_15_l)
  
  ca_var_5_15_r <- ca_var_5_15_r / convers_factor
  
  
  # 15-30
  
  ca_var_15_30_l <- lapply(var_15_30_files, raster::raster)
  
  ca_var_15_30_r <- Reduce(raster::merge, ca_var_15_30_l)
  
  ca_var_15_30_r <- ca_var_15_30_r / convers_factor
  
  
  # 30-60
  
  ca_var_30_60_l <- lapply(var_30_60_files, raster::raster)
  
  ca_var_30_60_r <- Reduce(raster::merge, ca_var_30_60_l)
  
  ca_var_30_60_r <- ca_var_30_60_r / convers_factor
  
  
  output_rasters <- c(ca_var_0_5_r,
                      ca_var_5_15_r,
                      ca_var_15_30_r,
                      ca_var_30_60_r)
  
  names(output_rasters)
  
  names(output_rasters) <- c(paste0(.var, "_0-5cm"),
                             paste0(.var, "_5-15cm"),
                             paste0(.var, "_15-30cm"),
                             paste0(.var, "_30-60cm"))
  
  return(output_rasters)
  
}

# processed_layers <- list()
# 
# for(i in s_vars[2:5]){
# 
#   processed_layers[[i]] <- process_soil_data(i)
# 
# 
# }

#bdod_layers <- process_soil_data("bdod")

cec_layers <- process_soil_data("cec")

cfvo_layers <- process_soil_data("cfvo")

nitrogen_layers <- process_soil_data("nitrogen")

clay_layers <- process_soil_data("clay")

phh2o_layers <- process_soil_data("phh2o")

sand_layers <- process_soil_data("sand")

silt_layers <- process_soil_data("silt")

soc_layers <- process_soil_data("soc")

ocd_layers <- process_soil_data("ocd")

#ocs_layers <- process_soil_data("ocs")


#names(processed_layers)

var_names <- expand.grid(s_vars, depths)

var_names <- dplyr::arrange(var_names, var_names$Var1)

var_names <- paste(var_names$Var1, var_names$Var2, sep = "_")


extract_soil_values <- function(.var, .layer_set){
  
  vars_to_extract <-  var_names[which(str_detect(var_names, .var))]
  
  soil_data <- matrix(data = NA, nrow = nrow(dt), ncol = 4)
  
  colnames(soil_data) <- vars_to_extract
  
  progress_bar <- txtProgressBar(min = 0, max = nrow(dt), style = 3)

  for(i in 1:nrow(dt)){
    for(j in vars_to_extract){
      soil_data[i, j] <- raster::extract(x = .layer_set[[j]], y = proj_coords[i, ])
      Sys.sleep(0.1)
      setTxtProgressBar(progress_bar, i)
      
    }
  }  
 return(soil_data)
}

#names(processed_layers)

#no buffer
cec_data <- extract_soil_values("cec", cec_layers)
head(cec_data)
tail(cec_data)

cfvo_data <- extract_soil_values("cfvo", cfvo_layers)

clay_data <- extract_soil_values("clay", clay_layers)

nitrogen_data <- extract_soil_values("nitrogen", nitrogen_layers)

phh2o <- extract_soil_values("phh2o", phh2o_layers)

sand <- extract_soil_values("sand", sand_layers)

silt <- extract_soil_values("silt", silt_layers)

soc <- extract_soil_values("soc", soc_layers)

ocd <- extract_soil_values("ocd", ocd_layers)

#ocs <- extract_soil_values("ocs", ocs_layers)


rbca_soil <- cbind(cec_data, cfvo_data, clay_data, nitrogen_data, phh2o, sand, silt, soc, ocd)

head(rbca_soil)

rbca_soil <- janitor::clean_names(as.data.frame(rbca_soil))

write_csv(rbca_soil, file = "data/soilgrids.csv")



