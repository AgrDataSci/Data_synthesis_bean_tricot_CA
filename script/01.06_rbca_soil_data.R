library(raster)

#load administrative layers
source("script/oldscripts/load_adm_layers.R")

source("script/oldscripts/get_bbox_gdal.R")

source("script/oldscripts/get_soil_data.R")

#bounding box for whole Central America
ca_bbox <- get_bbox_gdal(ca_adm_0_ext)

cri_bbox <- get_bbox_gdal(cri_adm_0)

nic_bbox <- get_bbox_gdal(nic_adm_0)

hnd_bbox <- get_bbox_gdal(hnd_adm_0)

slv_bbox <- get_bbox_gdal(slv_adm_0)

depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm")

s_vars <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc", "ocd", "ocs")
#countries <- 


for(i in s_vars){
  for(j in depths){
    get_soilgrids(.voi = i,
                  .depth = j,
                  .quantile = "mean",
                  .bbox = hnd_bbox,
                  .output_folder = "data/soilgrids",
                  .prefix_file_name = "hnd")
     #Sys.sleep(5)
  }
  
}


get_soilgrids(.voi = "ocs",
              .depth = "0-5cm",
              .quantile = "mean",
              .bbox = hnd_bbox,
              .output_folder = "data/soilgrids",
              .prefix_file_name = "hnd"
              )


out_files <- regexec(pattern = "(bdod)|(0-5cm)", text = list.files("data/soilgrids/"))
        
out_files[[125]][]

library(stringr)
bdod_0_5_files <- stringr::str_subset(string = list.files("data/soilgrids/",full.names = TRUE), pattern = "bdod_0-5cm")

bdod_5_15_files <- stringr::str_subset(string = list.files("data/soilgrids/",full.names = TRUE), pattern = "bdod_5-15cm")

bdod_5_15_files <- stringr::str_subset(string = list.files("data/soilgrids/",full.names = TRUE), pattern = "bdod_15-30cm")

plot(s)
plot(dt_sf["id"], add = TRUE, color = "black")

ca_bdod_0_5_l <- lapply(bdod_0_5_files, raster::raster)

ca_bdod_0_5_r <- Reduce(raster::merge, ca_bdod_0_5_raster)

ca_bdod_5_15_l <- lapply(bdod_5_15_files, raster::raster)

ca_bdod_0_5_r <- Reduce(raster::merge, ca_bdod_0_5_raster)

ca_bdod_15_30_l <- lapply(bdod_5_15_files, raster::raster)

ca_bdod_0_5_r <- Reduce(raster::merge, ca_bdod_0_5_raster)





s <- Reduce(raster::merge, ca_bdod_05_mean_r)
s <- raster::projectRaster(s, crs = "EPSG:4326")

plot(s)

raster::ca_bdod_05_mean_r


cri_nitrogen_0_5 <- raster("data/soilgrids/cri_nitrogen_0-5cm_mean.tif")

nic_nitrogen_0_5 <- raster("data/soilgrids/nic_nitrogen_0-5cm_mean.tif")

cri_nic_nitrogen <- raster::merge(cri_nitrogen_0_5, nic_nitrogen_0_5)
plot(cri_nic_nitrogen)

phh2o_05 <- raster("data/soilgrids/phh2o_0-5cm_Q0.5.tif")
plot(phh2o_05)

phh2o_05_prj <- raster::projectRaster(phh2o_05,  crs = "EPSG:4326")


n_0_5_cr <- raster("data/soilgrids/nitrogen_0-5cm_mean.tif")
image(n_0_5_cr)
n_0_5_cr_prj <- raster::projectRaster(n_0_5_cr,  crs = "EPSG:4326")
image(n_0_5_cr_prj)


raster::extract(x = n_0_5_cr_prj, data.frame("x" = dt$lon[1], "y" = dt$lat[1]))

raster


