#Download soil data from SoilGrids

#Workflow based on the https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/wcs_from_R.md

#Load libraries
library(XML)
library(rgdal)
library(gdalUtils)

#constants
igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

wcs_service <- "SERVICE=WCS"
wcs_version <- "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.


#function to download soilgrids data
get_soilgrids <- function(.voi = NULL,
                          .depth = NULL,
                          .quantile = NULL,
                          .bbox = NULL,
                          .output_folder = NULL,
                          .prefix_file_name = NULL
                          ){
  
  
  file.remove(list.files(.output_folder, pattern = ".xml", full.names = T))
  
  #create string of variable of interest
  voi_layer <- paste(.voi, .depth, .quantile, sep = "_")
  
  wcs_path <- paste0("https://maps.isric.org/mapserv?map=/map/", .voi, ".map") # Path to the WCS. See maps.isric.org
  
  #create parameter for the wcs request
  wcs <- paste(wcs_path, wcs_service, wcs_version, sep = "&") # This works for gdal >= 2.3
  
  l1 <- newXMLNode("WCS_GDAL")
  l1.s <- newXMLNode("ServiceURL", wcs, parent = l1)
  l1.l <- newXMLNode("CoverageName", voi_layer, parent = l1)
  
  xml_file <- paste0("/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "conn.xml")

  xml.out <- paste0(.output_folder, xml_file)
  #xml.out <- "conn.xml"
  saveXML(l1, file = xml.out)
  
  if(!is.null(.prefix_file_name)){
    
    file_name <- paste(.prefix_file_name, voi_layer, sep = "_")
    file_name <- paste(file_name, "tif", sep = ".")
  }
  else
  {
    file_name <- paste(voi_layer, "tif", sep = ".")
  }

  full_file_name <- paste(.output_folder, file_name, sep = "/")
  
  message("Requesting to download the following layers: ")
  print(paste("wcs string: ", wcs))
  print(paste("voi_layer: ", voi_layer))
    print(paste("Output file: ", full_file_name))
  
  if(!file.exists(full_file_name)){
  gdal_translate(xml.out,
                 full_file_name,
                 tr = c(250, 250),
                 projwin = .bbox,
                 projwin_srs = igh,
                 co = c("TILED = YES", "COMPRESS = DEFLATE", "PREDICTOR = 2","BIGTIFF = YES"),
                 verbose = TRUE)
  }
    else{
      message("The requested layer already exist in the target location")
        
    }
  
  file.remove(list.files(.output_folder, pattern = ".xml", full.names = T))
  
}

#Example
# get_soilgrids(.voi = "nitrogen", # variable of interest
#               .depth = "5-15cm",
#               .quantile = "Q0.5",
#               .bbox = c(-9988723.3,  1714261.0, -9282174.6, 991663.7), #format c(xmin, ymax, xmax, ymin)
#               .output_folder = "data/soilgrids")





