#Load administrative layers to be used in other functions

#libraries
library(readr)

#load Belize
blz_adm_0 <- sf::st_read("data/raw/gadm/gadm36_blz/gadm36_BLZ.gpkg", layer = "gadm36_BLZ_0")

#load Guatemala
gtm_adm_0 <- sf::st_read("data/raw/gadm/gadm36_gtm/gadm36_GTM.gpkg", layer = "gadm36_GTM_0")

#Load Honduras administrative areas 
#hnd_adm_0 <- readRDS("data/raw/gadm/gadm36_hnd/gadm36_HND_0_sf.rds")
hnd_adm_0 <- sf::st_read("data/raw/gadm/gadm36_hnd/gadm36_HND.gpkg", layer = "gadm36_HND_0")

#hnd_adm_0 <- sf::st_read("data/processed/gadm/hnd_adm0.gpkg")
#hnd_adm_0 <- hnd_adm_0[c("ISO", "NAME_ENGLI", "geom")]
#names(hnd_adm_0) <- c("GID_0", "NAME_0","geom")

hnd_adm_1 <- sf::st_read("data/raw/gadm/gadm36_hnd/gadm36_HND.gpkg", layer = "gadm36_HND_1")
hnd_adm_2 <- sf::st_read("data/raw/gadm/gadm36_hnd/gadm36_HND.gpkg", layer = "gadm36_HND_2")

#Load El Salvador administrative areas 
slv_adm_0 <- sf::st_read("data/raw/gadm/gadm36_slv/gadm36_SLV.gpkg", layer = "gadm36_SLV_0")
slv_adm_1 <- sf::st_read("data/raw/gadm/gadm36_slv/gadm36_SLV.gpkg", layer = "gadm36_SLV_1")
slv_adm_2 <- sf::st_read("data/raw/gadm/gadm36_slv/gadm36_SLV.gpkg", layer = "gadm36_SLV_2")

#Load Nicaragua administrative areas 
nic_adm_0 <- sf::st_read("data/raw/gadm/gadm36_nic/gadm36_NIC.gpkg", layer = "gadm36_NIC_0")
nic_adm_1 <- readRDS("data/raw/gadm/gadm36_nic/gadm36_NIC_1_sf.rds")
nic_adm_2 <- readRDS("data/raw/gadm/gadm36_nic/gadm36_NIC_2_sf.rds")

#Load Costa Rica administrative areas 
#cri_adm_0 <- sf::st_read("data/raw/gadm/gadm36_cri/gadm36_CRI.gpkg", layer = "gadm36_CRI_0")

cri_adm_0 <- sf::st_read("data/processed/gadm/cri_adm0.gpkg")
cri_adm_0 <- cri_adm_0[c("ISO", "NAME_ENGLI", "geom")]
names(cri_adm_0) <- c("GID_0", "NAME_0","geom")


#cri_adm_1 <- readRDS("data/raw/gadm/gadm36_cri/gadm36_CRI_1_sf.rds")
#cri_adm_2 <- readRDS("data/raw/gadm/gadm36_cri/gadm36_CRI_2_sf.rds")

#Load Panama administrative areas 
pan_adm_0 <- sf::st_read("data/raw/gadm/gadm36_pan/gadm36_PAN.gpkg", layer = "gadm36_PAN_0")


sf::st_crs(blz_adm_0) <- 4326
sf::st_crs(gtm_adm_0) <- 4326
sf::st_crs(hnd_adm_0) <- 4326
sf::st_crs(slv_adm_0) <- 4326
sf::st_crs(nic_adm_0) <- 4326
sf::st_crs(cri_adm_0) <- 4326
sf::st_crs(pan_adm_0) <- 4326


ca_adm_0_ext <- rbind(blz_adm_0,
                      gtm_adm_0,
                      hnd_adm_0,
                      slv_adm_0,
                      nic_adm_0,
                      cri_adm_0,
                      pan_adm_0)

rbca_countries <- rbind(hnd_adm_0,
                        slv_adm_0,
                        nic_adm_0,
                        cri_adm_0
                        )


#load inland water
inwater_hnd <- terra::vect("data/raw/inland_water/hnd/HND_water_areas_dcw.shp")

inwater_slv <- terra::vect("data/raw/inland_water/slv/SLV_water_areas_dcw.shp")

inwater_nic <- terra::vect("data/raw/inland_water/nic/NIC_water_areas_dcw.shp")

inwater_cri <- terra::vect("data/raw/inland_water/cri/CRI_water_areas_dcw.shp")

rbca_inwater <- rbind(inwater_hnd, inwater_slv, inwater_nic, inwater_cri)

#set square meters
inwat_min_size <- 200000000

#Select inland water bigger than min size
rbca_inwater_sel <- rbca_inwater[terra::expanse(rbca_inwater) > inwat_min_size, ]

rbca_inwater_sf <- sf::st_as_sf(rbca_inwater_sel)




