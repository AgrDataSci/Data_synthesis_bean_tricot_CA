#get newdata for predictions

load("data/processed/pdates_predictions/pred_primera.rda")

load("data/processed/pdates_predictions/pred_postrera.rda")

load("data/processed/pdates_predictions/pred_apante.rda")

source("script/utils/load_adm_layers.R")

years_list <- 2000:2019

length(years_list)

rbca_countries_spvt <- terra::vect(rbca_countries)

#get a template for raster layers
template_file <- "data/processed/raster_template/Precipitation-Flux_C3S-glob-agric_AgERA5_20000101_final-v1.0.nc"

map_template <- terra::rast(x = template_file)

map_template <- terra::crop(x = map_template, rbca_countries_spvt)

map_template <- terra::mask(x = map_template, mask = rbca_countries_spvt, touches = FALSE)

map_template_pts <- terra::as.points(x = map_template)

rbca_newdata_coords <- sf::st_coordinates(sf::st_as_sf(map_template_pts))


#format base data frames for newdata for each season

primera_newdata <- vector(mode = "list", length = length(years_list))

postrera_newdata <- vector(mode = "list", length = length(years_list))

apante_newdata <- vector(mode = "list", length = length(years_list))

for(i in seq_along(years_list)){
  
  primera_newdata[[i]] <- data.frame("pdate" = as.Date(pdates_primera_pred[[i]]),
                                     "hdate" = as.Date(end_dates_primera[[i]]),
                                     "lon" = rbca_newdata_coords[, 1],
                                     "lat" = rbca_newdata_coords[, 2])
  
  postrera_newdata[[i]] <- data.frame("pdate" = as.Date(pdates_postrera_pred[[i]]),
                                      "hdate" = as.Date(end_dates_postrera[[i]]),
                                      "lon" = rbca_newdata_coords[, 1],
                                      "lat" = rbca_newdata_coords[, 2])
  
  apante_newdata[[i]] <- data.frame("pdate" = as.Date(pdates_apante_pred[[i]]),
                                    "hdate" = as.Date(end_dates_apante[[i]]),
                                    "lon" = rbca_newdata_coords[, 1],
                                    "lat" = rbca_newdata_coords[, 2])
  
  
}


save(primera_newdata,
     postrera_newdata,
     apante_newdata, file = "data/processed/rbca_newdata.rda")


time_span <- seq.Date(from = min(primera_newdata[[1]]$pdate),
                      to = max(apante_newdata[[20]]$hdate),
                      by = "day")


mdt_files_path <- vector(mode = "character", length = length(time_span))

#Max Daytime Temperature
mdt_files_path <- vapply(X = time_span,
                         FUN.VALUE = "",
                         function(X) agera5:::get_file_path(.var = "temp",
                                                            .statistic = 2,
                                                            .date_to_search = X,
                                                            .agera5_folder = ""))
 
save(mdt_files_path, file = "data/processed/env_newdata/mdt_files_path.rda")

mdt_stack <- terra::rast(x = mdt_files_path)

mdt_values <- terra::extract(x = mdt_stack, y = rbca_newdata_coords)

dimnames(mdt_values)[[2]] <- as.character(time_span)

dimnames(mdt_values)[[2]]

dim(mdt_values)

class(mdt_values)

mdt_values <- as.matrix(mdt_values)

mdt_values <- mdt_values - 273.15


# #Min Night Temperature
# agera5::get_temp_stats()
# 
mnt_files_path <- vector(mode = "character", length = length(time_span))

mnt_files_path <- vapply(X = time_span,
                         FUN.VALUE = "",
                         function(X) agera5:::get_file_path(.var = "temp",
                                                            .statistic = 7,
                                                            .date_to_search = X,
                                                            .agera5_folder = ""))

save(mnt_files_path, file = "data/processed/env_newdata/mnt_files_path.rda")



mnt_stack <- terra::rast(x = mnt_files_path)

mnt_values <- terra::extract(x = mnt_stack, y = rbca_newdata_coords)

dimnames(mnt_values)[[2]] <- as.character(time_span)

dimnames(mnt_values)[[2]]

dim(mnt_values)

class(mnt_values)

mnt_values <- as.matrix(mnt_values)

mnt_values <- mnt_values - 273.15

save(mdt_values, mnt_values, file = "data/processed/env_newdata/temp_newdata_values.rda")

#load(file = "data/processed/env_newdata/temp_newdata_values.rda")
 
dim(mdt_values)[1]

dim(mnt_values)

temp_newdata_array <- array(data = c(mdt_values, mnt_values),
                            dim = c(dim(mdt_values)[1], dim(mdt_values)[2], 2),
                            dimnames = list(1:dim(mdt_values)[1], as.character(time_span), c("mdt", "mnt")))

dimnames(temp_newdata_array)

#Solar radiation flux
srf_files_path <- vapply(X = time_span,
                         FUN.VALUE = "",
                         function(X) agera5:::get_prec_file_path(.date_to_search = X,
                                                                 .agera5_folder = ""))

save(srf_files_path, file = "data/processed/env_newdata/srf_files_path.rda")

srf_stack <- terra::rast(x = srf_files_path)

srf_values <- terra::extract(x = srf_stack, y = rbca_newdata_coords)

#srf_prim_nd <- lapply(, agera5::get_srf.period)

dimnames(srf_values)[[2]] <- as.character(time_span)

dimnames(srf_values)[[2]]

dim(srf_values)

class(srf_values)

srf_values <- as.matrix(srf_values)



#----



#Climate temperature indices with climatrends
temp_ind_nd_prim <- vector(mode = "list", length = length(years_list))

temp_ind_nd_post <- vector(mode = "list", length = length(years_list))

temp_ind_nd_apan <- vector(mode = "list", length = length(years_list))


for(i in seq_along(years_list)){

  temp_ind_nd_prim[[i]] <- climatrends::temperature(temp_newdata_array,
                                              day.one = primera_newdata[[i]]$pdate,
                                              span = primera_newdata[[i]]$hdate - primera_newdata[[i]]$pdate)

  temp_ind_nd_post[[i]] <- climatrends::temperature(temp_newdata_array,
                                                    day.one = postrera_newdata[[i]]$pdate,
                                                    span = postrera_newdata[[i]]$hdate - postrera_newdata[[i]]$pdate)
  
  temp_ind_nd_apan[[i]] <- climatrends::temperature(temp_newdata_array,
                                                    day.one = apante_newdata[[i]]$pdate,
                                                    span = apante_newdata[[i]]$hdate - apante_newdata[[i]]$pdate)

}



save(temp_ind_nd_prim,
     temp_ind_nd_post,
     temp_ind_nd_apan,
     file = "data/processed/env_newdata/temp_newdata_indices.rda")

#get newdata phenological indices

source("script/utils/get_environmental_indices_newdata.R")

tphen_ind_prim <-  vector(mode = "list", length = length(years_list))

tphen_ind_post <-  vector(mode = "list", length = length(years_list))

tphen_ind_apan <-  vector(mode = "list", length = length(years_list))

for(i in seq_along(years_list)){

  tphen_ind_prim[[i]] <- get_phen_indx(primera_newdata[[i]], mdt_values, mnt_values)
  
  tphen_ind_post[[i]] <- get_phen_indx(postrera_newdata[[i]], mdt_values, mnt_values)
  
  tphen_ind_apan[[i]] <- get_phen_indx(apante_newdata[[i]], mdt_values, mnt_values)
   
}

save(tphen_ind_prim,
     tphen_ind_post,
     tphen_ind_apan,
     file = "data/processed/env_newdata/phenological_indices_newdata.rda")


#####- Precipitation data -####

prec_files_path <- vapply(X = time_span,
                         FUN.VALUE = "",
                         function(X) agera5:::get_prec_file_path(.date_to_search = X,
                                                                 .agera5_folder = ""))

save(prec_files_path, file = "data/processed/env_newdata/prec_files_path.rda")

prec_stack <- terra::rast(x = prec_files_path)

prec_values <- terra::extract(x = prec_stack, y = rbca_newdata_coords)

dimnames(prec_values)[[2]] <- as.character(time_span)

dimnames(prec_values)[[2]]

dim(prec_values)

class(prec_values)

prec_values <- as.matrix(prec_values)

save(prec_values, file = "data/processed/env_newdata/prec_newdata_values.rda")

#Climate precipitation indices with climatrends
prec_ind_prim <- vector(mode = "list", length = length(years_list))

prec_ind_post <- vector(mode = "list", length = length(years_list))

prec_ind_apan <- vector(mode = "list", length = length(years_list))

for(i in seq_along(prec_ind_prim)){
  
  prec_ind_prim[[i]] <- climatrends::rainfall(prec_values,
                                                     day.one = primera_newdata[[i]]$pdate,
                                                     span = primera_newdata[[i]]$hdate - primera_newdata[[i]]$pdate)
  
  
  prec_ind_post[[i]] <- climatrends::rainfall(prec_values,
                                              day.one = postrera_newdata[[i]]$pdate,
                                              span = postrera_newdata[[i]]$hdate - postrera_newdata[[i]]$pdate)
  
  
  prec_ind_apan[[i]] <- climatrends::rainfall(prec_values,
                                             day.one = apante_newdata[[i]]$pdate,
                                             span = apante_newdata[[i]]$hdate - apante_newdata[[i]]$pdate)
  
  
  
}

save(prec_ind_prim,
     prec_ind_post,
     prec_ind_apan,
     file = "data/processed/env_newdata/prec_newdata_indices.rda")


#Put all climate newdata together with season datasets

for(i in seq_along(years_list)){

  primera_newdata[[i]] <- cbind(primera_newdata[[i]], 
                                temp_ind_nd_prim[[i]], 
                                tphen_ind_prim[[i]],
                                prec_ind_prim[[i]])

  postrera_newdata[[i]] <- cbind(postrera_newdata[[i]], 
                                 temp_ind_nd_post[[i]], 
                                 tphen_ind_post[[i]],
                                 prec_ind_post[[i]])

  apante_newdata[[i]] <- cbind(apante_newdata[[i]], 
                               temp_ind_nd_apan[[i]],  
                               tphen_ind_apan[[i]],
                               prec_ind_apan[[i]])

  
  
}

save(primera_newdata,
     postrera_newdata,
     apante_newdata,
     file = "data/processed/env_newdata/newdata_clim_all.rda")



load("data/processed/env_newdata/newdata_clim_all.rda")

#####- Soils data -#####
#cec_0-5cm
cec_0_5_files <- list.files(path = "data/raw/soilgrids/", pattern = "_cec_0-5cm", full.names = TRUE)

cec_0_5_rast <- vector(mode = "list", length = 4)

cec_0_5_rast <- lapply(cec_0_5_files, terra::rast)

cec_0_5_rcoll <- terra::src(cec_0_5_rast)

cec_0_5_rast_m <- terra::merge(cec_0_5_rcoll)


#soc_0-5cm
soc_0_5_files <- list.files(path = "data/raw/soilgrids/", pattern = "_soc_0-5cm", full.names = TRUE)

soc_0_5_rast <- vector(mode = "list", length = 4)

soc_0_5_rast <- lapply(soc_0_5_files, terra::rast)

#create a raster collection (aka Stack in raster)
soc_0_5_rcoll <- terra::src(soc_0_5_rast)

soc_0_5_rast_m <- terra::merge(soc_0_5_rcoll)

#terra::plot(soc_05_rast_m)

#soc_30-60cm
soc_30_60_files <- list.files(path = "data/raw/soilgrids/", pattern = "_soc_30-60cm", full.names = TRUE)

soc_30_60_rast <- vector(mode = "list", length = 4)

soc_30_60_rast <- lapply(soc_30_60_files, terra::rast)

#create a raster collection (aka Stack in raster)
soc_30_60_rcoll <- terra::src(soc_30_60_rast)

soc_30_60_rast_m <- terra::merge(soc_30_60_rcoll)

#cec_15_30
cec_15_30_files <- list.files(path = "data/raw/soilgrids/", pattern = "_cec_15-30cm", full.names = TRUE)

cec_15_30_files

cec_15_30_rast <- vector(mode = "list", length = 4)

cec_15_30_rast <- lapply(cec_15_30_files, terra::rast)

cec_15_30_rcoll <- terra::src(cec_15_30_rast)

cec_15_30_rast_m <- terra::merge(cec_15_30_rcoll)


#
nitrogen_15_30_files <- list.files(path = "data/raw/soilgrids/", pattern = "_nitrogen_15-30cm", full.names = TRUE)

nitrogen_15_30_files

nitrogen_15_30_rast <- vector(mode = "list", length = 4)

nitrogen_15_30_rast <- lapply(nitrogen_15_30_files, terra::rast)

nitrogen_15_30_rcoll <- terra::src(nitrogen_15_30_rast)

nitrogen_15_30_rast_m <- terra::merge(nitrogen_15_30_rcoll)

terra::plot(nitrogen_15_30_rast_m)

#
cfvo_5_15_files <- list.files(path = "data/raw/soilgrids/", pattern = "_cfvo_5-15cm", full.names = TRUE)

cfvo_5_15_files

cfvo_5_15_rast <- vector(mode = "list", length = 4)

cfvo_5_15_rast <- lapply(cfvo_5_15_files, terra::rast)

cfvo_5_15_rcoll <- terra::src(cfvo_5_15_rast)

cfvo_5_15_rast_m <- terra::merge(cfvo_5_15_rcoll)

#terra::plot(cfvo_5_15_rast_m)

#terra::plot(map_template_pts, add = TRUE)

#ocd_30_60cm
ocd_30_60_files <- list.files(path = "data/raw/soilgrids/", pattern = "_ocd_30-60cm", full.names = TRUE)

ocd_30_60_files

ocd_30_60_rast <- vector(mode = "list", length = 4)

ocd_30_60_rast <- lapply(ocd_30_60_files, terra::rast)

ocd_30_60_rcoll <- terra::src(ocd_30_60_rast)

ocd_30_60_rast_m <- terra::merge(ocd_30_60_rcoll)


###########

map_template_pts_proj <- terra::project(map_template_pts, y = "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs")

#terra::plot(map_template_pts_proj, add = TRUE)

soc_30_60_values <- terra::extract(soc_30_60_rast_m, map_template_pts_proj)
dim(soc_30_60_values)


cec_15_30_values <- terra::extract(cec_15_30_rast_m, map_template_pts_proj)
dim(cec_15_30_values)

cec_0_5_values <- terra::extract(cec_0_5_rast_m, map_template_pts_proj)
dim(cec_15_30_values)

nitrogen_15_30_values <- terra::extract(nitrogen_15_30_rast_m, map_template_pts_proj)
dim(nitrogen_15_30_values)

cfvo_5_15_values <- terra::extract(cfvo_5_15_rast_m, map_template_pts_proj)
dim(cfvo_5_15_values)

head(cfvo_5_15_values)

soc_0_5_values <- terra::extract(soc_0_5_rast_m, map_template_pts_proj)
dim(soc_0_5_values)

head(soc_0_5_values)

ocd_30_60_values <- terra::extract(ocd_30_60_rast_m, map_template_pts_proj)
dim(ocd_30_60_values)



soils_newdata <- data.frame("soc_30_60cm" = soc_30_60_values[, 2],
                            "nitrogen_15_30cm" = nitrogen_15_30_values[, 2],
                            "cec_0_5cm" = cec_0_5_values[, 2],
                            "soc_0_5cm" = soc_0_5_values[, 2],
                            "cfvo_5_15cm" = cfvo_5_15_values[, 2],
                            "ocd_30_60cm" = ocd_30_60_values[, 2]
                            )

#soils_newdata <- data.frame("cec_15_30cm" = cec_15_30_values[, 2])

nrow(soils_newdata)
#Put all environmental data  together with season datasets

for(i in seq_along(years_list)){
  
  primera_newdata[[i]] <- cbind(primera_newdata[[i]], 
                                soils_newdata)
  
  postrera_newdata[[i]] <- cbind(postrera_newdata[[i]], 
                                 soils_newdata)
  
  apante_newdata[[i]] <- cbind(apante_newdata[[i]], 
                               soils_newdata)
  
  
  
}







