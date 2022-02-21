library(survival)
library(survminer)
#library(rms)
library(ggplot2)

source("script/utils/load_rbca_data.R")
source("script/utils/load_adm_layers.R")

rbca_countries_spvt <- terra::vect(rbca_countries)

#get a template for raster layers
template_file <- "data/processed/raster_template/Precipitation-Flux_C3S-glob-agric_AgERA5_20000101_final-v1.0.nc"

map_template <- terra::rast(x = template_file)

map_template <- terra::crop(x = map_template, rbca_countries_spvt)

map_template <- terra::mask(x = map_template, mask = rbca_countries_spvt, touches = FALSE)

map_template_pts <- terra::as.points(x = map_template)

n_cells <- terra::cells(map_template)

years_list <- 2000:2019

obs_pdates <- tricot_data

obs_pdates$pdate <- as.Date(obs_pdates$pdate)

obs_pdates$month <- as.integer(format(obs_pdates$pdate, "%m"))

obs_pdates$year <- as.integer(format(obs_pdates$pdate, "%Y"))

obs_pdates$season <- ifelse(obs_pdates$month >= 4 & obs_pdates$month <= 7, "Primera", 
                            ifelse(obs_pdates$month >= 8 & obs_pdates$month <= 10,  "Postrera", "Apante"))

obs_pdates$start_date <- ifelse(obs_pdates$season == "Primera", 
                                as.Date(paste0(obs_pdates$year, "-04-01")) ,
                                ifelse(obs_pdates$season == "Postrera",
                                       as.Date(paste0(obs_pdates$year, "-08-01")),
                                       ifelse(obs_pdates$month <= 3, 
                                              as.Date(paste0(obs_pdates$year - 1, "-10-01")), 
                                              as.Date(paste0(obs_pdates$year, "-10-01"))))
)

obs_pdates$start_date <- as.Date(obs_pdates$start_date, origin = "1970-01-01")

obs_pdates$elapsed_time <- as.numeric(obs_pdates$pdate - as.Date(obs_pdates$start_date))

obs_pdates_primera <- obs_pdates[obs_pdates$season == "Primera", ]

obs_pdates_postrera <- obs_pdates[obs_pdates$season == "Postrera", ]

obs_pdates_apante <- obs_pdates[obs_pdates$season == "Apante", ]

#convert into start-stop format
#primera season
primera_trials <- vector(mode = "list", length = nrow(obs_pdates_primera))

for(i in seq_len(nrow(obs_pdates_primera))){
  primera_trials[[i]] <- data.frame("id" = rep(obs_pdates_primera[i, "id"], 
                                             obs_pdates_primera[i,"elapsed_time"]),
                                  "time" = rep(obs_pdates_primera[i, "elapsed_time"], 
                                               obs_pdates_primera[i,"elapsed_time"]),
                                  "t_start" = seq(0, obs_pdates_primera[i, "elapsed_time"] - 1),
                                  "t_stop" = seq(1, obs_pdates_primera[i,"elapsed_time"]),
                                  "status" = c(rep(0, obs_pdates_primera[i,"elapsed_time"] - 1), 1)
                                  )
}


#postrera season
postrera_trials <- vector(mode = "list", length = nrow(obs_pdates_postrera))

for(i in seq_len(nrow(obs_pdates_postrera))){
  
  postrera_trials[[i]] <- data.frame("id" = rep(obs_pdates_postrera[i, "id"], 
                                                obs_pdates_postrera[i,"elapsed_time"]),
                                     "time" = rep(obs_pdates_postrera[i, "elapsed_time"], 
                                                  obs_pdates_postrera[i,"elapsed_time"]),
                                     "t_start" = seq(0, obs_pdates_postrera[i, "elapsed_time"] - 1),
                                     "t_stop" = seq(1, obs_pdates_postrera[i,"elapsed_time"]),
                                     "status" = c(rep(0, obs_pdates_postrera[i,"elapsed_time"] - 1), 1)
  )
}

#apante
apante_trials <- vector(mode = "list", length = nrow(obs_pdates_apante))

for(i in seq_len(nrow(obs_pdates_apante))){
  
  apante_trials[[i]] <- data.frame("id" = rep(obs_pdates_apante[i, "id"], 
                                              obs_pdates_apante[i,"elapsed_time"]),
                                   "time" = rep(obs_pdates_apante[i, "elapsed_time"], 
                                                obs_pdates_apante[i,"elapsed_time"]),
                                   "t_start" = seq(0, obs_pdates_apante[i, "elapsed_time"] - 1),
                                   "t_stop" = seq(1, obs_pdates_apante[i,"elapsed_time"]),
                                   "status" = c(rep(0, obs_pdates_apante[i,"elapsed_time"] - 1), 1)
  )
}



primera_trials
postrera_trials
apante_trials




##################################End data formating

#First get climate data using the script in Jobs folder: get_climate_pdates.R

# prec_primera_mod <- get_climate_pdates_results$prec_primera_mod
# 
# prec_postrera_mod <- get_climate_pdates_results$prec_postrera_mod
# 
# prec_apante_mod <- get_climate_pdates_results$prec_apante_mod
# is.data.frame(primera_trials[[i]])

# srf_primera_mod <- get_climate_pdates_results$srf_primera_mod
# 
# srf_postrera_mod <- get_climate_pdates_results$srf_primera_mod
# 
# srf_apante_mod <- get_climate_pdates_results$srf_apante_mod


# for(i in seq_along(primera_trials)){
#   primera_trials[[i]]$prec <- as.numeric(prec_primera_mod[[i]][2:length(prec_primera_mod[[i]])])
#   primera_trials[[i]]$prec_cuml <- cumsum(primera_trials[[i]]$prec)
#   
#   primera_trials[[i]]$srf <- as.numeric(srf_primera_mod[[i]][2:length(srf_primera_mod[[i]])])
# 
# }



length(srf_postrera_mod)
# for(i in seq_along(postrera_trials)){
#   postrera_trials[[i]]$prec <- as.numeric(prec_postrera_mod[[i]][2:length(prec_postrera_mod[[i]])])
#   postrera_trials[[i]]$prec_cuml <- cumsum(postrera_trials[[i]]$prec)
#   
#   postrera_trials[[i]]$srf <- as.numeric(srf_postrera_mod[[i]][2:length(srf_postrera_mod[[i]])])
# }
# # 
# 
# for(i in seq_along(apante_trials)){
#   apante_trials[[i]]$prec <- as.numeric(prec_apante_mod[[i]][2:length(prec_apante_mod[[i]])])
#   apante_trials[[i]]$prec_cuml <- cumsum(apante_trials[[i]]$prec)
#   
#   apante_trials[[i]]$srf <- as.numeric(srf_apante_mod[[i]][2:length(srf_apante_mod[[i]])])
# 
# 
# 
# }

# primera_trials_all <- do.call(what = rbind,
#                                args = primera_trials)
# 
# head(primera_trials_all)
# 
# write.csv(x = primera_trials_all,
#           file = "data/processed/pdates_modelling/primera_trials_all.csv",
#           row.names = FALSE)
# 
# 
# postrera_trials_all <- do.call(what = rbind,
#                                 args = postrera_trials)
# 
# head(postrera_trials_all)
# 
# write.csv(x = postrera_trials_all,
#           file = "data/processed/pdates_modelling/postrera_trials_all.csv",
#           row.names = FALSE)
# 
# 
# apante_trials_all <- do.call(what = rbind,
#                               args = apante_trials)
# 
# head(apante_trials_prec)
# 
# write.csv(x = apante_trials_all,
#           file = "data/processed/pdates_modelling/apante_trials_all.csv",
#           row.names = FALSE)

####################


#load all data to fit survival model

primera_trials_all <- read.csv(file = "data/processed/pdates_modelling/primera_trials_all.csv")

postrera_trials_all <- read.csv(file = "data/processed/pdates_modelling/postrera_trials_all.csv")

apante_trials_all <- read.csv(file = "data/processed/pdates_modelling/apante_trials_all.csv")

#primera_trials_all <- cbind(primera_trials_all, primera_trials_prec$srf)


head(primera_trials_all)

head(postrera_trials_all)

head(apante_trials_all)


#Create a survival object for primera season
primera_surv <- Surv(time = primera_trials_all$t_start,
                     time2 = primera_trials_all$t_stop,
                     event = primera_trials_all$status)

#Fit a cox proportional hazard regression model (Primera Season)
primera_cox <- coxph(Surv(time = t_start,
                          time2 = t_stop,
                          event = status) ~ prec_cuml + prec + srf,
                     data = primera_trials_all)

summary(primera_cox)

primera_test <- cox.zph(primera_cox)
primera_test

step(primera_cox)

ggcoxzph(primera_test)

survminer::ggcoxdiagnostics(primera_cox)


primera_cox <- coxph(Surv(time = t_start,
                          time2 = t_stop,
                          event = status) ~  prec_cuml,
                     data = primera_trials_all)

summary(primera_cox)

primera_test <- cox.zph(primera_cox)
primera_test

plot(primera_test)

step(primera_cox)

ggcoxzph(primera_test)

survminer::ggcoxdiagnostics(primera_cox)

#removing prec actually decrease concordance from 0.811 to 0.808

#Test a prediction with "new data" (subset of original data)
survfit_primera <- survfit(primera_cox,
                           newdata = primera_trials_all[1:1500,],
                           id = id)



#example how to extract quantile probabilities
quant_primera <- quantile(survfit_primera, 
                          probs = c(0.25, 0.5, 0.75), 
                          conf.int = FALSE)

quant_primera[,1]

#-------------------#

#Create a survival object for postrera season
postrera_surv <- Surv(time = postrera_trials_all$t_start,
                      time2 = postrera_trials_all$t_stop,
                      event = postrera_trials_all$status)


#postrera_trials_all$srf_2 <- postrera_trials_all$srf/1000000

#Fit a cox proportional hazard regression model (Primera Season)
postrera_cox <- coxph(Surv(time = t_start,
                           time2 = t_stop,
                           event = status) ~  prec + prec_cuml + srf,
                      data = postrera_trials_all)

summary(postrera_cox)

step(postrera_cox)
postrera_test <- cox.zph(postrera_cox)#, transform = "identity")
postrera_test

#Final model
postrera_cox <- coxph(Surv(time = t_start,
                           time2 = t_stop,
                           event = status) ~ prec + srf,
                      data = postrera_trials_all)

summary(postrera_cox)

postrera_test <- cox.zph(postrera_cox)#, transform = "identity")
postrera_test

plot(postrera_test)

ggcoxzph(postrera_test)

survminer::ggcoxdiagnostics(postrera_cox)


#Test a prediction with "new data" (subset of original data)
survfit_postrera <- survfit(postrera_cox,
                            newdata = postrera_trials_all,
                            id = id)

ggforest(postrera_cox, data = postrera_trials_all)

ggsurvplot(survfit_postrera, data = postrera_trials_all, conf.int = T, )

survfit_postrera <- survfit(postrera_cox, data = postrera_trials_all)

ggsurvplot(survfit_postrera)

ggsurvplot(survfit_postrera, fun = "cloglog")

#example how to extract quantile probabilities
quant_postrera <- quantile(survfit_postrera, 
                           probs = c(0.25, 0.5, 0.75), 
                           conf.int = FALSE)

quant_postrera[,1]

obs_pdates_postrera$pdate


#------------------------------#

#Create a survival object for apante season
apante_surv <- Surv(time = apante_trials_all$t_start,
                    time2 = apante_trials_all$t_stop,
                    event = apante_trials_all$status)

#Fit a cox proportional hazard regression model (Primera Season)
apante_cox <- coxph(Surv(time = t_start,
                         time2 = t_stop,
                         event = status) ~ prec + prec_cuml + srf,
                    data = apante_trials_all)

summary(apante_cox)

step(apante_cox)

apante_test <- cox.zph(apante_cox)#, transform = "identity")
apante_test

ggcoxzph(apante_test)

#final model
apante_cox <- coxph(Surv(time = t_start,
                         time2 = t_stop,
                         event = status) ~ prec,
                    data = apante_trials_all)

summary(apante_cox)

apante_test <- cox.zph(apante_cox)#, transform = "identity")
apante_test


#Test a prediction with "new data" (subset of original data)
survfit_apante <- survfit(apante_cox,
                          newdata = apante_trials_all[1:500,],
                          id = id)


survfit_apante <- survfit(apante_cox,
                          newdata = apante_trials_all,
                          id = id)

plot(survfit_apante, conf.int = .9)

survminer::ggsurvplot(survfit_apante, data = apante_trials_all)

#example how to extract quantile probabilities
quant_apante <- quantile(survfit_apante, 
                         probs = c(0.25, 0.5, 0.75), 
                         conf.int = FALSE)

head(quant_apante)


#----------------------------#


#function to get precipitation data to make the surv predictions
get_prec_surv <- function(year, season){
  
  if(season == "primera"){
    #min(obs_pdates_primera$pdate)
    start_date <- as.Date(paste0(year, "-04-01"))
    #max(obs_pdates_primera$pdate)
    end_date <- as.Date(paste0(year, "-05-25"))
    
    time_span <- seq.Date(from = start_date, to = end_date, by = "day")
  }
  
  if(season == "postrera"){
    
    #min(obs_pdates_postrera$pdate)
    start_date <- as.Date(paste0(year, "-08-01"))
    
    #max(obs_pdates_postrera$pdate)
    end_date <- as.Date(paste0(year, "-10-31"))
    
    time_span <- seq.Date(from = start_date, to = end_date, by = "day")
  }
  
  if(season == "apante"){#pending to check what happens with dates accross years
    #min(obs_pdates_apante$pdate)
    start_date <- as.Date(paste0(year, "-10-01"))
    
    #max(obs_pdates_apante$pdate)
    end_date <- as.Date(paste0(year, "-12-28"))
    
    time_span <- seq.Date(from = start_date, to = end_date, by = "day")
  }
  
  
  prec_files_path <- sapply(X = time_span,
                      function(X) agera5:::get_prec_file_path(X,
                                                    "ADD_agera5_folder_here..."))
  
  prec_data <- terra::rast(x = prec_files_path)
  
  prec_data <- terra::crop(x = prec_data, rbca_countries_spvt)
  
  prec_data <- terra::mask(x = prec_data, mask = rbca_countries_spvt, touches = FALSE)
  
  
  return(prec_data)
  
}

get_srf_surv <- function(year, season){
  
  if(season == "primera"){
    #min(obs_pdates_primera$pdate)
    start_date <- as.Date(paste0(year, "-04-01"))
    #max(obs_pdates_primera$pdate)
    end_date <- as.Date(paste0(year, "-05-25"))
    
    time_span <- seq.Date(from = start_date, to = end_date, by = "day")
  }
  
  if(season == "postrera"){
    
    #min(obs_pdates_postrera$pdate)
    start_date <- as.Date(paste0(year, "-08-01"))
    
    #max(obs_pdates_postrera$pdate)
    end_date <- as.Date(paste0(year, "-10-31"))
    
    time_span <- seq.Date(from = start_date, to = end_date, by = "day")
  }
  
  if(season == "apante"){#pending to check what happens with dates accross years
    #min(obs_pdates_apante$pdate)
    start_date <- as.Date(paste0(year, "-10-01"))
    
    #max(obs_pdates_apante$pdate)
    end_date <- as.Date(paste0(year, "-12-28"))
    
    time_span <- seq.Date(from = start_date, to = end_date, by = "day")
  }
  
  
 srf_files_path <- sapply(X = time_span,
                            function(X) agera5:::get_srf_filepath(X,
                                                                    "ADD_agera5_folder_here..."))
  
  srf_data <- terra::rast(x = srf_files_path)
  
  srf_data <- terra::crop(x = srf_data, rbca_countries_spvt)
  
  srf_data <- terra::mask(x = srf_data, mask = rbca_countries_spvt, touches = FALSE)
  
  
  return(srf_data)
  
  
  
}



prec_surv_primera_nd <- vector(mode = "list", length = length(years_list))
prec_surv_postrera_nd <- vector(mode = "list", length = length(years_list))
prec_surv_apante_nd <- vector(mode = "list", length = length(years_list))

srf_surv_postrera_nd <- vector(mode = "list", length = length(years_list))

for(i in seq_along(years_list)){
  
  prec_surv_primera_nd[i] <- get_prec_surv(years_list[i], "primera")

  prec_surv_postrera_nd[i] <- get_prec_surv(years_list[i], "postrera")

  prec_surv_apante_nd[i] <- get_prec_surv(years_list[i], "apante")
  
  srf_surv_postrera_nd[i] <- get_srf_surv(years_list[i], "postrera")
  
}

# save(prec_surv_primera_nd,
#      prec_surv_postrera_nd,
#      prec_surv_apante_nd,
#      srf_surv_postrera_nd,
#      file = "data/processed/pdates_predictions/pdates_newdata.rda")



#---Primera

#get the median of observed growing period for primera season
med_grow_primera <- median(as.Date(obs_pdates_primera$hdate) - as.Date(obs_pdates_primera$pdate))

med_grow_primera

#Predict survival with newdata and get predicted pdates and end dates (hdates)

surv_primera_newdata <- vector(mode = "list", length = length(n_cells))

pdates_primera_pred <- vector(mode = "list", length = length(years_list))

surv_primera_newdata_year <- vector(mode = "list", length = length(years_list))

surv_primera_pred <- vector(mode = "list", length = length(years_list))

surv_primera_quant <- vector(mode = "list", length = length(years_list))

end_dates_primera <- vector(mode = "list", length = length(years_list))



for(i in seq_along(years_list)){
  prec_primera_i <- terra::extract(prec_surv_primera_nd[[i]], map_template_pts)
  
  for(j in seq_along(n_cells)){
    
    
    surv_primera_newdata[[j]] <-  data.frame("id" = rep(j, ncol(prec_primera_i) - 1),
                                             "prec" = as.numeric(prec_primera_i[j, -1]),
                                             "prec_cuml" = cumsum(as.numeric(prec_primera_i[j, -1])),
                                             "t_start" = seq(0, ncol(prec_primera_i) - 2),
                                             "t_stop" = seq(1, ncol(prec_primera_i) - 1),
                                             "status" = rep(0, ncol(prec_primera_i) - 1)
                                             )
  
    
  
  # surv_primera_pred <- survival::survfit(primera_cox,
  #                                        newdata = pred_primera_data_b,
  #                                        id = id)
    
    print(j)
  }
  
  print(i)
  
  
  surv_primera_newdata_year[[i]] <- do.call(rbind, surv_primera_newdata) 
  
  surv_primera_pred[[i]] <- survival::survfit(primera_cox,
                                              newdata = surv_primera_newdata_year[[i]],
                                              id = id)
  
  #Get the quantile with 0.75 probabilities to plant (doesn't work with less, as it produces NA)
  #The interpretation is different from the usual survival analysis as we are interested
  #on the planting date, i.e. the quantile with the higher probability of "death"
  surv_primera_quant[[i]] <- quantile(surv_primera_pred[[i]], probs = c(0.25), conf.int = FALSE)
  
  #pdates are calculated from the 1st of april + the quantile
  #this is are the predicted pdates for each cell for the year i
  pdates_primera_pred[[i]] <- as.Date(paste0(years_list[i],"-04-01")) + as.numeric(unlist(surv_primera_quant[[i]]))
  
  #end dates
  end_dates_primera[[i]] <- pdates_primera_pred[[i]] + med_grow_primera  
  
  
  
}

save(surv_primera_newdata,
     surv_primera_newdata_year,
     surv_primera_pred,
     surv_primera_quant,
     pdates_primera_pred,
     end_dates_primera,
     file = "data/processed/pdates_predictions/pred_primera.rda")



#---Postrera------------------
#get the median of observed growing period for postrera season
med_grow_postrera <- median(as.Date(obs_pdates_postrera$hdate) - as.Date(obs_pdates_postrera$pdate))

med_grow_postrera


surv_postrera_newdata <- vector(mode = "list", length = length(n_cells))

pdates_postrera_pred <- vector(mode = "list", length = length(years_list))

surv_postrera_newdata_year <- vector(mode = "list", length = length(years_list))

surv_postrera_pred <- vector(mode = "list", length = length(years_list))

surv_postrera_quant <- vector(mode = "list", length = length(years_list))

end_dates_postrera <- vector(mode = "list", length = length(years_list))

for(i in seq_along(years_list)){
  
  prec_postrera_i <- terra::extract(prec_surv_postrera_nd[[i]], map_template_pts)
  
  srf_postrera_i <- terra::extract(srf_surv_postrera_nd[[i]], map_template_pts)
  
  for(j in seq_along(n_cells)){
  
    
    surv_postrera_newdata[[j]] <-  data.frame("id" = rep(j, ncol(prec_postrera_i) - 1),
                                              "prec" = as.numeric(prec_postrera_i[j, -1]),
                                              "srf" = as.numeric(srf_postrera_i[j, -1]),
                                              "prec_cuml" = cumsum(as.numeric(prec_postrera_i[j, -1])),
                                              "t_start" = seq(0, ncol(prec_postrera_i) - 2),
                                              "t_stop" = seq(1, ncol(prec_postrera_i) - 1),
                                              "status" = rep(0, ncol(prec_postrera_i) - 1)
    )
    
    surv_postrera_newdata_year[[i]] <- do.call(rbind, surv_postrera_newdata) 
    
    # surv_postrera_pred <- survival::survfit(postrera_cox,
    #                                        newdata = pred_postrera_data_b,
    #                                        id = id)
    
    print(j)
    
  }
  print(i)
  
  surv_postrera_pred[[i]] <- survival::survfit(postrera_cox,
                                               newdata = surv_postrera_newdata_year[[i]],
                                               id = id)
  
  #Get the quantile with 0.75 probabilities to plant (doesn't work with less, as it produces NA)
  #The interpretation is different from the usual survival analysis as we are interested
  #on the planting date, i.e. the quantile with the higher probability of "death"
  surv_postrera_quant[[i]] <- quantile(surv_postrera_pred[[i]], probs = c(0.25), conf.int = FALSE)
  
  #pdates are calculated from the 1st of August + the quantile
  #this is are the predicted pdates for each cell for the year i
  pdates_postrera_pred[[i]] <- as.Date(paste0(years_list[i],"-08-01")) + as.numeric(unlist(surv_postrera_quant[[i]]))
  
  #end dates
  end_dates_postrera[[i]] <- pdates_postrera_pred[[i]] + med_grow_postrera  
  
}

save(surv_postrera_newdata,
     surv_postrera_newdata_year,
     surv_postrera_pred,
     surv_postrera_quant,
     pdates_postrera_pred,
     end_dates_postrera,
     file = "data/processed/pdates_predictions/pred_postrera.rda")


#-------------------------end Postrera


#---Apante
#get the median of observed growing period for apante season
med_grow_apante <- median(as.Date(obs_pdates_apante$hdate) - as.Date(obs_pdates_apante$pdate))

med_grow_apante

surv_apante_newdata <- vector(mode = "list", length = length(n_cells))

pdates_apante_pred <- vector(mode = "list", length = length(years_list))

surv_apante_newdata_year <- vector(mode = "list", length = length(years_list))

surv_apante_pred <- vector(mode = "list", length = length(years_list))

surv_apante_quant <- vector(mode = "list", length = length(years_list))

end_dates_apante <- vector(mode = "list", length = length(years_list))

for(i in seq_along(years_list)){
  
  prec_apante_i <- terra::extract(prec_surv_apante_nd[[i]], map_template_pts)
  
  for(j in seq_along(n_cells)){
    
    
    surv_apante_newdata[[j]] <-  data.frame("id" = rep(j, ncol(prec_apante_i) - 1),
                                            "prec" = as.numeric(prec_apante_i[j, -1]),
                                            "prec_cuml" = cumsum(as.numeric(prec_apante_i[j, -1])),
                                            "t_start" = seq(0, ncol(prec_apante_i) - 2),
                                            "t_stop" = seq(1, ncol(prec_apante_i) - 1),
                                            "status" = rep(0, ncol(prec_apante_i) - 1)
    )
    
    surv_apante_newdata_year[[i]] <- do.call(rbind, surv_apante_newdata) 
    
    # surv_apante_pred <- survival::survfit(apante_cox,
    #                                        newdata = pred_apante_data_b,
    #                                        id = id)
    
    print(j)
    
  }
  
  print(i)
  
  surv_apante_pred[[i]] <- survival::survfit(apante_cox,
                                             newdata = surv_apante_newdata_year[[i]],
                                             id = id)
  
  #Get the quantile with 0.75 probabilities to plant (doesn't work with less, as it produces NA)
  #The interpretation is different from the usual survival analysis as we are interested
  #on the planting date, i.e. the quantile with the higher probability of "death"
  surv_apante_quant[[i]] <- quantile(surv_apante_pred[[i]], probs = c(0.25), conf.int = FALSE)
  
  #pdates are calculated from the 1st of October + the quantile
  #this is are the predicted pdates for each cell for the year i
  pdates_apante_pred[[i]] <- as.Date(paste0(years_list[i],"-10-01")) + as.numeric(unlist(surv_apante_quant[[i]]))
  
  #end dates
  end_dates_apante[[i]] <- pdates_apante_pred[[i]] + med_grow_apante  
  
}

save(surv_apante_newdata,
     surv_apante_newdata_year,
     surv_apante_pred,
     surv_apante_quant,
     pdates_apante_pred,
     end_dates_apante,
     file = "data/processed/pdates_predictions/pred_apante.rda")


#-------End Apante



###### End of survival pdates#########################




