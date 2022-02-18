library(readr)
library(climatrends)
library(agera5)

tricot_data <- read_csv("data/red_beans_data.csv")

tricot_data$pdate <- as.Date(tricot_data$pdate)
tricot_data$hdate <- as.Date(tricot_data$hdate)

#set data location
agera5_folder <- ""

#### Extract precipitation data from AgERA5 files
rbca_prec_agera5 <- agera5::get_prec.dataset(tricot_data, agera5_folder)

rbca_prec_agera5[[1]]
length(rbca_prec_agera5)

#save(rbca_prec_agera5, file = "data/raw/agera5/rbca_prec_agera5.rda")

#get rainfall indices with climatrends
rbca_prec_agera5_ind <- NULL

for(i in 1:length(rbca_prec_agera5)){
  rbca_prec_agera5_ind[[i]] <- rainfall(object  = rbca_prec_agera5[[i]],
                                    day.one = colnames(rbca_prec_agera5[[i]])[1],
                                    span    = length(rbca_prec_agera5[[i]]))
  
  
}

rbca_prec_agera5_ind <- dplyr::bind_rows(rbca_prec_agera5_ind)

rbca_prec_agera5_ind

#readr::write_csv(rbca_prec_agera5_ind, file = "data/rbca_prec_agera5_ind.csv")

#### Extract temperature data from AgERA5 files

rbca_maxDT <- agera5::get_temp.dataset(tricot_data, .stat = 2, agera5_folder)

length(rbca_maxDT)

#save(rbca_maxDT, file = "data/raw/agera5/rbca_maxDT_agera5.rda")

rbca_minNT <- agera5::get_temp.dataset(tricot_data, .stat = 7, agera5_folder)

length(rbca_minNT)

#save(rbca_minNT, file = "data/raw/agera5/rbca_minNT_agera5.rda")

rbca_temp_agera5_ind <- NULL
for(i in 1:length(rbca_maxDT)){
  rbca_temp_agera5_ind[[i]] <- temperature(rbca_maxDT[[i]][1, ], rbca_minNT[[i]][1, ])
}
rbca_temp_agera5_ind
rbca_temp_agera5_ind <- bind_rows(rbca_temp_agera5_ind)
rbca_temp_agera5_ind

#readr::write_csv(rbca_temp_agera5_ind, file = "data/rbca_agera5_temp_ind.csv")

#### Extract solar radiation flux data from AgERA5 files

rbca_agera5_srf <- agera5::get_srf.dataset(tricot_data, agera5_folder)

#save(rbca_agera5_srf, file = "data/raw/agera5/rbca_srf_agera5.rda")

head(rbca_agera5_srf)
length(rbca_agera5_srf)

mean_srf_agear5 <- lapply(rbca_agera5_srf, mean)

mean_srf_agear5 <- data.frame("srf" = as.numeric(mean_srf_agear5))

#readr::write_csv(x = mean_srf_agear5, file = "data/mean_srf.csv")












