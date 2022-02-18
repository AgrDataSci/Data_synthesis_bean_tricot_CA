#------------------------------------------------------------

# library(readr)
# library(PlackettLuce)
# library(gosset)
# library(dplyr)
# library(caret)
# library(patchwork)#for plot tree using gosset function
# library(pheatmap)
# library(GGally)

#load_rbca_data <- function(.full = FALSE){

#load tricot data
tricot_data <- read.csv("data/processed/red_beans_data.csv")
head(tricot_data)

unique(tricot_data$trial_name)

#load climate data
temperature <- readr::read_csv("data/processed/covariates_for_modelling/rbca_agera5_temp_ind.csv")
head(temperature)
precipitation <- readr::read_csv("data/processed/covariates_for_modelling/rbca_prec_agera5_ind.csv")
head(precipitation)
phenological_indices <- readr::read_csv("data/processed/covariates_for_modelling/phenological_indices.csv")
head(phenological_indices)

climate_data <- cbind(temperature, precipitation, phenological_indices)

#load soil data
soil_data <- readr::read_csv("data/processed/covariates_for_modelling/soilgrids.csv")
head(soil_data)

#load soil water content data
swc <- readr::read_csv("data/processed/covariates_for_modelling/mean_swc.csv")
head(swc)

#load actual evapotranspiration data
aet <- readr::read_csv("data/processed/covariates_for_modelling/mean_aet.csv")
head(aet)

#load solar radiation flux data
srf <- readr::read_csv("data/processed/covariates_for_modelling/mean_srf.csv")
head(srf)

#combine all the environmental data
env_data <- cbind(climate_data, soil_data, swc, aet, srf)

#--Comment / uncomment next lines to remove or keep unconnected varieties
#--varieties to remove due to weak connectivity
var_to_remove <- c("Rosado", "BFS-24",
                   "SRS 56-3", "703-SM15216-11-4-VR",
                   "MHR 311-17", "RRH 336-28", "RS 907-28", "RS 909-35", "SRS2-36-34", "Seda")

for(i in var_to_remove){
  print(i)
  tricot_data[!is.na(tricot_data$variety_a) & tricot_data$variety_a == i,]$variety_a <- NA
  tricot_data[!is.na(tricot_data$variety_b) & tricot_data$variety_b == i,]$variety_b <- NA
  tricot_data[!is.na(tricot_data$variety_c) & tricot_data$variety_c == i,]$variety_c <- NA
}

to_remove <- c(which(is.na(tricot_data$variety_a) & is.na(tricot_data$variety_b)),
               which(is.na(tricot_data$variety_a) & is.na(tricot_data$variety_c)),
               which(is.na(tricot_data$variety_b) & is.na(tricot_data$variety_c)))

length(to_remove)

to_remove <- unique(to_remove)

length(to_remove)

tricot_data <- tricot_data[-to_remove, ]

env_data <- env_data[-to_remove, ]

#.---------END DATA REMOVE.-----------###

vars_to_remv <- c("tveg", "tflo", "tgra", "fdate", "gdate")

env_data <- env_data[, !colnames(env_data) %in% vars_to_remv]


#get rankings for PLT - Grouped
rbca_ranks <- gosset::rank_tricot(data = tricot_data,
                                  items = paste0("variety_", letters[1:3]),
                                  input = c("overall_best", "overall_worst"), group = TRUE
                                  )

rbca_data <- cbind(rbca_ranks, env_data)

# Use each trial as folds
rbca_folds <- as.integer(factor(tricot_data$trial_name))
rbca_folds

#add coordinates as covariates
# rbca_data$lat <- tricot_data$lat
# rbca_data$lon <- tricot_data$lon
# rbca_data$lon_plus_lat <- tricot_data$lon + tricot_data$lat
# rbca_data$lon_minus_lat <- tricot_data$lon - tricot_data$lat


