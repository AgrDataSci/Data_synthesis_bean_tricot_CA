#Code for table request by reviewer to be included as SI

source("script/utils/load_rbca_data.R")

load("data/processed/env_newdata/newdata_clim_all.rda")

tricot_env <- cbind(tricot_data, env_data)

sel_col <- c("trial_name",
             grep("veg|gra|flo",
                  colnames(tricot_env),
                  value = TRUE))

tricot_env <- tricot_env[, colnames(tricot_env) %in% sel_col]

mean_clim_trial <- aggregate(tricot_env[-1],
                             by = list(trial = tricot_env$trial_name),
                             FUN = mean)


write.table(mean_clim_trial, 
            file = "output/mean_clim_trial.csv",
            sep = ",",
            row.names = FALSE)

