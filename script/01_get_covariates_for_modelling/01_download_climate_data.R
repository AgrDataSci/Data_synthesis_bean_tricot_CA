#Downloads AgERA5 data for modelling (2015-2015) and predictions (2000-2020)
#Red bean Tricot trial data from Central America
library(agera5)
library(terra)

months_list <- formatC(x = seq(1:12), width = 2, flag = 0)

#change as required
years_list <- 2000:2014

# #download precipitation data
# for(i in seq_along(years_list)){
#   for(j in months_list){
#     agera5:::download_agera5(agera5_var = "precipitation_flux",
#                              day = "all",
#                              month = j,
#                              year = years_list[i],
#                              path = paste0("--Put target location here.../prec/", 
#                                          years_list[i]))
#   }
# }
# 
# 
# agera5:::get_temp_stats()
# 
# agera5::get_temp_stats_cds()
# 
# #download Maximum day temperature
# for(i in seq_along(years_list)){
#   for(j in months_list){
#     agera5:::download_agera5(agera5_var = "2m_temperature",
#                              agera5_stat = "day_time_maximum",
#                              day = "all",
#                              month = j,
#                              year = years_list[i],
#                              path = paste0("--Put target location here.../temp/", 
#                                            years_list[i]))
#   }
# }



#download Minimum night temperature
#year 
# for(i in seq_along(years_list)){
#   for(j in months_list){
#     agera5:::download_agera5(agera5_var = "2m_temperature",
#                              agera5_stat = "night_time_minimum",
#                              day = "all",
#                              month = j,
#                              year = years_list[i],
#                              path = paste0("--Put target location here.../temp/", 
#                                            years_list[i]))
#   }
# }

#Solar radiation flux
# for(i in seq_along(years_list)){
#   for(j in months_list){
#     agera5:::download_agera5(agera5_var = "solar_radiation_flux",
#                              day = "all",
#                              month = j,
#                              year = years_list[i],
#                              path = paste0("--Put target location here.../srf/", 
#                                            years_list[i]))
#   }
# }





