library(climatrends)

get_phen_indx <- function(dt, mdt, mnt){

dt$hdate <- as.Date(dt$hdate)
dt$pdate <- as.Date(dt$pdate)

# table with thermal time required by beans
# https://www.hindawi.com/journals/ija/2016/8065985/
# in this order: vegetative, flowering, grain development
pheno <- data.frame(stage = c("veg","flo","gra"),
                    code  = c("V0-R5","R5-R7","R7-H"),
                    gdd   = c(430, 185, 485))

nr <- dim(dt)[[1]]

# compute the estimated days and dates from 
# planting to the end of each period
fdate <- rep(NA, nr)
gdate <- rep(NA, nr)
tveg <- rep(NA, nr)
tflo <- rep(NA, nr)
tgra <- rep(NA, nr)
ind_veg <- data.frame()
ind_flo <- data.frame()
ind_gra <- data.frame()
cropsen_flo <- data.frame()
cropsen_all <- data.frame()

for(i in seq_len(nr)){
  tmax <- mdt_values[dt[i,]$pdate:dt[i,]$hdate]
  tmin <- mnt_values[dt[i,]$pdate:dt[i,]$hdate]
  
  # duration of vegetative stage
  gv <- GDD(tmax, 
            tmin, 
            #tbase = 5,
            tbase = 10,#chagend to 10 for common bean
            degree.days = pheno[pheno$stage == "veg", "gdd"], 
            return.as = "ndays")$gdd
  
  # date where flowering stage started
  fdate[i] <- dt$pdate[i] + gv + 1
  
  # duration of vegetative stage
  tveg[i] <- gv
  
  # indices for the vegetative stage
  x <- temperature(tmax[1:gv], 
                   tmin = tmin[1:gv])
  
  ind_veg <- rbind(ind_veg, x)
  
  # duration of flowering stage
  gf <- GDD(tmax[(gv + 1):length(tmax)], 
            tmin[(gv + 1):length(tmax)], 
            #tbase = 5,
            tbase = 10,#chagend to 10 for common bean
            degree.days = pheno[pheno$stage == "flo", "gdd"], 
            return.as = "ndays")$gdd
  
  # date where grain development stage started
  gdate[i] <- dt$pdate[i] + gv + gf + 1
  
  # indices for the flowering stage
  x <- temperature(tmax[(gv + 1):(gv + gf)], 
                   tmin = tmin[(gv + 1):(gv + gf)])
  ind_flo <- rbind(ind_flo, x)
  
  # crop sensitive indices
  x <- crop_sensitive(tmax[(gv + 1):(gv + gf)], 
                      tmin = tmin[(gv + 1):(gv + gf)],
                      #hts_mean.threshold = c(24, 26),
                      #Agtunong, T., Redden, R., Mangge-Nang, M., Searle, C., & Fukai, S. (1992). Genotypic variation in response to high temperature at flowering in common bean (<I>Phaseolus vulgaris</I> L.). Australian Journal of Experimental Agriculture, 32(8), 1135-1140. doi:https://doi.org/10.1071/EA9921135
                      hts_mean.threshold = c(19, 25),
                      #hts_max.threshold = c(31, 33)
                      #changed following Gross & Kiegel (1994)
                      hts_max.threshold = c(26, 32),
                      hse.threshold = 35)
  #print(x)
  
  # remove cdi index
  x <- x[, -which(grepl("cdi_", names(x)))]
  cropsen_flo <- rbind(cropsen_flo, x)
  
  # duration of flowering stage
  tflo[i] <- gf
  # duration of grain development stage
  gg <- GDD(tmax[(gv + gf + 1):length(tmax)], 
            tmin[(gv + gf + 1):length(tmax)], 
            #tbase = 5,
            tbase = 10,#chagend to 10 for common bean
            degree.days = pheno[pheno$stage == "gra", "gdd"], 
            return.as = "ndays")$gdd
  
  tgra[i] <- gg
  
  # indices for the grain development stage
  x <- temperature(tmax[(gg + 1):(gv + gf + gg)], 
                   tmin = tmin[(gg + 1):(gv + gf + gg)])
  
  ind_gra <- rbind(ind_gra, x)
  
  
  # crop sensitive indices for the whole development
  # x <- crop_sensitive(tmax[1:(gv + gf + gg)], 
  #                     tmin = tmin[1:(gv + gf + gg)],
  #                     hts_mean.threshold = c(24, 26),
  #                     hts_max.threshold = c(31, 33))
  
  #changed to avoid NAs
  x <- crop_sensitive(#tmax[1:(gv + gf + gg)], 
    tmax[1:length(tmax)],
    #tmin = tmin[1:(gv_T + gf_T + gg_T)],
    tmin = tmin[1:length(tmax)],
    #hts_mean.threshold = c(24, 26),
    #Agtunong, T., Redden, R., Mangge-Nang, M., Searle, C., & Fukai, S. (1992). Genotypic variation in response to high temperature at flowering in common bean (<I>Phaseolus vulgaris</I> L.). Australian Journal of Experimental Agriculture, 32(8), 1135-1140. doi:https://doi.org/10.1071/EA9921135
    hts_mean.threshold = c(19, 25),
    #hts_max.threshold = c(31, 33)
    #changed following Gross & Kiegel (1994)
    hts_max.threshold = c(26, 32),
    hse.threshold = 35)
  
  
  
  
  cropsen_all <- rbind(cropsen_all, x)
  print(i)
}

# rename indices
names(ind_veg) <- paste0(names(ind_veg), "_veg")
names(ind_flo) <- paste0(names(ind_flo), "_flo")
names(ind_gra) <- paste0(names(ind_gra), "_gra")
names(cropsen_flo) <- paste0(names(cropsen_flo), "_flo")
names(cropsen_all) <- paste0(names(cropsen_all), "_all")

ind <- cbind(ind_veg, ind_flo, ind_gra, cropsen_flo, cropsen_all)

return(ind)

}






