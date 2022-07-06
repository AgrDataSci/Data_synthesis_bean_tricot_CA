# read rankings data with covariates
source("script/utils/load_rbca_data.R")

#put covariates in a separate df for preprocessing
covar <- rbca_data[, -1]

names(covar)

#remove covariates with near zero variance
out_r <- caret::nearZeroVar(covar)

names(covar)[out_r]

covar <- covar[, -out_r]

colnames(covar)

ct <- cor.test(covar$maxNT, covar$WSDI)
round(ct$estimate, 2)

format(round(ct$p.value, 4), scientific = TRUE)

