library(PlackettLuce)
library(gosset)

# read rankings data with covariates
source("script/utils/load_rbca_data.R")

plt_models <- vector(mode = "list", length = max(rbca_folds))

deviance_bsl <- numeric(length = max(rbca_folds))

r2_bsl <- numeric(length = max(rbca_folds))

for(i in 1:max(rbca_folds)){
  
  train_data <- rbca_data[rbca_folds != i, ]
  
  test_data <- rbca_data[rbca_folds == i, ]
  
  plt_model <- PlackettLuce::pltree(rbca_ranks ~ 1,
                                    data = train_data,
                                    npseudo = 0.5,
                                    alpha = .9,
                                    bonferroni = TRUE,
                                    verbose = FALSE,
                                    ncores = 6,
                                    #prune = "AIC",
                                    dfsplit = F
                                    )
  
  plt_models[[i]] <- plt_model
  
  deviance_bsl[i] <- deviance(plt_model, newdata = test_data)
  
  r2_bsl[i] <- pseudoR2(plt_model, newdata = test_data)[[5]]
  
}

sum(deviance_bsl)

round(sum(table(rbca_folds) * unlist(r2_bsl))/sum(table(rbca_folds)), 3)
