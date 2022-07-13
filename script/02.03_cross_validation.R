library(PlackettLuce)
library(gosset)
library(caret)

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



cross_validation <- function(.formula, .data, .folds){
  
  
  base_deviance <- numeric(length = max(.folds))
  
  deviance_a <- numeric(length = max(.folds))
  
  r2_a <- numeric(length = max(.folds))
  
  kw <- numeric(length = max(.folds))
  
  kw_2 <- numeric(length = max(.folds))
  
  kt <- numeric(length = max(.folds))
  
  fold_size <- table(.folds)
  
  plt_models <- vector(mode = "list", length = max(.folds))
  
  res <- vector(mode = "list", length = 5)
  
  for(j in 1:max(.folds)){
    
    train_data <- .data[.folds != j, ]
    
    test_data <- .data[.folds == j, ]
    
    plt_model <- PlackettLuce::pltree(as.formula(.formula),
                                      data = train_data,
                                      npseudo = 0.5,
                                      alpha = .9,
                                      bonferroni = TRUE,
                                      verbose = FALSE,
                                      ncores = 6,
                                      dfsplit = FALSE
                                      )
    
    plt_models[[j]] <- plt_model
    
    deviance_a[j] <- deviance(plt_model, newdata = test_data)#, method = "tree")
    
    pred_rank <-  predict(plt_model, newdata = test_data, type = "rank")
    
    obs_rank <- rbca_ranks[.folds == j, , as.grouped_rankings = FALSE]
    
    r2_a[j] <- gosset::pseudoR2(plt_model, newdata = test_data)[[5]]
    
    kt[j] <- gosset::kendallTau(x = obs_rank, y = pred_rank)[[1]]
    
    kw[j] <- mean(unlist(lapply(X = seq_len(nrow(pred_rank)),
                                       FUN = function(X){
                                         DescTools::KendallW(data.frame("pred" = pred_rank[X, ],
                                                                        "obs" = obs_rank[X, ]),
                                                             correct = TRUE,
                                                             test = TRUE)$estimate
                                       }
                                       )
                                )
                         )
    
    
    kw_2[j] <- mean(unlist(lapply(X = seq_len(nrow(pred_rank)),
                                  FUN = function(X){
                                    #print(X)
                                    a <- obs_rank[X, ]
                                    b <- pred_rank[X,]
                                    #b[b==0] <-NA
  
                                    a <- a[a > 0]
                                    #print(a)

                                    b <- b[names(b)%in%names(a)]

                                    #print(b)

                                    DescTools::KendallW(data.frame("obs" = a,
                                                                   "pred" = b),
                                                        correct = TRUE,
                                                        test = TRUE)$estimate}
    )
    )
    )
    
  }
  
  sum_deviance_a <- sum(deviance_a)
  
  #Pseudo R2 weighted by the fold size
  mean_r2_a <- sum(r2_a * fold_size) / sum(fold_size)
  
  mean_kw <- sum(kw * fold_size) / sum(fold_size)
  
  mean_kw_2 <- sum(kw_2 * fold_size) / sum(fold_size)
  
  mean_kt <- sum(kt * fold_size) / sum(fold_size)
  
  res[[1]] <- data.frame(sum_deviance_a,
                         mean_r2_a,
                         mean_kw, 
                         mean_kw_2,
                         mean_kt)
  
  res[[2]] <- plt_models
  
  res[[3]] <- r2_a
  
  res[[4]] <- deviance_a
  
  res[[5]] <- kw
  
  res[[6]] <- kw_2
  
  return(res)
  
}


#Environmental covariates
cv_plt_01 <- cross_validation(.formula = rbca_ranks ~ WSDI + R20mm + T10p + hts_mean_19_flo,
                              .data = rbca_data,
                              .folds = rbca_folds
                              )
cv_plt_01[[1]]
cv_plt_01[[1]][3]


round(cv_plt_01[[1]], 4)

lapply(cv_plt_01[[2]], gosset:::get_rules_labels)

data.frame("fold" = 1:14, "McFadden R2" = cv_plt_01[[3]], "Kendall-W" = cv_plt_01[[5]])

#Environmental covariates + geolocation
cv_plt_02 <- cross_validation(.formula = rbca_ranks ~ WSDI + R20mm + T10p + hts_mean_19_flo +
                                lon + lat + lon_plus_lat + lon_minus_lat,
                              .data = rbca_data,
                              .folds = rbca_folds
                              )

round(cv_plt_02[[1]], 4)

cv_plt_02[[4]]

#Only geolocation
cv_plt_03 <- cross_validation(.formula = rbca_ranks ~ lon + lat + lon_plus_lat + lon_minus_lat,
                              .data = rbca_data,
                              .folds = rbca_folds)

round(cv_plt_03[[1]], 4)

#Baseline model (NULL)
cv_plt_04 <- cross_validation(.formula = rbca_ranks ~ 1,
                              .data = rbca_data,
                              .folds = rbca_folds)

round(cv_plt_04[[1]], 4)


cv_results <- rbind(cbind("model" = "no covariates", round(cv_plt_04[[1]], 4)),
                    cbind("model" = "env covariates", round(cv_plt_01[[1]], 4)),
                    cbind("model" = "env + geo covariates", round(cv_plt_02[[1]], 4)),
                    cbind("model" = "geo only", round(cv_plt_03[[1]], 4)))

write.table(cv_results, file = "output/cv_results.csv", sep = ",", row.names = FALSE)


#### End of script ####

