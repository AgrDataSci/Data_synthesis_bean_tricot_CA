library(PlackettLuce)
library(gosset)
library(caret)

library(abind)
library(foreach)
library(parallel)

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/model_selection_session_info.txt")


#' Combine parallel outputs
#' @param ... arguments passed to methods
#' @noRd
.comb <- function(...) {
  abind::abind(..., along = 1, force.array = TRUE)
}

#' Model call for parallel
#' @param formula a symbolic description of the model, if set as   
#'   \eqn{ y ~ . } all variables in \code{data} are used 
#' @param args a list with model call arguments 
#' @param estimator the name of estimators to select
#' @noRd
.forward_dopar <- function(formula, args){
  
  args <- c(formula, args)
  
  m <- do.call(gosset::crossvalidation, args)

  result <- m$raw$estimators

  nfold <- m$raw$k

  result  <- array(unlist(result), c(1, nfold, ncol(result)))
  
  return(result)
}

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

G <- rbca_ranks

pld <- cbind(G, covar)

head(pld)

# run a forward selection with cross-validation
# to select the best model
# Define initial parameters 

#set the baseline as a deviance from a Plackett-Luce model without covariates
#baseline <- NULL
#First run cv_baselin_model.R
baseline <- sum(deviance_bsl)

# vector to keep best explanatory variables
covar_keep <- character(0L)

# keep running if TRUE
best <- TRUE

# number of runs
counter <- 1

# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()

# get the names of explanatory and response variables

covarnames <- names(covar)
covarnames

folds <- rbca_folds
k <- max(folds)
siglevel <- .9
npseudo <- 0.5
bonferroni <- TRUE
sendpkg <- c("PlackettLuce", "partykit")
select_by <- "deviance"

# create cluster to do parallelisation
ncores <- parallel::detectCores() - 2
cluster <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cluster)

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(data = pld,
               k = k,
               folds = folds,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               dfsplit = FALSE
               )
  
  i <- seq_len(fs)
  
  models <- foreach(i = i, .combine = .comb, .packages = sendpkg) %dopar%
    (.forward_dopar(as.formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + "))),
                    args))
  

  dimnames(models) <- list(seq_len(fs),
                           paste0("bin", seq_len(k)),
                           c("deviance", "AIC","logLik", "logLikNull",
                             "MaxLik","CraggUhler", "Agresti", "kendall"))
  
  #take the matrix with selected goodness of fit
   GOF <- models[, , dimnames(models)[[3]] %in% select_by]
  
  #sum the deviances of the folds
  deviances <- apply(t(GOF), 2, sum)
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
    best_model <- covarnames[best_dv]
  
  
  # compare if the best value is better than the baseline
  #ifelse(counter == 1, baseline <- value_best, best <- value_best < baseline)
  
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}

# Stop cluster connection
parallel::stopCluster(cluster)


