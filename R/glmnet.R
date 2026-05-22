

do_glmnet <- function(dat, yvar = "yi", cluster_var = "ArticleID", ...){
  tuning_pars <- expand.grid(
    weights = c("unif", "fixed", "random")
  )
  
  foldid <- vector("numeric", length = nrow(dat$train))
  for(i in seq_along(dat$fold_rownums)){
    foldid[dat$fold_rownums[[i]]] <- i
  }
  
  X <- as.data.frame(model.matrix(as.formula(paste0("~", paste0(c(yvar, "vi", cluster_var, unique(unlist(moderator_list)))), collapse = " + ")), dat$train)[, -1])
  
  res_rma <- metaforest:::rma_dl(y = X$yi, v = X$vi)
  tau2_est <- res_rma$tau2
  
  wts_fe <- 1/X$vi
  wts_fe <- (nrow(X)*wts_fe)/sum(wts_fe) # Adds to nrow(dat), just like regular 1 weights
  
  wts_re <- 1/((X$vi + tau2_est))
  wts_re <- (nrow(X)*wts_re)/sum(wts_re) # Adds to nrow(dat), just like regular 1 weights
  res_models <- list(
  res_uni = glmnet::cv.glmnet(
    x = as.matrix(X[, -match(c(yvar, "vi", cluster_var), names(X))]),
    y = X[[yvar]],
    foldid = foldid
  ),
  res_fe = glmnet::cv.glmnet(
    x = as.matrix(X[, -match(c(yvar, "vi", cluster_var), names(X))]),
    y = X[[yvar]],
    foldid = foldid,
    weights = wts_fe
  ),
  res_re = glmnet::cv.glmnet(
    x = as.matrix(X[, -match(c(yvar, "vi", cluster_var), names(X))]),
    y = X[[yvar]],
    foldid = foldid,
    weights = wts_re
  )
  )

  lowest_cvm <- function(x){
    bestlambda <- which(x$lambda == x$lambda.1se)
    c(cvm = x$cvm[bestlambda], cvsd = x$cvsd[bestlambda], lambda = x$lambda.1se)
  }
  compare_models <- sapply(res_models, lowest_cvm)
  best_model <- names(which.min(compare_models[1,]))
  #compare_models <- compare_models[, which.min(compare_models[1,])]
  X_test <- as.data.frame(model.matrix(as.formula(paste0("~", paste0(c(yvar, "vi", cluster_var, unique(unlist(moderator_list)))), collapse = " + ")), dat$test)[, -1])
  missingcols <- setdiff(names(X), names(X_test))
  missingcols <- matrix(0, nrow = nrow(X_test), ncol = length(missingcols), dimnames = list(NULL, missingcols))
  X_test <- cbind(X_test, missingcols)[, names(X)]
  X_test <- as.matrix(X_test[, -match(c(yvar, "vi", cluster_var), names(X_test))])
  pred <- glmnet:::predict.cv.glmnet(res_models[[best_model]], newx = X_test, s = "lambda.1se")
  pred_train <- glmnet:::predict.cv.glmnet(res_models[[best_model]], newx = as.matrix(X[, -match(c(yvar, "vi", cluster_var), names(X))]), s = "lambda.1se")
  out <- list(
    res_cv = res_models[[best_model]],
    tune_pars = c(weights = best_model, lambda = compare_models[3, best_model]),
    mse_cv = compare_models[1:2, best_model],
    rsq = rsq_numeric(dat$test[[yvar]], pred, mean(dat$train[[yvar]])),
    rsq_train = rsq_numeric(dat$train[[yvar]], pred_train, mean(dat$train[[yvar]]))
  )
  class(out) <- "res_glmnet_meta"
  return(out)
}