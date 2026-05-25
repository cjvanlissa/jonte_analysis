do_brma <- function(dat, yvar = "yi", cluster_var = "id_exp", ...){
  tuning_pars <- expand.grid(
    relevant_pars = ceiling(exp(seq(log(1), log(ncol(dat$train)-4L), length.out = 10))),
    interactions = c(FALSE, TRUE)
  )
  # X <- model.matrix(as.formula(paste0(yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi", cluster_var)), collapse = " + "))), dat$train)[, -1]
  X <- dat$train[, -which(names(dat$train) %in% c("yi", "vi", "id_exp"))]
  X2 <- model.matrix( ~.^2, data=X)[, -1]
  cv_rmses <- sapply(1:nrow(tuning_pars), function(i){
    sapply(dat$fold_rownums, function(thisfold){
      if(tuning_pars$interactions[i]){
        X <- X2
      } else {
        X <- X
      }
      std <- list(center = rep(0, ncol(X)), scale = rep(1, ncol(X)))
      X_train <- X[-thisfold, ]
      X_test <- X[thisfold, ]
      Y_train <- dat$train$yi[-thisfold]
      Y_test <- dat$train$yi[thisfold]
      fit_cv <- pema::brma(
        x = X_train,
        y = Y_train,
        vi = dat$train$vi[-thisfold],
        study = dat$train$id_exp[-thisfold],
        method = "hs",
        standardize = std,
        prior = c(relevant_pars = tuning_pars$relevant_pars[i]),
        intercept = TRUE,
        iter = 5000
      )
      preds <- pema:::predict.brma(fit_cv, newdata = X_test, type = c("mean"))
      mean((Y_test - preds)^2)
    })
  })

  cvm <- colMeans(cv_rmses)

  # std <- list(center = rep(0, ncol(X)), scale = rep(1, ncol(X)))
  if(tuning_pars$interactions[which.min(cvm)]){
    X <- X2
  } else {
    X <- X
  }
  std <- list(center = rep(0, ncol(X)), scale = rep(1, ncol(X)))
  best_model <- pema::brma(
    x = X,
    y = dat$train$yi,
    vi = dat$train$vi,
    study = dat$train$id_exp,
    method = "hs",
    standardize = std,
    prior = c(relevant_pars = tuning_pars$relevant_pars[which.min(cvm)]),
    intercept = TRUE,
    iter = 5000
  )

  #X_holdout <- model.matrix(as.formula(paste0(yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi", cluster_var)), collapse = " + "))), dat$test)[, -1]
  X_holdout <- dat$test[, -which(names(dat$test) %in% c("yi", "vi", "id_exp"))]
  # if(any(! colnames(X) %in% colnames(X_holdout))){
  #   add_these <- matrix(0, ncol = sum(! colnames(X) %in% colnames(X_holdout)), nrow = nrow(X_holdout))
  #   colnames(add_these) <- setdiff(colnames(X), colnames(X_holdout))
  #   X_holdout <- cbind(X_holdout, add_these)
  # }
  # X_holdout <- X_holdout[, colnames(X)]

  preds <- pema:::predict.brma(best_model, newdata = X_holdout, type = c("mean"))

  pred_train <- pema:::predict.brma(best_model, newdata = X, type = c("mean"))
  out <- list(
    res_cv = cv_rmses,
    res = best_model,
    tune_pars = as.vector(tuning_pars[which.min(cvm), , drop = FALSE]),
    mse_cv = c(cvm = min(cvm),
               cvsd = sd(cv_rmses[, which.min(cvm)])),
    rsq = rsq_numeric(dat$test$yi, as.numeric(preds), mean(dat$train$yi)),
    rsq_train = rsq_numeric(dat$train$yi, as.numeric(pred_train), mean(dat$train$yi))
  )
  class(out) <- "res_brma"
  return(out)
}

final_brma <- function(df, res_brma){
  X <- df[, -which(names(df) %in% c("yi", "vi", "id_exp"))]
  X2 <- model.matrix( ~.^2, data=X)[, -1]
  if(res_brma$tune_pars$interactions){
    X <- X2
  } else {
    X <- X
  }
  std <- list(center = rep(0, ncol(X)), scale = rep(1, ncol(X)))
  pema::brma(
    x = X,
    y = df$yi,
    vi = df$vi,
    study = df$id_exp,
    method = "hs",
    standardize = std,
    prior = c(relevant_pars = res_brma$tune_pars$relevant_pars),
    intercept = TRUE,
    iter = 5000
  )
}
