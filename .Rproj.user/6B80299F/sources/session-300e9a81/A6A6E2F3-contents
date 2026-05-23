mlrf <- function (formula, data, vi = "vi", study = NULL, whichweights = "random", num.trees = 500,
          mtry = NULL, min.node.size = 5, method = "REML", ...)
{
  data$ES_ID <- 1:nrow(data)
  v <- data[[vi]]
  id <- data[[study]]
  mf <- model.frame(formula = formula, data = data)
  yi <- mf[[1]]
  X <- model.matrix(formula, data)[, -1, drop = FALSE]
  if(study %in% colnames(X)) X <- X[, !colnames(X) == study]
  if(vi %in% colnames(X)) X <- X[, !colnames(X) == vi]
  # Your exact 3-level meta-analytic model
  m3 <- metafor::rma.mv(
    yi = yi,
    V  = v,
    random = ~ 1 | id_exp/ES_ID,
    data = data,
    method = "REML"
  )

  resid <- residuals(m3, type = "response")
  wts <- sqrt(1/v)

  rf_mod <- ranger::ranger(
    x = X,
    y = yi,
    case.weights = wts,
    num.trees = num.trees,
    importance = "permutation")

  out <- list(m3 = m3, rf = rf_mod)
  class(out) <- c("mlrf", class(out))
  return(out)
}

predict.mlrf <- function(object, newdata = NULL, ...){
  predict(object[["m3"]])$pred + ranger:::predict.ranger(object[["rf"]], data = newdata, type = "response")$predictions
}

do_mlrf <- function(dat, yvar = "yi", ...) {
  Args <- list(
    formula = as.formula(paste0(
      yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi", "id_exp")), collapse = " + ")
    )),
    data = dat$train,
    study = "id_exp"
  )
  cv_rmses <- sapply(dat$fold_rownums, function(thisfold) {
    Args$data = dat$train[-thisfold, ]
    fit_cv <- do.call(mlrf, Args)
    preds <- predict.mlrf(fit_cv, newdata = dat$train[thisfold, ])
    mean((dat$train$yi[thisfold] - preds)^2)
  })

  best_model <- do.call(mlrf, Args)
  pred <- predict.mlrf(best_model, newdata = dat$test)

  pred_train <- predict.mlrf(best_model, newdata = dat$train)

  out <- list(
    res_cv = cv_rmses,
    res = best_model,
    tune_pars = NA,
    mse_cv = c(cvm = mean(cv_rmses), cvsd = sd(cv_rmses)),
    rsq = rsq_numeric(
      dat$test$yi,
      as.numeric(pred),
      mean(dat$train$yi)
    ),
    rsq_train = rsq_numeric(
      dat$train$yi,
      as.numeric(pred_train),
      mean(dat$train$yi)
    )
  )
  class(out) <- "res_mlrf"
  return(out)
}
