do_metaforest <- function(dat, ...) {
  yvar = "yi"
  tuning_pars <- expand.grid(
    whichweights = c("random", "fixed", "unif"),
    mtry = ceiling(exp(seq(log(1), log(ncol(dat$train)-4L), length.out = 10))),
    min.node.size = round(seq.int(
      from = 10,
      to = ceiling(.2 * nrow(dat$train)),
      length.out = 10
    )),
    stringsAsFactors = FALSE
  )
  cv_rmses <- sapply(1:nrow(tuning_pars), function(i) {
    sapply(dat$fold_rownums, function(thisfold) {
      Args <- list(
        formula = as.formula(paste0(
          yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi", "id_exp")), collapse = " + ")
        )),
        data = dat$train[-thisfold, ],
        mtry = tuning_pars$mtry[i],
        study = "id_exp",
        whichweights = tuning_pars$whichweights[i],
        min.node.size = tuning_pars$min.node.size[i]
      )
      fit_cv <- do.call(metaforest::MetaForest, Args)
      preds <- metaforest:::predict.MetaForest(fit_cv, data = dat$train[thisfold, ])$predictions
      mean((dat$train$yi[thisfold] - preds)^2)
    })
  })

  cvm <- colMeans(cv_rmses)
  Args <- list(
    formula = as.formula(paste0(
      yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi", "id_exp")), collapse = " + ")
    )),
    data = dat$train,
    mtry = tuning_pars$mtry[which.min(cvm)],
    study = "id_exp",
    whichweights = tuning_pars$whichweights[which.min(cvm)],
    min.node.size = tuning_pars$min.node.size[which.min(cvm)]
  )

  best_model <- do.call(metaforest::MetaForest, Args)
  pred <- metaforest:::predict.MetaForest(best_model, data = dat$test)$predictions

  pred_train <- metaforest:::predict.MetaForest(best_model, data = dat$train)$predictions

  out <- list(
    res_cv = cv_rmses,
    res = best_model,
    tune_pars = as.vector(tuning_pars[which.min(cvm), , drop = FALSE]),
    mse_cv = c(cvm = min(cvm), cvsd = sd(cv_rmses[, which.min(cvm)])),
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
  class(out) <- "res_metaforest"
  return(out)
}
