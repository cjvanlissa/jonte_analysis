do_metacart <- function(dat, ...) {
  library(metacart)
  tuning_pars <- expand.grid(
    whichweights = c("REmrt", "FEmrt"),
    c.pruning = c(.2, .5, .7, .9, 1),
    stringsAsFactors = FALSE
  )
  X <- model.matrix(yi ~.-vi-ArticleID-effectID, dat$train)[, -1]
  Y <- as.numeric(dat$train$yi)
  cv_rmses <- sapply(1:nrow(tuning_pars), function(i) {
    sapply(dat$fold_rownums, function(thisfold) {
      Args <- list(
        formula = ~.,
        data = data.frame(X[-thisfold, ]),
        vi = dat$train$vi[-thisfold],
        c.pruning = tuning_pars$c.pruning[i],
        xval = 1)
      
      fit_cv <- try(suppressWarnings(do.call(tuning_pars$whichweights[i], Args)))
      if(!inherits(fit_cv, "try-error")){
        preds <- suppressWarnings(unlist(predict(fit_cv, newdata = dat$train[thisfold, ])))
        mean((dat$train$yi[thisfold] - preds)^2)
      } else {
        write(paste0("Error in tuning_pars: ", i), "metacart.log", append = TRUE, sep = "\n")
        mean((dat$train$yi[thisfold] - mean(dat$train$yi[thisfold]))^2)
      }
    })
  })
  
  cvm <- colMeans(cv_rmses)
  Args <- list(
    formula = as.formula(paste0("yi ~", paste0(setdiff(names(dat$train), c("yi", "vi")), collapse = " + ")
    )),
    data = dat$train,
    vi = dat$train$vi,
    c.pruning = tuning_pars$c.pruning[which.min(cvm)],
    xval = 10)
  best_model <- suppressWarnings(do.call(tuning_pars$whichweights[which.min(cvm)], Args))
  pred <- suppressWarnings(unlist(predict(best_model, newdata = dat$test)))
  pred_train <- suppressWarnings(unlist(predict(best_model, newdata = dat$train)))
  
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
  class(out) <- "res_metacart"
  return(out)
}


print.REmrt <- function (x, ...)
{
  if (!is.null(x$pruned)) {
    if (x$pruned == FALSE) {
      cat("\n")
      cat("The following tree is the initial tree and it has not been pruned yet.")
      cat("\n")
      cat("Random Effects Meta-tree (K = ", sum(x$initial),
          " studies); ", sep = "")
      cat("\n")
      cat("A tree with ", length(x$initial), " terminal nodes was detected",
          sep = "")
      cat("\n")
      cat("The moderators are ", paste(as.character(x$mod[!is.na(x$mod)]),
                                       collapse = ", "), sep = "")
      cat("\n")
      cat("use summary() and plot() to see the moderator analysis results and the tree structure")
      cat("\n")
      cat("\n")
      rows <- grep("<", x$tree$split)
      for (i in rows) {
        x$tree$split[i] <- paste0(substr(x$tree$split[i],
                                         1, unlist(gregexpr("<", x$tree$split[i])) +
                                           1), " ", round(as.numeric(substr(x$tree$split[i],
                                                                            unlist(gregexpr("<", x$tree$split[i])) + 2,
                                                                            nchar(x$tree$split[i]))), 2))
      }
      print(x$tree)
    }
  }
  else {
    if (length(x$n) < 2) {
      cat("\n")
      cat("Random Effects Meta-Tree (K = ", sum(x$n),
          " studies); ", sep = "")
      cat("\n")
      cat("No moderator effect was detected")
      cat("\n")
      cat("Use summary() to inspect the meta-analysis results")
    }
    else {
      cat("\n")
      cat("Random Effects Meta-tree (K = ", sum(x$n),
          " studies); ", sep = "")
      cat("\n")
      cat("A tree with ", length(x$n), " terminal nodes was detected",
          sep = "")
      cat("\n")
      cat("The moderators are ", paste(as.character(x$moderators),
                                       collapse = ", "), sep = "")
      cat("\n")
      cat("use summary() and plot() to see the moderator analysis results and the tree structure")
      cat("\n")
      rows <- grep("<", x$tree$split)
      for (i in rows) {
        x$tree$split[i] <- paste0(substr(x$tree$split[i],
                                         1, unlist(gregexpr("<", x$tree$split[i])) +
                                           1), " ", round(as.numeric(substr(x$tree$split[i],
                                                                            unlist(gregexpr("<", x$tree$split[i])) + 2,
                                                                            nchar(x$tree$split[i]))), 2))
      }
      print(x$tree)
    }
  }
}

print.FEmrt <- function (x, ...)
{
  if (length(x$n) < 2) {
    cat("\n")
    cat("Fixed Effects Meta-Tree (K = ", sum(x$n), " studies); ",
        sep = "")
    cat("\n")
    cat("No moderator effect was detected")
    cat("\n")
    cat("use summary() to see the meta-analysis results")
  }
  else {
    cat("\n")
    cat("Fixed Effects Meta-tree (K = ", sum(x$n), " studies); ",
        sep = "")
    cat("\n")
    cat("A tree with ", length(x$n), " terminal nodes was detected",
        sep = "")
    cat("\n")
    cat("The moderators are ", paste(as.character(x$moderators),
                                     collapse = ", "), sep = "")
    cat("\n")
    cat("Use summary() and plot() to inspect the moderator analysis results and the tree structure.")
    cat("\n")
    x$tree$splits[, 4L] <- round(x$tree$splits[, 4L], 2)
    print(x$tree)
  }
}