# Tree
do_tree <- function(dat){
  library(rpart)
  tune_grid <- expand.grid(
    cp = c(.001, .01, .1, .2, .3, .6)
  )
  df_train <- data.frame(yi = dat$train$yi, model.matrix(yi~.-vi-ArticleID,  data = dat$train)[, -1])
  df_test <- data.frame(yi = dat$test$yi, model.matrix(yi~.-vi-ArticleID,  data = dat$test)[, -1])
  df_test[which(!names(df_test) %in% names(df_train))] <- NULL
  df_train[which(!names(df_train) %in% names(df_test))] <- NULL
  if(!(all(names(df_train) %in% names(df_test)) & all(names(df_test) %in% names(df_train)) )) stop()
  res_tune <- sapply(1:nrow(tune_grid), function(i){
    sapply(dat$fold_rownums, function(f){
      Args <- list(
        formula = yi~., 
        data = df_train[-f, ],
        method = "anova",
        control = do.call(rpart.control, args = as.list(tune_grid[i, , drop = F]))
      )
      tree_model <- do.call(rpart, args = Args)
      preds <- predict(tree_model, newdata = df_train[f, ])
      mean((dat$train$yi[f]-preds)^2)
    })
  })
  
  Args <- list(
    formula = quote(yi ~.),
    data = df_train,
    method = "anova",
    control = do.call(rpart.control, args = as.list(tune_grid[which.min(colMeans(res_tune)), , drop = F]))
  )
  tree_model <- do.call(rpart, args = Args)
  
  pred <- predict(tree_model, newdata = df_test)
  pred_train <- predict(tree_model, newdata = df_train)
  
  out <- list(
    res_cv = res_tune,
    res = tree_model,
    tune_pars = as.vector(tune_grid[which.min(res_tune), , drop = FALSE]),
    mse_cv = res_tune[, which.min(colMeans(res_tune)), drop = TRUE],
    rsq = rsq_numeric(df_test$yi, pred, mean(df_train$yi))
    , rsq_train = rsq_numeric(df_train$yi, pred_train, mean(df_train$yi))
  )
  class(out) <- "res_tree"
  return(out)
}