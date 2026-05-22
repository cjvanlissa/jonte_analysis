impute_missings <- function(df_split){
  df_split$train <- VIM::kNN(df_split$train, k = 100)
  df_split$train <- df_split$train[, !sapply(df_split$train, inherits, what = "logical")]
  df_split$test <- VIM::kNN(df_split$test, k = 100)
  df_split$test <- df_split$test[, !sapply(df_split$test, inherits, what = "logical")]
  return(df_split)
}