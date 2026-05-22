create_folds <- function(df_imp, cluster_variable = "id_exp", k = 10){
  train_id <- df_imp$train_id
  # Create k-folds ----------------------------------------------------------
  fold_ids <- sample.int(k, size = length(train_id), replace = TRUE)
  names(fold_ids) <- train_id
  fold_rownums <- lapply(1:k, function(i){
    which(df_imp$train[[cluster_variable]] %in% train_id[fold_ids == i])
  })
  names(fold_rownums) <- 1:k
  df_imp[["fold_rownums"]] <- fold_rownums
  df_imp[["fold_ids"]] <- fold_ids
  return(df_imp)
}
