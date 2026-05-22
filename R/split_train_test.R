split_train_test <- function(df, cluster_variable = "ArticleID", train_size = .7){
  # Split the data into train and test data sets ----------------------------
  n <- length(unique(df[[cluster_variable]]))
  train_id <- sample(unique(df[[cluster_variable]]), size = floor(train_size*n))
  df_train <- df[which(df[[cluster_variable]] %in% train_id), ]
  
  df_test <- df[-which(df[[cluster_variable]] %in% train_id), ]
  test_id <- setdiff(unique(df[[cluster_variable]]), train_id)
  
  # Clean Data
  
  facs <- names(df_train)[sapply(df_train, inherits, what = c("factor", "character"))]
  df_train[facs] <- lapply(df_train[facs], factor)

  nums <- names(df_train)[sapply(df_train, inherits, what = c("numeric", "integer"))]
  nums <- setdiff(nums, c("yi", "vi", cluster_variable)) # Exclude DV
  scld <- scale(df_train[nums], center = TRUE, scale = TRUE)
  means <- attr(scld, "scaled:center")
  sds <- attr(scld, "scaled:scale")
  df_train[nums] <- scld
  
  scld_test <- df_test[nums]
  scld_test <- sweep(scld_test, 2, means)
  scld_test <- sweep(scld_test, 2, sds, FUN = "/")
  df_test[nums] <- scld_test
  df_test[facs] <- lapply(df_test[facs], factor)
  
  return(
    list(
      train = df_train,
      test = df_test,
      train_id = train_id,
      test_id = test_id,
      train_means = means,
      train_sds = sds
    )
  )
}