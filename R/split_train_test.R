split_train_test <- function(df, cluster_variable = "ArticleID", train_size = .7){
  # Split the data into train and test data sets ----------------------------
  n <- length(unique(df[[cluster_variable]]))
  train_id <- sample(unique(df[[cluster_variable]]), size = floor(train_size*n))
  df_train <- df[which(df[[cluster_variable]] %in% train_id), ]

  df_test <- df[-which(df[[cluster_variable]] %in% train_id), ]
  test_id <- setdiff(unique(df[[cluster_variable]]), train_id)

  return(
    list(
      train = df_train,
      test = df_test,
      train_id = train_id,
      test_id = test_id
    )
  )
}
