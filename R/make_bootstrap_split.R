make_balanced_folds <- function(df, predictors, k = 8, cluster_var = NULL, max_tries = 100) {
  # Check that each level occurs at least k times
  df_pred <- df[, predictors]
  level_counts <- lapply(df_pred, table)

  too_rare <- lapply(level_counts, function(tab) tab < k)

  if (any(unlist(too_rare))) {
    stop(paste0("Each factor level must occur at least k times to appear in every fold. You can make at most ", min(unlist(level_counts)), " folds."))
  }
  if(is.null(cluster_var)){
    idvec <- 1:nrow(df)
    ids <- 1:nrow(df)
  } else {
    idvec <- df[[cluster_var]]
    ids <- unique(df[[cluster_var]])
  }
  n = length(ids)

  for (try in seq_len(max_tries)) {
    # Balanced random fold assignment
    fold_id <- sample(rep(seq_len(k), length.out = n))

    ok <- TRUE

    for (fold in seq_len(k)) {
      fold_rownums <- which(idvec %in% ids[fold_id == fold])

      fold_data <- df_pred[fold_rownums, , drop = FALSE]

      # Each factor must have both levels in this fold
      has_both_levels <- vapply(fold_data, function(x) {
        all(levels(x) %in% x)
      }, logical(1))

      if (!all(has_both_levels)) {
        ok <- FALSE
        break
      }
    }

    if (ok) {
      return(fold_id)
    }
  }

  stop("Could not find a valid split. Try increasing max_tries or check level imbalance.")
}

make_balanced_folds2 <- function(df, predictors, k = 8, cluster_var = NULL, max_tries = 100) {
  # Check that each level occurs at least k times
  df_pred <- df[, predictors]
  level_counts <- lapply(df_pred, table)

  too_rare <- lapply(level_counts, function(tab) tab < k)

  if (any(unlist(too_rare))) {
    stop(paste0("Each factor level must occur at least k times to appear in every fold. You can make at most ", min(unlist(level_counts)), " folds."))
  }
  if(is.null(cluster_var)){
    idvec <- 1:nrow(df)
    ids <- 1:nrow(df)
  } else {
    idvec <- df[[cluster_var]]
    ids <- unique(df[[cluster_var]])
  }
  n = length(ids)

  for (try in seq_len(max_tries)) {
    # Balanced random fold assignment
    fold_id <- sample(rep(seq_len(k), length.out = n))

    ok <- TRUE
    errorvars <- vector("character")
    for (fold in seq_len(k)) {
      fold_rownums <- which(idvec %in% ids[fold_id == fold])

      df_infold <- df_pred[fold_rownums, predictors, drop = FALSE]
      df_notinfold <- df_pred[-fold_rownums, predictors, drop = FALSE]

      # Each factor must have both levels in this fold
      has_both_levels <- vapply(names(df_infold), function(n) {
        suppressWarnings(tryCatch(all(levels(droplevels(df_infold[[n]])) == levels(droplevels(df_notinfold[[n]]))), error = function(e)FALSE))
        }, logical(1))

      if (!all(has_both_levels)) {
        errorvars <- c(errorvars, names(has_both_levels[!has_both_levels]))
        ok <- FALSE
        break
      }
    }

    if (ok) {
      return(fold_id)
    }
  }
  return(errorvars)
}



make_bootstrap_split <- function(df, predictors, p = .5, cluster_var = NULL, num_samples = 10, max_tries = 5000) {
  # Check that each level occurs at least k times
  df_pred <- df[, predictors]

  if(is.null(cluster_var)){
    idvec <- 1:nrow(df)
    ids <- 1:nrow(df)
  } else {
    idvec <- df[[cluster_var]]
    ids <- unique(df[[cluster_var]])
  }
  n = length(ids)
  results <- replicate(num_samples, {
    for (try in seq_len(max_tries)) {
      fold_id <- sample(ids, size = round(p*n))
      ok <- TRUE
      #for (fold in seq_len(k)) {
      fold_rownums <- which(idvec %in% fold_id)

      df_infold <- df_pred[fold_rownums, predictors, drop = FALSE]
      df_notinfold <- df_pred[-fold_rownums, predictors, drop = FALSE]

      # Each factor must have both levels in this fold
      has_both_levels <- vapply(names(df_infold), function(n) {
        suppressWarnings(tryCatch(all(levels(droplevels(df_infold[[n]])) == levels(droplevels(df_notinfold[[n]]))), error = function(e)FALSE))
      }, logical(1))

      if (!all(has_both_levels)) {
        break
      }
      return(fold_id)
    }
    return(NULL)
  })
  results <- results[!sapply(results, is.null)][1:10]
  return(results)
}
