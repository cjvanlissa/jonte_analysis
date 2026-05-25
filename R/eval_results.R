rsq_numeric <- function(obs, preds, mn){
  tss <- sum((obs-mn)^2)
  rss <- sum((preds - obs) ^ 2)
  return(1 - rss/tss)
}

eval_results_single <- function(dat, models){

models <- list(brma = res_brma, mf = res_metaforest, mlrf = res_mlrf)
  rsqs_test <- list(
    brma = pema:::predict.brma(models$brma, newdata = rbind(dat$train, dat$test), type = c("mean"))[-c(1:nrow(dat$train))],
    mf = metaforest:::predict.MetaForest(models$mf, data = rbind(dat$train, dat$test))$predictions[-c(1:nrow(dat$train))],
    mlrf = predict.mlrf(models$mlrf, newdata = df)[which(df$id_exp %in% df_split$test_id)]
  )
  rsqs_test <- sapply(models, `[[`, "rsq")

  df_rsq <- data.frame(
    mse = mses,
    mse_se = mse_sds,
    rsq_test = rsqs_test,
    rsq_train = rsqs_train,
    model = names(rsqs_test))

  # Choose best model
  #rsqs <- unlist(lapply(do.call(c, models), `[[`, "rsq"))
  best_model <- df_rsq$model[which.max(df_rsq$rsq_test)]

  model = models[[best_model]]$res

  return(
    list(
      rsqs = df_rsq,
      best = best_model,
      model = model
      # , interpret = interpret
    )
  )
}

eval_results <- function(dat, models){

  mses <- sapply(models, function(x){
    if("cvm" %in% names(x$mse_cv)){
      return(x$mse_cv["cvm"])
    } else {
      return(mean(x$mse_cv))
    }
  })
  mse_sds <- sapply(models, function(x){
    if("cvsd" %in% names(x$mse_cv)){
      return(x$mse_cv["cvsd"])
    } else {
      return(sd(x$mse_cv))
    }
  })
  rsqs_train <- sapply(models, `[[`, "rsq_train")
  # On test data
  rsqs_test <- sapply(models, `[[`, "rsq")

  df_rsq <- data.frame(
    mse = mses,
    mse_se = mse_sds,
    rsq_test = rsqs_test,
    rsq_train = rsqs_train,
    model = names(rsqs_test))

  # Choose best model
  #rsqs <- unlist(lapply(do.call(c, models), `[[`, "rsq"))
  best_model <- df_rsq$model[which.max(df_rsq$rsq_test)]

  model = models[[best_model]]$res

  return(
    list(
      rsqs = df_rsq,
      best = best_model,
      model = model
      # , interpret = interpret
    )
  )
}

interpret_model_metaforest <- function(res_metaforest, dat){

  # shaps <- c("corruption" = "n", "power_distance" = "p", "GDP" = "n", "PISA.Reading" = "n", "pandemic_response" = "n",
  #            "individualism" = "n", "inequality" = "p", "PISA.Science" = "n", "PISA.Math" = "n", "indulgence" = "o",
  #            "political_stability" = 'n', "human_development_index" = "n", "university_graduates" = "n",
  #            "WEIRDness" = "o", "uncertainty_avoidance" = "o", "longterm_orientation" = "o",
  #            "masculinity" = "o", "hospitalbeds_per_1000_people" = "n", "dataset" = 'o')
  var_importance <- sort(res_metaforest$res$variable.importance, decreasing = FALSE)
  var_importance <- data.frame(Variable = names(var_importance),
                               importance = unname(var_importance))
  var_importance$Variable <- ordered(var_importance$Variable, levels = var_importance$Variable)
  #var_importance$Shape <- ordered(shaps[as.character(var_importance$Variable)], levels = c("p", 'n', 'o'), labels = c("Positive", "Negative", "Other"))
  library(ggplot2)
  library(svglite)
  vim_plot <- ggplot(var_importance, aes(y = Variable, x = importance
                                         #, shape = Shape
                                         )) +
    geom_segment(aes(x = 0, xend = importance,
                     y = Variable, yend = Variable), colour = "grey50", linetype = 2) +
    geom_vline(xintercept = 0, colour = "grey50", linetype = 1) +
    geom_point(size = 2) + xlab("Variable Importance (Permutation importance)") +
    theme_bw() + theme(panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())

  ggsave("vim_plot.svg", vim_plot, device = "svg")
  set.seed(79974)
  library(metaforest)
  pd_plot <- metaforest::PartialDependence(res_metaforest$res, data = dat$train,
                                           vars = rev(tail(levels(var_importance$Variable), 30)), pi = .95, rawdata = FALSE)
  ggsave("pd_plot.svg", pd_plot, device = "svg", width = 11)
  return(list(vimp = "vim_plot.svg", pd = "pd_plot.svg"))
}


interpret_model_lasso <- function(res_lasso){
  imp <- coef(res_lasso)
  nms <- rownames(imp)
  imp <- abs(as.numeric(imp))
  names(imp) <- nms
  var_importance <- sort(imp[-1], decreasing = TRUE)

  var_importance <- data.frame(Variable = names(var_importance),
                               importance = unname(var_importance))
  var_importance$Variable <- ordered(var_importance$Variable, levels = rev(var_importance$Variable))

  library(ggplot2)
  library(svglite)
  vim_plot <- ggplot(var_importance, aes(y = Variable, x = importance
                                         #, shape = Shape
  )) +
    geom_segment(aes(x = 0, xend = importance,
                     y = Variable, yend = Variable), colour = "grey50", linetype = 2) +
    geom_vline(xintercept = 0, colour = "grey50", linetype = 1) +
    geom_point(size = 2) + xlab("Variable Importance") +
    theme_bw() + theme(panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())

  ggsave("vim_plot.svg", vim_plot, device = "svg")

  groupthese <- c(relationtype = "bivarRelIndivDimension_", GLOBE = "GLOBE_", HOF = "HOF_", M49 = "M49_subregion", rs = "RS_", SVS = "SVS_", taras = "taras")

  df_plot <- var_importance[rowSums(sapply(paste0("^", groupthese), grepl, x = var_importance$Variable)) == 0, ]

  df_plot <- rbind(df_plot,
                   do.call(rbind,
                           lapply(names(groupthese), function(n){
                              data.frame(Variable = n,
                                         importance = sum(var_importance$importance[grepl(paste0("^", groupthese[n]), x = var_importance$Variable)]))
  })))

  df_plot$Variable <- ordered(df_plot$Variable, levels = df_plot$Variable[order(df_plot$importance)])

  vim_plot <- ggplot(df_plot, aes(y = Variable, x = importance
                                         #, shape = Shape
  )) +
    geom_segment(aes(x = 0, xend = importance,
                     y = Variable, yend = Variable), colour = "grey50", linetype = 2) +
    geom_vline(xintercept = 0, colour = "grey50", linetype = 1) +
    geom_point(size = 2) + xlab("Log Variable Importance") +
    theme_bw() + theme(panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())+
    scale_x_log10()

  ggsave("vim_plot_grouped.svg", vim_plot, device = "svg")

  return(c(vimp = "vim_plot.svg", vimp_grouped = "vim_plot_grouped.svg"))
}
