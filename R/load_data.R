load_data <- function(predictors){
  dat <- readxl::read_excel("Metadata_2026.xlsx", sheet = 1)
  dat$id_exp = factor(dat$id_exp)

  mat_preds <- dat[predictors]
  mat_preds <- lapply(mat_preds, factor)
  mat_preds <- model.matrix(~., mat_preds)[, -1]
  dat_rf <- data.frame(dat[, c("yi", "vi", "id_exp")], mat_preds)
  return(dat_rf)
}

