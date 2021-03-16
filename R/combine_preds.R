#' Combine predictions from models into one dataframe
#' @importFrom dplyr mutate bind_rows
#' @export

combine_preds <- function(rf_preds,
                          boost_preds,
                          glm_preds,
                          lasso_preds,
                          ridge_preds) {
  rf_preds$model_name <- "Random forest"
  boost_preds$model_name <- "XGboost"
  glm_preds$model_name <- "Logistic regression"
  lasso_preds$model_name <- "Lasso regression"
  ridge_preds$model_name <- "Ridge regression"

  bind_rows(
    rf_preds,
    boost_preds,
    glm_preds,
    lasso_preds,
    ridge_preds
  ) %>%
    mutate(outcome = ifelse(long == "long", 1, 0))
}
