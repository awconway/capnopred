#' Create folds for cross validation
#' @param training data frame for training model
#' @param k number of folds
#' @param seed seed for random sample
#' @importFrom dplyr inner_join

#' @export

create_custom_folds <- function(x, k, seed) {
  cli::cli_alert("Creating CV folds...")

  set.seed(seed)
  ID <- unique(x$ID)
  fold <- sample(1:k, size = length(ID), replace = TRUE)

  result <- x %>%
  inner_join(data.frame(ID, fold), by = "ID")

  return(result)
}