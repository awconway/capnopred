#' Adds a variable to allocate to training or testing

#' @importFrom dplyr left_join
#' @importFrom stats rbinom
#' @export
make_train_id <- function(x, seed, frac) {
  cli::cli_alert("Splitting data into training and test sets...")

  ID <- unique(x$ID)
  set.seed(seed)
  train <- stats::rbinom(n = length(ID), size = 1, prob = frac)

  tmp <- data.frame(ID, train)

  result <- dplyr::left_join(x, tmp, by = "ID")

  return(result)
}