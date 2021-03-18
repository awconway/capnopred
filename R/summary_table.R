#' Create a summary table of participant characteristics
#' @importFrom gtsummary tbl_summary
#' @importFrom dplyr select group_by filter
#' @param combined dataframe used for modelling
#' @export 
#' 
create_summary_table <- function(combined) {
    combined %>%
    group_by(ID) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(Age, "Body mass index" = BMI,
    Sex,
    "Obstructive sleep apnea" = OSA,
    "American Society of Anesthesiology Physical Classification Status" = ASA) %>%
    tbl_summary()
}