#' Format data for outcome
#'
#' @description The predictors in this model will be total dose of
#' sedation, time that sedation was administered, duration and time
#'  since previous apneic event, and (non-missing) patient
#' demographics.
#'
#' @export
#'
#' @importFrom dplyr group_by mutate lag filter left_join select ungroup
#' @importFrom tidyr replace_na
format_data <- function(resp_data, durations,
                        lower_trigger,
                        upper_trigger) {
    formatted_data <- durations %>%
        group_by(ID) %>%
        mutate(num_states = row_number()) %>%
        mutate(last_state = lag(state)) %>%
        mutate(last_state_dur = lag(dur)) %>%
        filter(state == 4) %>%
        mutate(num_apnea = row_number()) %>%
        filter(
            dur >= lower_trigger,
            ID %in% resp_data$ID
        ) %>%
        mutate(
            last_apneic_dur = lag(dur),
            time_since_apneic = start - lag(start + dur)
        ) %>%
        mutate(
            long = ifelse(dur >= upper_trigger, "long", "short"),
            last_state = case_when(
                last_state == 1 ~ "Normal breathing",
                last_state == 2 ~ "Hypoventilation",
                last_state == 3 ~ "Bradypnea"
            )
        )

    dose_table <- resp_data %>%
        filter(!is.na(Midazolam) | !is.na(Fentanyl)) %>%
        group_by(ID) %>%
        mutate(dose_number = row_number())


    demographics <- resp_data %>%
        left_join(dose_table) %>%
        fill(dose_number) %>%
        group_by(ID, dose_number) %>%
        mutate(dose_start = first(Time)) %>%
        mutate(dose_time_diff = Time - dose_start) %>%
        ungroup() %>%
        mutate(
            Midazolam = ifelse(is.na(Midazolam), 0, Midazolam),
            Fentanyl = ifelse(is.na(Fentanyl), 0, Fentanyl)
        ) %>%
        select(-Smoke) %>%
        mutate(
            OSA = ifelse(OSA == 1, "OSA", "No OSA"),
            Sex = ifelse(Sex == 1, "Female", "Male")
        )

    demographics <- demographics %>%
        group_by(ID) %>%
        mutate(
            Midazolam = cumsum(Midazolam),
            Fentanyl = cumsum(Fentanyl)
        )

    formatted_data <- left_join(formatted_data,
        demographics %>%
            select(-state),
        by = c("ID" = "ID", "start" = "Time")
    ) %>%
        ungroup() %>%
        mutate(first_apnea = ifelse(is.na(last_apneic_dur),
            "first apnea",
            "not first apnea"
        )) %>%
        select(-state, -dur) %>%
        replace_na(replace = list(
            last_apneic_dur = 0,
            time_since_apneic = 0
        )) %>%
        na.omit() %>%
        mutate(ASA = case_when(
            ASA == 1 | ASA == 2 ~ "Low",
            TRUE ~ "High"
        )) %>%
        mutate(last_state = case_when(
            last_state == "Normal breathing" ~ "Normal breathing",
            TRUE ~ "Abnormal breathing"
        ))

    return(formatted_data)
}