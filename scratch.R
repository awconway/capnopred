patient$`OSA=1`
targets::tar_load(testing)
testing %>% filter(OSA == "OSA")
resp_data %>% filter(OSA==1)


resp_data %>%
        mutate(
            Midazolam = ifelse(is.na(Midazolam), 0, Midazolam),
            Fentanyl = ifelse(is.na(Fentanyl), 0, Fentanyl)
        ) %>% 
        select(-Smoke) %>%
        mutate(OSA = ifelse(OSA == 1, "OSA", "No OSA"),
        Sex = ifelse(Sex == 1, "Female", "Male")) %>% 
        filter(OSA=="OSA")

library(tidyverse)

rf_preds %>%
mutate(pred = ifelse(.pred_long > 0.75, "long", "short")) %>%
ppv(estimate = factor(pred), truth = long)



rf_preds %>%
mutate(pred = ifelse(.pred_long > 0.5, "long", "short")) %>%
filter(pred=="short" & long =="long")


 durations %>%
        group_by(ID) %>%
        mutate(last_state = lag(state)) %>%
        mutate(last_state_dur = lag(dur)) %>%
        filter(state == 4) %>%
        mutate(num_apnea = row_number()) %>%
        filter(dur >= lower_trigger,
        ID %in% resp_data$ID) %>% View()

