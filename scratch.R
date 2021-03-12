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

data <- testing %>%
 mutate(outcome = ifelse(long == "long", 1, 0))

 data$rf <- rf_test$.pred_long

test <- dca(data = as.data.frame(data),
        outcome = "outcome",
        predictors = "Random forest",
        smooth = TRUE,
        xstop = 0.8
      )

test$net.benefit %>%
select(-rf_sm,
"Threshold" = threshold,
"All" = all) %>%
pivot_longer(cols = -Threshold, names_to = "Decisions", values_to = "Net benefit") %>%
ggplot(aes(x = Threshold, y= `Net benefit`)) +
geom_line(aes(color = Decisions)) +
coord_cartesian( ylim = c(-0.1, 0.5))



rf_test %>%
    mutate(pred = ifelse(.pred_long > 0.6, "long", "short")) %>%
        mutate(total_pos = ifelse(pred == "long", 1, 0)) %>%
    mutate(tp = ifelse(pred == "long" & long == "long", 1, 0)) %>%
    mutate(fp = ifelse(pred == "long" & long == "short", 1, 0)) %>%
    summarize(tpos = sum(tp)/nrow(rf_test),
    fpos = sum(fp)/nrow(rf_test),
    nb = tpos - fpos * (0.6/0.4),
    tp = sum(tp),
    fp = sum(fp),
    total_pos = sum(total_pos),
    afrr_cons = sum(total_pos)/tp,
    afrr_aggr = 1 - (sum(total_pos)/nrow(rf_test)))



    ppv(estimate = factor(pred), truth = long) %>%
    pull(.estimate)


