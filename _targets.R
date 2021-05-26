library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c(
  "tidymodels", "tidyverse",
  "patchwork", "capnopred"
))
list(
  # Load in patient data and respiratory state data
  # and reformat into dataframe
  tar_target(resp_data, format_raw()),

  tar_target(resp_dur, as.seq(resp_data) %>%
    state.dur() %>%
    ungroup()),

  tar_target(combined, format_data(resp_data,
    resp_dur,
    lower_trigger = 15,
    upper_trigger = 30
  )),

  tar_target(
    summary_table,
    create_summary_table(combined)
  ),

  tar_target(data_split, combined %>%
    make_train_id(
      seed = 42,
      frac = 0.75 
    )),

  tar_target(
    init_split,
    custom_rsplit(
      data_split,
      which(data_split$train == 1),
      which(data_split$train == 0)
    )
  ),

  tar_target(training, training(init_split)),
  tar_target(testing, testing(init_split)),

  tar_target(folds_index, create_custom_folds(data_split,
    k = 10,
    seed = 42
  )),
  tar_target(folds, group_vfold_cv(folds_index,
    group = fold,
    v = 10
  )),
  # workflow

  tar_target(base_rec, recipe(long ~ .,
    data = training
  ) %>%
    update_role(ID, new_role = "id variable") %>%
    update_role(train, new_role = "splitting indicator") %>%
    step_normalize(
      all_numeric(),
      -all_outcomes(),
    ) %>%
    step_interact(terms = ~ last_state_dur:num_apnea) %>%
    # step_interact(terms = ~ time_since_apneic:num_apnea) %>%
    step_dummy(
      all_nominal(),
      -has_role("id variable"),
      -all_outcomes()
    )),

  tar_target(wf, workflow() %>%
    add_recipe(base_rec)),

  # tar_target(class_metrics, metric_set(ppv, sensitivity, specificity)),

  # random forest model
  tar_target(rf, rand_forest(trees = 100) %>%
    set_engine("randomForest") %>%
    set_mode("classification")),
  tar_target(
    rf_fit,
    wf %>%
      add_model(rf) %>%
      fit_resamples(folds,
        # metrics = class_metrics,
        control = control_resamples(save_pred = TRUE)
      )
  ),
  tar_target(rf_metrics, collect_metrics(rf_fit)),
  tar_target(rf_preds, rf_fit %>%
    collect_predictions()),



  # Only when optimized feature selection and tuning
  # if using train/test split
  # tar_target(rf_test, last_fit(
  #   wf %>%
  #     add_model(rf),
  #   init_split
  # ) %>%
  #   collect_predictions()),

  tar_target(combined_preds, combine_preds(
    rf_preds,
    boost_preds,
    glm_preds,
    lasso_preds,
    ridge_preds
  )),

  tar_target(
    tpp,
    runway::threshperf_plot_multi(
      combined_preds,
      outcome = "outcome",
      prediction = ".pred_long",
      model = "model_name"
    ),
  ),

  tar_target(
    cal_plot,
    runway::cal_plot_multi(combined_preds,
      outcome = "outcome",
      prediction = ".pred_long",
      show_loess = TRUE,
      n_bins = 0,
      model = "model_name"
    )
  ),
  tar_target(
    roc_plot,
    runway::roc_plot_multi(combined_preds,
      outcome = "outcome",
      prediction = ".pred_long",
      ci = FALSE,
      model = "model_name"
    )
  ),

  # xgboost model
  tar_target(boost, boost_tree() %>%
    set_mode("classification") %>%
    set_engine("xgboost")),

  tar_target(
    boost_fit,
    wf %>%
      add_model(boost) %>%
      fit_resamples(folds,
        # metrics = class_metrics,
        control = control_resamples(save_pred = TRUE) # add this line
      )
  ),
  tar_target(boost_metrics, collect_metrics(boost_fit)),
  tar_target(boost_preds, boost_fit %>%
    collect_predictions()),


  tar_target(boost_ppv, boost_preds %>%
    mutate(pred = ifelse(.pred_long > 0.7, "long", "short")) %>%
    ppv(estimate = factor(pred), truth = long) %>%
    pull(.estimate)),

  # linear model
  tar_target(glm, logistic_reg(penalty = 0.001, mixture = 0.5) %>%
    set_engine("glmnet") %>%
    set_mode("classification")),
  tar_target(
    glm_fit, wf %>%
      add_model(glm) %>%
      fit_resamples(folds,
        # metrics = class_metrics,
        control = control_resamples(save_pred = TRUE) # add this line
      )
  ),
  tar_target(glm_metrics, collect_metrics(glm_fit)),
  tar_target(glm_preds, glm_fit %>%
    collect_predictions()),

  # lasso model
  tar_target(lasso, logistic_reg(penalty = 0.001, mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("classification")),
  tar_target(
    lasso_fit, wf %>%
      add_model(lasso) %>%
      fit_resamples(folds,
        # metrics = class_metrics,
        control = control_resamples(save_pred = TRUE) # add this line
      )
  ),
  tar_target(lasso_metrics, collect_metrics(lasso_fit)),
  tar_target(lasso_preds, lasso_fit %>%
    collect_predictions()),
  # ridge model
  tar_target(ridge, logistic_reg(penalty = 0.001, mixture = 0) %>%
    set_engine("glmnet") %>%
    set_mode("classification")),
  tar_target(
    ridge_fit, wf %>%
      add_model(ridge) %>%
      fit_resamples(folds,
        # metrics = class_metrics,
        control = control_resamples(save_pred = TRUE) # add this line
      )
  ),
  tar_target(ridge_metrics, collect_metrics(ridge_fit)),
  tar_target(ridge_preds, ridge_fit %>%
    collect_predictions()),

  # DCA

  tar_target(dca_data, rf_preds %>%
    mutate(
      outcome = ifelse(long == "long", 1, 0),
      `Random forest` = .pred_long
    )),

  tar_target(dca30, dca(
    data = as.data.frame(dca_data),
    outcome = "outcome",
    predictors = "Random forest",
    smooth = TRUE,
    xstart = 0.7,
    xstop = 0.8,
    graph = F,
  )),

  tar_target(dca_plot30, dca30$net.benefit %>%
    select(-`Random forest_sm`,
      "Threshold" = threshold,
      -all,
      "Alarm at 30 seconds" = none
    ) %>%
    pivot_longer(cols = -Threshold, names_to = "Decisions", values_to = "Net benefit") %>%
    ggplot(aes(x = Threshold, y = `Net benefit`)) +
    geom_line(aes(color = Decisions)) +
    # coord_cartesian(ylim = c(-0.05, 0.1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
      legend.title = element_blank())),

  tar_target(dca15, dca(
    data = as.data.frame(dca_data),
    outcome = "outcome",
    predictors = "Random forest",
    smooth = TRUE,
    xstart = 0.3,
    xstop = 0.5,
    graph = F,
  )),

  tar_target(dca_plot15, dca15$net.benefit %>%
    select(-`Random forest_sm`,
      "Threshold" = threshold,
      "Alarm at 15 seconds" = all,
      -none
    ) %>%
    pivot_longer(cols = -Threshold, names_to = "Decisions", values_to = "Net benefit") %>%
    ggplot(aes(x = Threshold, y = `Net benefit`)) +
    geom_line(aes(color = Decisions)) +
    coord_cartesian(ylim = c(0, 0.4)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )),

  tar_target(
    dca_plot_combined,
    ((dca_plot15 | dca_plot30) +
      plot_annotation(tag_levels = "A"))
  ),

  # manuscript
  tar_render(manuscript, "manuscript.Rmd")
)
