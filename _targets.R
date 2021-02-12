library(targets)
library(tidymodels)
library(tidyverse)


devtools::load_all()



list(
  # Load in patient data and respiratory state data
  # and reformat into dataframe
  tar_target(resp_data, format_raw(
    patient = "data/myCapnoData.rds",
    capno_data = "data/capno.rds"
  )),

  tar_target(resp_dur, as.seq(resp_data) %>%
    state.dur() %>%
    ungroup()),

  tar_target(combined, format_data(resp_data,
    resp_dur,
    lower_trigger = 15,
    upper_trigger = 30
  )),

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

  tar_target(folds_index, create_custom_folds(training,
    k = 5,
    seed = 42
  )),
  tar_target(folds, group_vfold_cv(folds_index,
    group = fold,
    v = 5
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
    )
    ),

  tar_target(wf, workflow() %>%
    add_recipe(base_rec)),

  tar_target(class_metrics, metric_set(roc_auc, sens)),

  # random forest model
  tar_target(rf, rand_forest(trees = 50) %>%
    set_engine("randomForest") %>%
    set_mode("classification")),
  tar_target(
    rf_fit,
    wf %>%
      add_model(rf) %>%
      fit_resamples(folds,
        # metrics = class_metrics,
        control = control_resamples(save_pred = TRUE) # add this line
      )
  ),
  tar_target(rf_metrics, collect_metrics(rf_fit)),
  tar_target(rf_preds, rf_fit %>%
    collect_predictions()),

  tar_target(rf_ppv, rf_preds %>%
    mutate(pred = ifelse(.pred_long > 0.7, "long", "short")) %>%
    ppv(estimate = factor(pred), truth = long) %>%
    pull(.estimate)),

  # Only when optimized feature selection and tuning
  # tar_target(rf_test, last_fit(
  #   wf %>%
  #     add_model(rf),
  #   init_split
  # ) %>%
  #   collect_metrics()),

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
  tar_target(glm_metrics, collect_metrics(glm_fit))
)