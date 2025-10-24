# load libraries ----------------------------------------------------------
library(stacks)
library(tune)
library(finetune)
library(rsample)
library(parsnip)
library(workflows)
library(recipes)
library(yardstick)
library(workflowsets)
library(modeldata)

# data setup ------------------------------------------------------------
data(ames)
ames_narrow <- ames[1:1000, c(72, 35:45)]
ames_narrow$Sale_Price <- log(ames_narrow$Sale_Price)

set.seed(1)
ames_split <- rsample::initial_split(ames_narrow)

ames_train <- rsample::training(ames_split)

ames_test <- rsample::testing(ames_split)

n_res <- 5

folds <- rsample::vfold_cv(ames_train, v = n_res)

base_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = .1) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors())

metric <- yardstick::metric_set(yardstick::rmse)

# model definitions ----------------------------------------------------------
spec_lr <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

spec_bt <-
  boost_tree(mtry = tune(), min_n = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

spec_dt <-
  decision_tree(cost_complexity = tune(), tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

spec_svm <-
  svm_linear(cost = tune(), margin = tune()) %>%
  set_engine("LiblineaR") %>%
  set_mode("regression")

wf_set <-
  workflow_set(
    preproc = list(rec = base_rec),
    models = list(lr = spec_lr, bt = spec_bt, dt = spec_dt, svm = spec_svm),
    cross = TRUE
  )

# tests ------------------------------------------------------------------
test_that("stacking with grid search works", {
  skip_if(utils::packageVersion("stacks") < "1.0.0.9000")

  wf_set_grid <-
    workflow_map(
      wf_set %>% option_add(control = control_stack_grid()),
      fn = "tune_grid",
      seed = 1,
      resamples = folds,
      metrics = metric,
      grid = 3
    ) %>%
    suppressMessages()

  data_st_grid <-
    stacks() %>%
    add_candidates(wf_set_grid)

  expect_true(inherits(data_st_grid, "tbl_df"))

  model_st_grid <-
    data_st_grid %>%
    blend_predictions(times = 8, penalty = 5^c(-3:-1)) %>%
    fit_members()

  expect_true(inherits(model_st_grid, "model_stack"))

  betas_grid <-
    stacks:::.get_glmn_coefs(
      model_st_grid$coefs$fit,
      penalty = model_st_grid$penalty$penalty
    ) %>%
    rowwise() %>%
    dplyr::filter(terms != "(Intercept)" && estimate != 0) %>%
    ungroup()

  expect_true(nrow(betas_grid) == length(model_st_grid$member_fits))
  expect_true(all(betas_grid$terms %in% names(model_st_grid$member_fits)))

  preds_grid <-
    predict(model_st_grid, ames_test)

  expect_true(inherits(preds_grid, "tbl_df"))
})

test_that("stacking with Bayesian tuning works", {
  skip_if(utils::packageVersion("stacks") < "1.0.0.9000")

  wf_set_bayes <-
    workflow_map(
      wf_set %>% option_add(control = control_stack_bayes()),
      fn = "tune_bayes",
      seed = 1,
      resamples = folds,
      metrics = metric,
      iter = 3
    ) %>%
    suppressMessages()

  expect_snapshot_warning(
    data_st_bayes <-
      stacks() %>%
      add_candidates(wf_set_bayes) %>%
      suppressMessages()
  )

  expect_true(inherits(data_st_bayes, "tbl_df"))

  model_st_bayes <-
    data_st_bayes %>%
    blend_predictions(times = 8, penalty = 5^c(-3:-1)) %>%
    fit_members()

  expect_true(inherits(model_st_bayes, "model_stack"))

  betas_bayes <-
    stacks:::.get_glmn_coefs(
      model_st_bayes$coefs$fit,
      penalty = model_st_bayes$penalty$penalty
    ) %>%
    rowwise() %>%
    dplyr::filter(terms != "(Intercept)" && estimate != 0) %>%
    ungroup()

  expect_true(nrow(betas_bayes) == length(model_st_bayes$member_fits))
  expect_true(all(betas_bayes$terms %in% names(model_st_bayes$member_fits)))

  preds_bayes <-
    predict(model_st_bayes, ames_test)

  expect_true(inherits(preds_bayes, "tbl_df"))
})

test_that("stacking with finetune works (anova)", {
  skip_if(utils::packageVersion("stacks") < "1.0.0.9000")

  wf_set_anova <-
    workflow_map(
      wf_set %>%
        option_add(
          control = control_race(save_pred = TRUE, save_workflow = TRUE)
        ),
      fn = "tune_race_anova",
      seed = 1,
      resamples = folds,
      metrics = metric,
      # use higher grid value to ensure that some models are not resampled fully
      grid = 10
    )

  data_st_anova <-
    stacks() %>%
    add_candidates(wf_set_anova)

  expect_true(inherits(data_st_anova, "tbl_df"))

  # ensure that only candidates with complete resamples were kept
  raw_preds <- tune::collect_predictions(wf_set_anova, summarize = TRUE)

  model_st_anova <-
    data_st_anova %>%
    blend_predictions(times = 8, penalty = 5^c(-3:-1)) %>%
    fit_members()

  expect_true(inherits(model_st_anova, "model_stack"))

  betas_anova <-
    stacks:::.get_glmn_coefs(
      model_st_anova$coefs$fit,
      penalty = model_st_anova$penalty$penalty
    ) %>%
    rowwise() %>%
    dplyr::filter(terms != "(Intercept)" && estimate != 0) %>%
    ungroup()

  expect_true(nrow(betas_anova) == length(model_st_anova$member_fits))
  expect_true(all(betas_anova$terms %in% names(model_st_anova$member_fits)))

  preds_anova <-
    predict(model_st_anova, ames_test)

  expect_true(inherits(preds_anova, "tbl_df"))

  skip_if_not_installed("stacks", "1.1.1.9001")

  retain_configs <-
    tune::collect_metrics(wf_set_anova, summarize = FALSE) %>%
    dplyr::group_by(wflow_id, .config) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == n_res) %>%
    dplyr::select(wflow_id, .config) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .config = gsub("Preprocessor|Model|pre|mod|post", "", .config),
      col_name = paste(wflow_id, .config, sep = "_")
    ) %>%
    pull(col_name)

  expect_true(all(
    colnames(data_st_anova)[2:length(data_st_anova)] %in% retain_configs
  ))
})

test_that("stacking with finetune works (sim_anneal)", {
  skip_if(utils::packageVersion("stacks") < "1.0.0.9000")
  skip_if(utils::packageVersion("finetune") < "1.1.0.9001")

  wf_set_sim_anneal <-
    workflow_map(
      wf_set %>%
        option_add(
          control = control_sim_anneal(
            save_pred = TRUE,
            save_workflow = TRUE,
            verbose_iter = FALSE
          )
        ),
      fn = "tune_sim_anneal",
      seed = 1,
      resamples = folds,
      metrics = metric,
      iter = 3
    )

  wf_set_sim_anneal <- wf_set_sim_anneal[
    purrr::map(wf_set_sim_anneal$result, inherits, "tune_results") %>% unlist(),
  ]

  wf_set_preds <-
    wf_set_sim_anneal %>%
    collect_metrics(summarize = FALSE) %>%
    group_by(wflow_id, .config) %>%
    count()

  data_st_sim_anneal <-
    stacks() %>%
    add_candidates(wf_set_sim_anneal)

  expect_true(inherits(data_st_sim_anneal, "tbl_df"))

  model_st_sim_anneal <-
    data_st_sim_anneal %>%
    blend_predictions(times = 8, penalty = 5^c(-3:-1)) %>%
    fit_members()

  expect_true(inherits(model_st_sim_anneal, "model_stack"))

  betas_sim_anneal <-
    stacks:::.get_glmn_coefs(
      model_st_sim_anneal$coefs$fit,
      penalty = model_st_sim_anneal$penalty$penalty
    ) %>%
    rowwise() %>%
    dplyr::filter(terms != "(Intercept)" && estimate != 0) %>%
    ungroup()

  expect_true(nrow(betas_sim_anneal) == length(model_st_sim_anneal$member_fits))
  expect_true(all(
    betas_sim_anneal$terms %in% names(model_st_sim_anneal$member_fits)
  ))

  preds_sim_anneal <-
    predict(model_st_sim_anneal, ames_test)

  expect_true(inherits(preds_sim_anneal, "tbl_df"))
})

test_that("stacking with finetune works (win_loss)", {
  skip_if(utils::packageVersion("stacks") < "1.0.0.9000")

  wf_set_win_loss <-
    workflow_map(
      wf_set %>%
        option_add(
          control = control_race(save_pred = TRUE, save_workflow = TRUE)
        ),
      fn = "tune_race_win_loss",
      seed = 1,
      resamples = folds,
      metrics = metric,
      grid = 3
    ) %>%
    suppressMessages()

  data_st_win_loss <-
    stacks() %>%
    add_candidates(wf_set_win_loss)

  expect_true(inherits(data_st_win_loss, "tbl_df"))

  expect_true(all(
    colnames(data_st_win_loss)[2:length(data_st_win_loss)] %in%
      purrr::flatten_chr(attr(data_st_win_loss, "cols_map"))
  ))

  model_st_win_loss <-
    data_st_win_loss %>%
    blend_predictions(times = 8, penalty = 5^c(-3:-1)) %>%
    fit_members()

  expect_true(inherits(model_st_win_loss, "model_stack"))

  betas_win_loss <-
    stacks:::.get_glmn_coefs(
      model_st_win_loss$coefs$fit,
      penalty = model_st_win_loss$penalty$penalty
    ) %>%
    rowwise() %>%
    dplyr::filter(terms != "(Intercept)" && estimate != 0) %>%
    ungroup()

  expect_true(nrow(betas_win_loss) == length(model_st_win_loss$member_fits))
  expect_true(all(
    betas_win_loss$terms %in% names(model_st_win_loss$member_fits)
  ))

  preds_win_loss <-
    predict(model_st_win_loss, ames_test)

  expect_true(inherits(preds_win_loss, "tbl_df"))
})
