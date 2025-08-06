suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(baguette))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9011")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
skip_if_not_installed("workflowsets", minimum_version = "1.0.1.9001")

test_that("resampling survival models with static metric", {
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  spec_bt <-
    bag_tree() %>%
    set_mode("censored regression")

  spec_ph <-
    proportional_hazards(penalty = .5) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  # putting together a workflow set --------------------------------------------

  expect_silent(
    wflow_set <-
      workflow_set(
        preproc = list(form = event_time ~ X1 + X2),
        models = list(bt = spec_bt, ph = spec_ph)
      )
  )

  expect_s3_class(wflow_set, "workflow_set")

  # resampling models with static metrics --------------------------------------

  stc_mtrc  <- metric_set(concordance_survival)
  ctrl <- control_resamples(save_pred = TRUE)

  expect_silent(
    wflow_set_fit_stc <-
      workflow_map(
        wflow_set,
        "fit_resamples",
        seed = 2193,
        resamples = sim_rs,
        metrics = stc_mtrc,
        control = ctrl
      )
  )

  set.seed(2193)
  spec_bt_rs <-
    fit_resamples(
      spec_bt,
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = stc_mtrc,
      control = ctrl
    )

  expect_equal(
    extract_workflow_set_result(wflow_set_fit_stc, "form_bt"),
    spec_bt_rs
  )

  # passing in eval time when not required -------------------------------------

  expect_snapshot({
    wflow_set_fit_stc_eval <-
      workflow_map(
        wflow_set,
        "fit_resamples",
        seed = 2193,
        resamples = sim_rs,
        metrics = stc_mtrc,
        eval_time = time_points,
        control = ctrl
      )
  })

  suppressWarnings({
    set.seed(2193)
    spec_bt_rs_eval <-
      fit_resamples(
        spec_bt,
        event_time ~ X1 + X2,
        resamples = sim_rs,
        metrics = stc_mtrc,
        eval_time = time_points,
        control = ctrl
      )
  })

  expect_equal(
    extract_workflow_set_result(wflow_set_fit_stc_eval, "form_bt"),
    spec_bt_rs_eval
  )

  # test metric collection -----------------------------------------------------

  wflow_set_mtrcs <- collect_metrics(wflow_set_fit_stc)

  expect_false(".eval_time" %in% names(wflow_set_mtrcs))
  expect_equal(nrow(wflow_set_mtrcs), 2)
  expect_true(all(wflow_set_mtrcs$.metric == "concordance_survival"))

  exp_metric_sum <-
    tibble(
      wflow_id = character(0),
      .config = character(0),
      preproc = character(0),
      model = character(0),
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0)
    )

  expect_ptype(wflow_set_mtrcs, exp_metric_sum)

  # test prediction collection -------------------------------------------------

  wflow_set_preds <- collect_predictions(wflow_set_fit_stc)

  expect_equal(nrow(wflow_set_preds), 750)

  static_ptype <- tibble::tibble(
    wflow_id = character(0),
    .config = character(0),
    preproc = character(0),
    model = character(0),
    .pred_time = numeric(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE]
  )

  expect_ptype(wflow_set_preds, static_ptype)
})

test_that("resampling survival models with integrated metric", {
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  spec_bt <-
    bag_tree() %>%
    set_mode("censored regression")

  spec_ph <-
    proportional_hazards(penalty = .5) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  # putting together a workflow set --------------------------------------------

  expect_silent(
    wflow_set <-
      workflow_set(
        preproc = list(form = event_time ~ X1 + X2),
        models = list(bt = spec_bt, ph = spec_ph)
      )
  )

  expect_s3_class(wflow_set, "workflow_set")

  # resampling models with integrated metrics ----------------------------------

  int_mtrc  <- metric_set(brier_survival_integrated)
  ctrl <- control_resamples(save_pred = TRUE)

  expect_silent(
    wflow_set_fit_stc <-
      workflow_map(
        wflow_set,
        "fit_resamples",
        seed = 2193,
        resamples = sim_rs,
        metrics = int_mtrc,
        eval_time = time_points,
        control = ctrl
      )
  )

  set.seed(2193)
  spec_bt_rs <-
    fit_resamples(
      spec_bt,
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = int_mtrc,
      eval_time = time_points,
      control = ctrl
    )

  expect_equal(
    extract_workflow_set_result(wflow_set_fit_stc, "form_bt"),
    spec_bt_rs
  )

  # test metric collection -----------------------------------------------------

  wflow_set_mtrcs <- collect_metrics(wflow_set_fit_stc)

  expect_false(".eval_time" %in% names(wflow_set_mtrcs))
  expect_equal(nrow(wflow_set_mtrcs), 2)
  expect_true(all(wflow_set_mtrcs$.metric == "brier_survival_integrated"))

  exp_metric_sum <-
    tibble(
      wflow_id = character(0),
      .config = character(0),
      preproc = character(0),
      model = character(0),
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0)
    )

  expect_ptype(wflow_set_mtrcs, exp_metric_sum)

  # test prediction collection -------------------------------------------------

  wflow_set_preds <- collect_predictions(wflow_set_fit_stc)

  expect_equal(nrow(wflow_set_preds), 750)

  static_ptype <- tibble::tibble(
    wflow_id = character(0),
    .config = character(0),
    preproc = character(0),
    model = character(0),
    .pred = list(),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE]
  )

  expect_ptype(wflow_set_preds, static_ptype)
})

test_that("resampling survival models with dynamic metric", {
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  spec_bt <-
    bag_tree() %>%
    set_mode("censored regression")

  spec_ph <-
    proportional_hazards(penalty = .5) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  # putting together a workflow set --------------------------------------------

  expect_silent(
    wflow_set <-
      workflow_set(
        preproc = list(form = event_time ~ X1 + X2),
        models = list(bt = spec_bt, ph = spec_ph)
      )
  )

  expect_s3_class(wflow_set, "workflow_set")

  # resampling models with dynamic metrics ----------------------------------

  dyn_mtrc  <- metric_set(brier_survival)
  ctrl <- control_resamples(save_pred = TRUE)

  expect_silent(
    wflow_set_fit_stc <-
      workflow_map(
        wflow_set,
        "fit_resamples",
        seed = 2193,
        resamples = sim_rs,
        metrics = dyn_mtrc,
        eval_time = time_points,
        control = ctrl
      )
  )

  set.seed(2193)
  spec_bt_rs <-
    fit_resamples(
      spec_bt,
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = ctrl
    )

  expect_equal(
    extract_workflow_set_result(wflow_set_fit_stc, "form_bt"),
    spec_bt_rs
  )

  # test metric collection -----------------------------------------------------

  wflow_set_mtrcs <- collect_metrics(wflow_set_fit_stc)

  expect_true(".eval_time" %in% names(wflow_set_mtrcs))
  expect_equal(nrow(wflow_set_mtrcs), 2 * length(time_points))
  expect_true(all(wflow_set_mtrcs$.metric == "brier_survival"))

  exp_metric_sum <-
    tibble(
      wflow_id = character(0),
      .config = character(0),
      preproc = character(0),
      model = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0)
    )

  expect_ptype(wflow_set_mtrcs, exp_metric_sum)

  # test prediction collection -------------------------------------------------

  wflow_set_preds <- collect_predictions(wflow_set_fit_stc)

  expect_equal(nrow(wflow_set_preds), 750)

  static_ptype <- tibble::tibble(
    wflow_id = character(0),
    .config = character(0),
    preproc = character(0),
    model = character(0),
    .pred = list(),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE]
  )

  expect_ptype(wflow_set_preds, static_ptype)
})

test_that("resampling survival models with mixture of metric types", {
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  spec_bt <-
    bag_tree() %>%
    set_mode("censored regression")

  spec_ph <-
    proportional_hazards(penalty = .5) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  # putting together a workflow set --------------------------------------------

  expect_silent(
    wflow_set <-
      workflow_set(
        preproc = list(form = event_time ~ X1 + X2),
        models = list(bt = spec_bt, ph = spec_ph)
      )
  )

  expect_s3_class(wflow_set, "workflow_set")

  # resampling models with dynamic metrics ----------------------------------

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)
  ctrl <- control_resamples(save_pred = TRUE)

  expect_silent(
    wflow_set_fit_stc <-
      workflow_map(
        wflow_set,
        "fit_resamples",
        seed = 2193,
        resamples = sim_rs,
        metrics = mix_mtrc,
        eval_time = time_points,
        control = ctrl
      )
  )

  set.seed(2193)
  spec_bt_rs <-
    fit_resamples(
      spec_bt,
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = ctrl
    )

  expect_equal(
    extract_workflow_set_result(wflow_set_fit_stc, "form_bt"),
    spec_bt_rs
  )

  # test metric collection -----------------------------------------------------

  wflow_set_mtrcs <- collect_metrics(wflow_set_fit_stc)

  expect_true(".eval_time" %in% names(wflow_set_mtrcs))
  expect_equal(nrow(wflow_set_mtrcs), 12)

  exp_metric_sum <-
    tibble(
      wflow_id = character(0),
      .config = character(0),
      preproc = character(0),
      model = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0)
    )

  expect_ptype(wflow_set_mtrcs, exp_metric_sum)

  # test prediction collection -------------------------------------------------

  wflow_set_preds <- collect_predictions(wflow_set_fit_stc)

  expect_equal(nrow(wflow_set_preds), 750)

  static_ptype <- tibble::tibble(
    wflow_id = character(0),
    .config = character(0),
    preproc = character(0),
    model = character(0),
    .pred = list(),
    .pred_time = numeric(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE]
  )

  expect_ptype(wflow_set_preds, static_ptype)
})
