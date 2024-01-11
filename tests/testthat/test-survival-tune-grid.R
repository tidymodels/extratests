suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9011")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("grid tuning survival models with static metric", {
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

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # Grid search with static metrics --------------------------------------------

  stc_mtrc  <- metric_set(concordance_survival)

  set.seed(2193)
  grid_static_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = gctrl
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(grid_static_res$.metrics[[1]]))
  expect_equal(
    names(grid_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "penalty", "event_time", ".config")
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(grid_static_res)),
    "stc-grid"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(grid_static_res)
  exp_metric_sum <- tibble(
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_sum) == 3)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "concordance_survival"))

  metric_all <- collect_metrics(grid_static_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_all) == 30)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "concordance_survival"))

  # test prediction collection -------------------------------------------------

  static_ptype <- tibble::tibble(
    .pred_time = numeric(0),
    id = character(0),
    .row = integer(0),
    penalty = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  unsum_pred <- collect_predictions(grid_static_res)
  expect_equal(unsum_pred[0,], static_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  sum_pred <- collect_predictions(grid_static_res, summarize = TRUE)
  expect_equal(sum_pred[0,], static_ptype[, names(static_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

})

test_that("grid tuning survival models with integrated metric", {
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

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # Grid search with integrated metrics ----------------------------------------

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  grid_integrated_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(grid_integrated_res$.metrics[[1]]))
  expect_equal(
    names(grid_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "penalty", "event_time", ".config")
  )
  expect_true(is.list(grid_integrated_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(grid_integrated_res)),
    "int-grid"
  )

  ## test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(grid_integrated_res)
  exp_metric_sum <- tibble(
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_sum) == 3)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival_integrated"))

  metric_all <- collect_metrics(grid_integrated_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_all) == 30)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival_integrated"))

  # test prediction collection -------------------------------------------------

  integrated_ptype <- tibble::tibble(
    .pred = list(),
    id = character(0),
    .row = integer(0),
    penalty = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  integrated_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(grid_integrated_res)
  expect_equal(unsum_pred[0,], integrated_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(unsum_pred$.pred[[1]][0,], integrated_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(grid_integrated_res, summarize = TRUE)
  expect_equal(sum_pred[0,], integrated_ptype[, names(integrated_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(sum_pred$.pred[[1]][0,], integrated_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

})

test_that("grid tuning survival models with dynamic metric", {
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

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # Grid search with dynamic metrics -------------------------------------------

  dyn_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  grid_dynamic_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(grid_dynamic_res$.metrics[[1]]))
  expect_equal(
    names(grid_dynamic_res$.predictions[[1]]),
    c(".pred", ".row", "penalty", "event_time", ".config")
  )
  expect_true(is.list(grid_dynamic_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_dynamic_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(grid_dynamic_res)),
      "dyn-grid"
    )
  )

  expect_snapshot_plot(
    print(autoplot(grid_dynamic_res, eval_time = 1)),
    "dyn-grid-1"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(grid_dynamic_res)
  exp_metric_sum <- tibble(
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_sum) == 12)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival"))

  metric_all <- collect_metrics(grid_dynamic_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_all) == 120)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival"))

  # test prediction collection -------------------------------------------------

  dynamic_ptype <- tibble::tibble(
    .pred = list(),
    id = character(0),
    .row = integer(0),
    penalty = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  dynamic_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(grid_dynamic_res)
  expect_equal(unsum_pred[0,], dynamic_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(unsum_pred$.pred[[1]][0,], dynamic_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(grid_dynamic_res, summarize = TRUE)
  expect_equal(sum_pred[0,], dynamic_ptype[, names(dynamic_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(sum_pred$.pred[[1]][0,], dynamic_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

})

test_that("grid tuning survival models mixture of metric types", {
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

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # Grid search with a mixture of metrics --------------------------------------

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  set.seed(2193)
  grid_mixed_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(grid_mixed_res$.metrics[[1]]))
  expect_equal(
    names(grid_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "penalty", ".pred_time", "event_time", ".config")
  )
  expect_true(is.list(grid_mixed_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(grid_mixed_res, eval_time = c(1, 5))),
    "mix-grid-2-times"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(grid_mixed_res)),
      "mix-grid-0-times"
    )
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(grid_mixed_res)
  exp_metric_sum <- tibble(
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_sum) == 18)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_sum$.eval_time)) == 6)
  expect_equal(as.vector(table(metric_sum$.metric)), c(12L, 3L, 3L))

  metric_all <- collect_metrics(grid_mixed_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  expect_true(nrow(metric_all) == 180)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 60)
  expect_equal(as.vector(table(metric_all$.metric)), c(120L, 30L, 30L))

  # test prediction collection -------------------------------------------------

  mixed_ptype <- tibble::tibble(
    .pred = list(),
    .pred_time = numeric(0),
    id = character(0),
    .row = integer(0),
    penalty = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  mixed_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(grid_mixed_res)
  expect_equal(unsum_pred[0,], mixed_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(unsum_pred$.pred[[1]][0,], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(grid_mixed_res, summarize = TRUE)
  expect_equal(sum_pred[0,], mixed_ptype[, names(mixed_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(sum_pred$.pred[[1]][0,], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test show_best() -----------------------------------------------------------

  expect_snapshot_warning(show_best(grid_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(grid_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot(
    show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1.001)),
    error = TRUE
  )
  expect_snapshot_warning(
    show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(grid_mixed_res, metric = "brier_survival_integrated")
  )
})
