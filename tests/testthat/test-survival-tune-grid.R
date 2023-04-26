library(testthat)
library(tidymodels)
library(prodlim)
library(censored)
library(yardstick)
library(finetune)

# ------------------------------------------------------------------------------

test_that('grid tuning survival models ', {

  set.seed(1)
  sim_dat <- SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  dyn_mtrc  <- metric_set(brier_survival)
  stc_mtrc  <- metric_set(concordance_survival)
  sint_mtrc <- metric_set(brier_survival_integrated)
  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  mod_spec <-
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)

  # ------------------------------------------------------------------------------
  # Mixtures of static, dynamic, and integrated metrics

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

  expect_true(".eval_time" %in% names(grid_mixed_res$.metrics[[1]]))
  expect_equal(
    names(grid_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", ".pred_time", "event_time", ".config")
  )
  expect_true(is.list(grid_mixed_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored", ".weight_censored")
  )
  expect_equal(
    grid_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_plot(
    "mixed-metric-grid-search-with-two-time-points",
    print(autoplot(grid_mixed_res, eval_time = c(1, 5)))
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      "mixed-metric-grid-search-with-no-set-time-points",
      print(autoplot(grid_mixed_res))
    )
  )

  # test some S3 methods for any tune_result object
  expect_snapshot_warning(show_best(grid_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(grid_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot_error(
    show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1.001))
  )
  expect_snapshot_error(
    show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(grid_mixed_res, metric = "brier_survival_integrated")
  )

  # ------------------------------------------------------------------------------
  # Static metrics only

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

  expect_false(".eval_time" %in% names(grid_static_res$.metrics[[1]]))
  expect_equal(
    names(grid_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      "static-metric-grid-search",
      print(autoplot(grid_static_res))
    )
  )

  # ------------------------------------------------------------------------------
  # Integrated metrics only

  # TODO no eval_time column when you collect_metrics

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

  expect_false(".eval_time" %in% names(grid_integrated_res$.metrics[[1]]))
  expect_equal(
    names(grid_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_true(is.list(grid_integrated_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored", ".weight_censored")
  )
  expect_equal(
    grid_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # TODO this should throw a warning
  # expect_snapshot_plot(autoplot(grid_integrated_res, eval_time = c(1, 5)))
  expect_snapshot_plot(
    "integrated-metric-grid-search",
    print(autoplot(grid_integrated_res))
  )

  # ------------------------------------------------------------------------------
  # Dynamic metrics only

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

  expect_true(".eval_time" %in% names(grid_dynamic_res$.metrics[[1]]))
  expect_equal(
    names(grid_dynamic_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_true(is.list(grid_dynamic_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_dynamic_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored", ".weight_censored")
  )
  expect_equal(
    grid_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      "dynamic-metric-grid-search",
      print(autoplot(grid_dynamic_res))
    )
  )


})
