library(testthat)
library(tidymodels)
library(censored)
library(yardstick)
library(finetune)

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("grid tuning with static metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

  if (is_object_available(grid_static_res)) {
    grid_static_res <- return_object(grid_static_res)
  } else {
    stc_mtrc  <- metric_set(concordance_survival)

    # standard setup start
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
      decision_tree(tree_depth = tune(), min_n = 4) %>%
      set_engine("partykit") %>%
      set_mode("censored regression")

    grid <- tibble(tree_depth = c(1, 2, 10))

    gctrl <- control_grid(save_pred = TRUE)
    # standard setup end

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
    save_object(grid_static_res)
  }

  expect_s3_class(grid_static_res, "tune_results")
})


test_that("grid tuning with static metric - check structure", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

  is_object_available(grid_static_res, fail = TRUE)
  grid_static_res <- return_object(grid_static_res)

  expect_false(".eval_time" %in% names(grid_static_res$.metrics[[1]]))
  expect_equal(
    names(grid_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )
})

test_that("grid tuning with static metric - autoplot", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

  is_object_available(grid_static_res, fail = TRUE)
  grid_static_res <- return_object(grid_static_res)

  expect_snapshot_plot(
    print(autoplot(grid_static_res)),
    "static-metric-grid-search"
  )
})


test_that("grid tuning survival models with integrated metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

  sint_mtrc <- metric_set(brier_survival_integrated)

  # standard setup start
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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  # standard setup end

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
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # TODO this should throw a warning
  # expect_snapshot_plot(autoplot(grid_integrated_res, eval_time = c(1, 5)))
  expect_snapshot_plot(
    print(autoplot(grid_integrated_res)),
    "integrated-metric-grid-search"
  )
})

test_that("grid tuning survival models with dynamic metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

  dyn_mtrc  <- metric_set(brier_survival)

  # standard setup start
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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  # standard setup end

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
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(grid_dynamic_res)),
      "dynamic-metric-grid-search"
    )
  )
})

test_that("grid tuning survival models mixture of metric types", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  # standard setup start
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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  # standard setup end

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
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_plot(
    print(autoplot(grid_mixed_res, eval_time = c(1, 5))),
    "mixed-metric-grid-search-with-two-time-points"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(grid_mixed_res)),
      "mixed-metric-grid-search-with-no-set-time-points"
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
})
