library(testthat)
library(tidymodels)
library(prodlim)
library(censored)
library(yardstick)
library(finetune)

skip_if_not_installed("finetune", minimum_version = "1.1.0.9001")

# ------------------------------------------------------------------------------

test_that("sim annealing tuning survival models with static metrics", {
  skip_if_not_installed("coin") # required for partykit engine
  skip_if_not_installed("prodlim")

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
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  init_grid_static_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = gctrl
    )

  set.seed(2193)
  sa_static_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = stc_mtrc,
      control = sctrl,
      initial = init_grid_static_res
    )

  expect_false(".eval_time" %in% names(sa_static_res$.metrics[[1]]))
  expect_equal(
    names(sa_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )

  expect_snapshot_plot(
    "static-metric-sa-search",
    print(autoplot(sa_static_res))
  )

  expect_snapshot_plot(
    "static-sa-search-with-two-time-points-marginals",
    print(autoplot(sa_static_res, type = "marginals"))
  )
  expect_snapshot_plot(
    "static-sa-search-with-two-time-points-performance",
    print(autoplot(sa_static_res, type = "performance"))
  )
})

test_that("sim annealing tuning survival models with integrated metrics", {
  skip_if_not_installed("coin") # required for partykit engine
  skip_if_not_installed("prodlim")

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
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)
  # standard setup end

  # TODO no eval_time column when you collect_metrics

  set.seed(2193)
  init_grid_integrated_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  sa_integrated_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = sctrl,
      initial = init_grid_integrated_res
    )

  expect_false(".eval_time" %in% names(sa_integrated_res$.metrics[[1]]))
  expect_equal(
    names(sa_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_true(is.list(sa_integrated_res$.predictions[[1]]$.pred))
  expect_equal(
    names(sa_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    sa_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # TODO this should throw a warning
  # expect_snapshot_plot(autoplot(sa_integrated_res, eval_time = c(1, 5)))
  expect_snapshot_plot(
    "integrated-metric-sa-search",
    print(autoplot(sa_integrated_res))
  )
  expect_snapshot_plot(
    "integrated-metric-sa-search-with-two-time-points-marginals",
    print(autoplot(sa_integrated_res, type = "marginals"))
  )
  expect_snapshot_plot(
    "integrated-metric-sa-search-with-two-time-points-performance",
    print(autoplot(sa_integrated_res, type = "performance"))
  )
})

test_that("sim annealing tuning survival models with dynamic metrics", {
  skip_if_not_installed("coin") # required for partykit engine
  skip_if_not_installed("prodlim")

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
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  init_grid_dynamic_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  sa_dynamic_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = sctrl,
      initial = init_grid_dynamic_res
    )

  expect_true(".eval_time" %in% names(sa_dynamic_res$.metrics[[1]]))
  expect_equal(
    names(sa_dynamic_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_true(is.list(sa_dynamic_res$.predictions[[1]]$.pred))
  expect_equal(
    names(sa_dynamic_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    sa_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      "dynamic-metric-sa-search",
      print(autoplot(sa_dynamic_res))
    )
  )
  expect_snapshot_plot(
    "dynamic-metric-sa-search-with-two-time-points-marginals",
    print(autoplot(sa_dynamic_res, eval_time = 1, type = "marginals"))
  )
  expect_snapshot_plot(
    "dynamic-metric-sa-search-with-two-time-points-performance",
    print(autoplot(sa_dynamic_res, eval_time = 1, type = "performance"))
  )
})

test_that("sim annealing tuning survival models with mixture of metric types", {
  skip_if_not_installed("coin") # required for partykit engine
  skip_if_not_installed("prodlim")

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
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  init_grid_mixed_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  sa_mixed_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = mix_mtrc,
      eval_time = time_points,
      initial = init_grid_mixed_res,
      control = sctrl
    )

  expect_true(".eval_time" %in% names(sa_mixed_res$.metrics[[1]]))
  expect_equal(
    names(sa_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", ".pred_time", "event_time", ".config")
  )
  expect_true(is.list(sa_mixed_res$.predictions[[1]]$.pred))
  expect_equal(
    names(sa_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    sa_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_plot(
    "mixed-metric-sa-search-with-two-time-points",
    print(autoplot(sa_mixed_res, eval_time = c(1, 5)))
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      "mixed-metric-sa-search-with-no-set-time-points",
      print(autoplot(sa_mixed_res))
    )
  )
  expect_snapshot_plot(
    "mixed-metric-sa-search-with-two-time-points-marginals",
    print(autoplot(sa_mixed_res, eval_time = 1, type = "marginals"))
  )
  expect_snapshot_plot(
    "mixed-metric-sa-search-with-two-time-points-performance",
    print(autoplot(sa_mixed_res, eval_time = 1, type = "performance"))
  )

  # test some S3 methods for any tune_result object
  expect_snapshot_warning(show_best(sa_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(sa_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot_error(
    show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1.001))
  )
  expect_snapshot_error(
    show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(sa_mixed_res, metric = "brier_survival_integrated")
  )
})
