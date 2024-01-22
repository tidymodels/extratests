suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9012")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("select_*() with static metric", {
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
  sim_rs <- vfold_cv(sim_tr, repeats = 3)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^seq(-2, -1, length.out = 10))

  gctrl <- control_grid(save_pred = TRUE)

  ## Grid search with static metrics --------------------------------------------

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

  ## Test selecting functions --------------------------------------------------

  expect_snapshot(
    select_best(grid_static_res)
  )

  expect_snapshot(
    select_best(grid_static_res, metric = "concordance_survival")
  )

  expect_snapshot(
    select_best(grid_static_res, metric = "brier_survival_integrated"),
    error = TRUE
  )

  expect_snapshot(
    select_best(grid_static_res, metric = "concordance_survival", eval_time = 0)
  )

  expect_snapshot(
    select_by_one_std_err(grid_static_res, metric = "concordance_survival", penalty)
  )

  expect_snapshot(
    select_by_pct_loss(grid_static_res, metric = "concordance_survival", penalty)
  )
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
  sim_rs <- vfold_cv(sim_tr, repeats = 3)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^seq(-2, -1, length.out = 10))

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

  ## Test selecting functions --------------------------------------------------

  expect_snapshot(
    select_best(grid_integrated_res)
  )

  expect_snapshot(
    select_best(grid_integrated_res, metric = "brier_survival_integrated")
  )

  expect_snapshot(
    select_best(grid_integrated_res, metric = "brier_survival_integrated", eval_time = 0)
  )

  expect_snapshot(
    select_by_one_std_err(grid_integrated_res, metric = "brier_survival_integrated", penalty)
  )

  expect_snapshot(
    select_by_pct_loss(grid_integrated_res, metric = "brier_survival_integrated", penalty)
  )

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
  sim_rs <- vfold_cv(sim_tr, repeats = 3)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^seq(-2, -1, length.out = 10))

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

  ## Test selecting functions --------------------------------------------------

  expect_snapshot(
    select_best(grid_dynamic_res)
  )

  expect_snapshot(
    select_best(grid_dynamic_res, metric = "brier_survival", eval_time = 10)
  )

  expect_snapshot(
    select_best(grid_dynamic_res, metric = "brier_survival", eval_time = c(5, 10))
  )

  expect_snapshot(
    select_best(grid_dynamic_res, metric = "brier_survival_integrated"),
    error = TRUE
  )

  expect_snapshot(
    select_best(grid_dynamic_res, metric = "brier_survival", eval_time = 0),
    error = TRUE
  )

  expect_snapshot(
    select_by_one_std_err(grid_dynamic_res, metric = "brier_survival", penalty, eval_time = 10)
  )

  expect_snapshot(
    select_by_pct_loss(grid_dynamic_res, metric = "brier_survival", penalty, eval_time = 10)
  )

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
  sim_rs <- vfold_cv(sim_tr, repeats = 3)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^seq(-2, -1, length.out = 10))

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

  ## Test selecting functions --------------------------------------------------

  expect_snapshot(
    select_best(grid_mixed_res)
  )

  expect_snapshot(
    select_best(grid_mixed_res, metric = "brier_survival", eval_time = 10)
  )

  expect_snapshot(
    select_best(grid_mixed_res, metric = "brier_survival_integrated", eval_time = 0)
  )

  expect_snapshot(
    select_best(grid_mixed_res, metric = "brier_survival", eval_time = 0),
    error = TRUE
  )

  expect_snapshot(
    select_by_one_std_err(grid_mixed_res, metric = "brier_survival", penalty, eval_time = 10)
  )

  expect_snapshot(
    select_by_pct_loss(grid_mixed_res, metric = "brier_survival", penalty, eval_time = 10)
  )

})
