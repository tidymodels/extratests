suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9012")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("fit best with static metric", {
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

  gctrl <- control_grid(save_workflow = TRUE)

  # standard setup end -------------------------------------------------------

  stc_mtrc <- metric_set(concordance_survival)

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

  expect_silent(static_res <- fit_best(grid_static_res))
  expect_s3_class(static_res, "workflow")
  expect_true(is_trained_workflow(static_res))
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

  gctrl <- control_grid(save_workflow = TRUE)

  # standard setup end -------------------------------------------------------

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

  expect_silent(integrated_res <- fit_best(grid_integrated_res))
  expect_s3_class(integrated_res, "workflow")
  expect_true(is_trained_workflow(integrated_res))
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

  gctrl <- control_grid(save_workflow = TRUE)

  # standard setup end -------------------------------------------------------

  dyn_mtrc <- metric_set(brier_survival)

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

  expect_silent(dynamic_res <- fit_best(grid_dynamic_res))
  dynamic_res <- fit_best(grid_dynamic_res, eval_time = 1)
  expect_s3_class(dynamic_res, "workflow")
  expect_true(is_trained_workflow(dynamic_res))
})

test_that("fit best with linear_pred metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("yardstick", minimum_version = "1.3.2.9000")
  skip_if_not_installed("tune", minimum_version = "2.0.1.9001")

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

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_workflow = TRUE)

  # standard setup end -------------------------------------------------------

  linpred_mtrc <- metric_set(royston_survival)

  set.seed(2193)
  grid_linpred_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = linpred_mtrc,
      control = gctrl
    )

  expect_silent(linpred_res <- fit_best(grid_linpred_res))
  expect_s3_class(linpred_res, "workflow")
  expect_true(is_trained_workflow(linpred_res))
})

test_that("grid tuning survival models mixture of metric types", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("tune", minimum_version = "2.0.1.9001")
  skip_if_not_installed("yardstick", minimum_version = "1.3.2.9000")

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

  gctrl <- control_grid(save_workflow = TRUE)

  # standard setup end -------------------------------------------------------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival,
    royston_survival
  )

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

  expect_silent(mixed_res <- fit_best(grid_mixed_res))
  mixed_res <- fit_best(grid_mixed_res, eval_time = 1)
  expect_s3_class(mixed_res, "workflow")
  expect_true(is_trained_workflow(mixed_res))
})
