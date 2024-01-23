library(testthat)
library(tidymodels)
library(prodlim)
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9012")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("augmenting survival models", {

  # General setup --------------------------------------------------------------

  set.seed(1)
  sim_dat <- SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)

  time_points <- c(1, 5, 10)

  # survival_reg() -------------------------------------------------------------

  sr_fit <-
    survival_reg() %>%
    fit(event_time ~ ., data = sim_tr)

  sr_aug <- augment(sr_fit, new_data = sim_tr, eval_time = time_points)
  expect_equal(nrow(sr_aug), nrow(sim_tr))
  expect_equal(names(sr_aug), c(".pred", ".pred_time", "event_time", "X1", "X2"))
  expect_true(is.list(sr_aug$.pred))
  expect_equal(
    names(sr_aug$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored",
      ".weight_censored")
  )

  # proportional_hazards() -----------------------------------------------------

  glmn_fit <-
    proportional_hazards(penalty = 0.1) %>%
    set_engine("glmnet") %>%
    fit(event_time ~ ., data = sim_tr)

  glmn_aug <- augment(glmn_fit, new_data = sim_tr, eval_time = time_points)
  expect_equal(nrow(glmn_aug), nrow(sim_tr))
  expect_equal(names(glmn_aug), c(".pred", ".pred_time", "event_time", "X1", "X2"))
  expect_true(is.list(glmn_aug$.pred))
  expect_equal(
    names(glmn_aug$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored",
      ".weight_censored")
  )
})

test_that("augment() works for tune_results", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("tune", "1.1.2.9016")

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

  expect_snapshot(
    aug_res <- augment(grid_mixed_res)
  )

  expect_equal(nrow(aug_res), nrow(sim_tr))
  expect_equal(names(aug_res), c(".pred", ".pred_time", "event_time", "X1", "X2"))
  expect_true(is.list(aug_res$.pred))
  expect_equal(
    names(aug_res$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_snapshot(
    aug_res <- augment(grid_mixed_res, eval_time = 10)
  )
  expect_no_warning(
    aug_res <- augment(grid_mixed_res, parameters = tibble(penalty = 0.1))
  )
})

test_that("augment() works for resample_results", {
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
    bag_tree() %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with a mixture of metrics --------------------------------

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  set.seed(2193)
  rs_mixed_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rsctrl
    )

  aug_res <- augment(rs_mixed_res)

  expect_equal(nrow(aug_res), nrow(sim_tr))
  expect_equal(names(aug_res), c(".pred", ".pred_time", "event_time", "X1", "X2"))
  expect_true(is.list(aug_res$.pred))
  expect_equal(
    names(aug_res$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
})

test_that("augment() works for last fit", {
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

  # last fit for models with a mixture of metrics ------------------------------

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  set.seed(2193)
  rs_mixed_res <-
    survival_reg() %>%
    last_fit(
      event_time ~ X1 + X2,
      split = split,
      metrics = mix_mtrc,
      eval_time = time_points
    )

  aug_res <- augment(rs_mixed_res)

  expect_equal(nrow(aug_res), nrow(sim_te))
  expect_equal(names(aug_res), c(".pred", ".pred_time", "event_time", "X1", "X2"))
  expect_true(is.list(aug_res$.pred))
  expect_equal(
    names(aug_res$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
})

test_that("augment() for survival models skips unavailble prediction type", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9001")
  skip_if_not_installed("prodlim")
  skip_if_not_installed("aorsf")

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  time_points <- c(1, 5, 10)

  # this engine does not provide predictions of type = "time"
  rf_fit <-
    rand_forest() %>%
    set_engine("aorsf") %>%
    set_mode("censored regression") %>%
    fit(event_time ~ ., data = sim_dat)

  rf_aug <- augment(rf_fit, new_data = sim_dat, eval_time = time_points)
  expect_equal(nrow(rf_aug), nrow(sim_dat))
  expect_equal(names(rf_aug), c(".pred", "event_time", "X1", "X2"))
  expect_true(is.list(rf_aug$.pred))
  expect_equal(
    names(rf_aug$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored",
      ".weight_censored")
  )
})

test_that("augment() for survival models errors if eval_time is missing", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9008")

  sr_fit <- survival_reg() %>%
    fit(Surv(time, status) ~ ., data = lung)

  expect_snapshot(error = TRUE, {
    augment(sr_fit, lung)
  })
})
