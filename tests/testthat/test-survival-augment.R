library(testthat)
library(tidymodels)
library(prodlim)
suppressPackageStartupMessages(library(censored))

# ------------------------------------------------------------------------------

test_that('augmenting survival models ', {
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9001")

  set.seed(1)
  sim_dat <- SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)

  time_points <- c(1, 5, 10)

  # ------------------------------------------------------------------------------

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

  # ------------------------------------------------------------------------------

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

test_that("augment() for survival models errors if eval_time is missing", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9008")

  sr_fit <- survival_reg() %>%
    fit(Surv(time, status) ~ ., data = lung)

  expect_snapshot(error = TRUE, {
    augment(sr_fit, lung)
  })
})
