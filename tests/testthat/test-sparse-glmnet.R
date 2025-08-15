library(testthat)
library(tidymodels)
library(hardhat)
data(mlc_churn, package = "modeldata")

set.seed(123)
mlc_folds <- vfold_cv(mlc_churn, v = 5)

## -----------------------------------------------------------------------------

parsnip_mod <-
  logistic_reg(penalty = .1) %>%
  set_engine("glmnet")

wf <-
  workflow() %>%
  add_model(parsnip_mod)

rec <-
  recipe(
    churn ~
      number_vmail_messages +
        number_customer_service_calls +
        international_plan,
    data = mlc_churn
  )

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
matrix_bp <- default_recipe_blueprint(composition = "matrix")

## -----------------------------------------------------------------------------

test_that('sparse composition errors', {
  skip_if_not_installed("hardhat", minimum_version = "1.4.0.9002")

  expect_snapshot(error = TRUE, {
    mold(rec, mlc_churn, blueprint = sparse_bp)
  })
})

test_that('sparse composition works', {
  rec <- rec %>% step_dummy(international_plan)

  expect_error(
    processed <- mold(rec, mlc_churn, blueprint = sparse_bp),
    NA
  )
  expect_s4_class(processed$predictors, "dgCMatrix")

  forged <- forge(mlc_churn, blueprint = processed$blueprint)$predictors
  expect_s4_class(forged, "dgCMatrix")

  expect_error(
    wf_pre <-
      wf %>%
      add_recipe(rec, blueprint = sparse_bp) %>%
      .fit_pre(data = mlc_churn),
    NA
  )
  expect_s4_class(wf_pre$pre$mold$predictors, "dgCMatrix")

  expect_error(
    .fit_model(wf_pre, control = control_workflow()),
    NA
  )

  expect_error(
    wf %>%
      add_recipe(rec, blueprint = sparse_bp) %>%
      fit_resamples(mlc_folds),
    NA
  )
})

test_that('matrix composition works', {
  rec <- rec %>% step_dummy(international_plan)

  expect_error(
    processed <- mold(rec, mlc_churn, blueprint = matrix_bp),
    NA
  )
  expect_true(is.numeric(processed$predictors))
  expect_equal(dim(processed$predictors), c(5000, 3))

  forged <- forge(mlc_churn, blueprint = processed$blueprint)$predictors

  expect_true(is.numeric(forged))
  expect_equal(dim(forged), c(5000, 3))

  expect_error(
    wf_pre <-
      wf %>%
      add_recipe(rec, blueprint = matrix_bp) %>%
      .fit_pre(data = mlc_churn),
    NA
  )
  expect_true(is.numeric(wf_pre$pre$mold$predictors))
  expect_equal(dim(wf_pre$pre$mold$predictors), c(5000, 3))

  expect_error(
    .fit_model(wf_pre, control = control_workflow()),
    NA
  )

  expect_error(
    wf %>%
      add_recipe(rec, blueprint = matrix_bp) %>%
      fit_resamples(mlc_folds),
    NA
  )
})
