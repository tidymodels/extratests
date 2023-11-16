skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")

library(tidymodels)
suppressPackageStartupMessages(library(censored))

test_that("can `fit()` a censored workflow with a formula", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  mod <- proportional_hazards(engine = "glmnet", penalty = 0.1)

  workflow <- workflow()
  workflow <- add_formula(workflow, surv ~ .)
  workflow <- add_model(workflow, mod)

  wf_fit <- fit(workflow, lung)

  expect_s3_class(wf_fit$fit$fit, "model_fit")

  expect_equal(
    wf_fit$fit$fit$fit$fit$beta,
    censored::coxnet_train(surv ~ ., data = lung)$fit$beta
  )
})

test_that("can `fit()` a censored workflow with a model formula", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  model_formula <- surv ~ . - sex + strata(sex)

  mod <- proportional_hazards(engine = "survival")

  workflow <- workflow()
  workflow <- add_formula(workflow, surv ~ .)
  workflow <- add_model(workflow, mod, formula = model_formula)

  wf_fit <- fit(workflow, lung)

  expect_s3_class(wf_fit$fit$fit, "model_fit")

  expect_equal(
    wf_fit$fit$fit$fit$coefficients,
    survival::coxph(
      formula = surv ~ . - sex + strata(sex),
      data = lung)$coefficients
  )
})

test_that("can `fit()` a censored workflow with variables", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  mod <- proportional_hazards(engine = "glmnet", penalty = 0.1)

  workflow <- workflow()
  workflow <- add_variables(workflow, outcomes = surv, predictors = everything())
  workflow <- add_model(workflow, mod)

  wf_fit <- fit(workflow, lung)

  expect_s3_class(wf_fit$fit$fit, "model_fit")

  expect_equal(
    wf_fit$fit$fit$fit$fit$beta,
    censored::coxnet_train(surv ~ ., data = lung)$fit$beta
  )
})

test_that("can `fit()` a censored workflow with a recipe", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  rec <- recipes::recipe(surv ~ ., lung)

  mod <- proportional_hazards(engine = "glmnet", penalty = 0.1)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  wf_fit <- fit(workflow, lung)

  expect_s3_class(wf_fit$fit$fit, "model_fit")

  expect_equal(
    wf_fit$fit$fit$fit$fit$beta,
    censored::coxnet_train(surv ~ ., data = lung)$fit$beta
  )
})

test_that("can `predict()` a censored workflow with a formula", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  mod <- proportional_hazards(engine = "glmnet", penalty = 0.1)

  workflow <- workflow()
  workflow <- add_formula(workflow, surv ~ .)
  workflow <- add_model(workflow, mod)

  wf_fit <- fit(workflow, lung)

  preds <- predict(wf_fit, new_data = lung)

  expect_identical(names(preds), ".pred_time")
  expect_type(preds$.pred_time, "double")

  preds <- predict(wf_fit, new_data = lung, type = "survival", eval_time = c(100, 200))

  expect_identical(names(preds), ".pred")
  expect_type(preds$.pred, "list")
  expect_true(
    all(purrr::map_lgl(
      preds$.pred,
       ~ identical(names(.x), c(".eval_time", ".pred_survival"))
    ))
  )

  expect_error(
    predict(wf_fit, new_data = lung, type = "numeric")
  )
})

test_that("can `predict()` a censored workflow with a model formula", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  model_formula <- surv ~ . - sex + strata(sex)

  mod <- proportional_hazards(engine = "survival")

  workflow <- workflow()
  workflow <- add_formula(workflow, surv ~ .)
  workflow <- add_model(workflow, mod, formula = model_formula)

  wf_fit <- fit(workflow, lung)

  preds <- predict(wf_fit, new_data = lung)

  expect_identical(names(preds), ".pred_time")
  expect_type(preds$.pred_time, "double")

  preds <- predict(wf_fit, new_data = lung, type = "survival", eval_time = c(100, 200))

  expect_identical(names(preds), ".pred")
  expect_type(preds$.pred, "list")
  expect_true(
    all(purrr::map_lgl(
      preds$.pred,
      ~ identical(names(.x), c(".eval_time", ".pred_survival"))
    ))
  )

  expect_error(
    predict(wf_fit, new_data = lung, type = "numeric")
  )
})

test_that("can `predict()` a censored workflow with variables", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  mod <- proportional_hazards(engine = "glmnet", penalty = 0.1)

  workflow <- workflow()
  workflow <- add_variables(workflow, outcomes = surv, predictors = everything())
  workflow <- add_model(workflow, mod)

  wf_fit <- fit(workflow, lung)

  preds <- predict(wf_fit, new_data = lung)

  expect_identical(names(preds), ".pred_time")
  expect_type(preds$.pred_time, "double")

  preds <- predict(wf_fit, new_data = lung, type = "survival", eval_time = c(100, 200))

  expect_identical(names(preds), ".pred")
  expect_type(preds$.pred, "list")
  expect_true(
    all(purrr::map_lgl(
      preds$.pred,
      ~ identical(names(.x), c(".eval_time", ".pred_survival"))
    ))
  )

  expect_error(
    predict(wf_fit, new_data = lung, type = "numeric")
  )
})

test_that("can `predict()` a censored workflow with a recipe", {
  lung <- lung |>
    tidyr::drop_na() |>
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  rec <- recipes::recipe(surv ~ ., lung)

  mod <- proportional_hazards(engine = "glmnet", penalty = 0.1)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  wf_fit <- fit(workflow, lung)

  preds <- predict(wf_fit, new_data = lung)

  expect_identical(names(preds), ".pred_time")
  expect_type(preds$.pred_time, "double")

  preds <- predict(wf_fit, new_data = lung, type = "survival", eval_time = c(100, 200))

  expect_identical(names(preds), ".pred")
  expect_type(preds$.pred, "list")
  expect_true(
    all(purrr::map_lgl(
      preds$.pred,
      ~ identical(names(.x), c(".eval_time", ".pred_survival"))
    ))
  )

  expect_error(
    predict(wf_fit, new_data = lung, type = "numeric")
  )
})
