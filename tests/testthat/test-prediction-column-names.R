suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9004")

test_that("determine prediction column names for workflows", {
  skip_if_not_installed("parsnip", "1.2.1.9004")
  # complement to tidymodels/parsnip#1224

  ### classification
  lr_fit <- workflow(Class ~ ., logistic_reg()) %>% fit(data = two_class_dat)
  expect_equal(
    .get_prediction_column_names(lr_fit),
    list(estimate = ".pred_class",
         probabilities = c(".pred_Class1", ".pred_Class2"))
  )
  expect_equal(
    .get_prediction_column_names(lr_fit, syms = TRUE),
    list(estimate = list(quote(.pred_class)),
         probabilities = list(quote(.pred_Class1), quote(.pred_Class2)))
  )

  ### regression
  ols_fit <-  workflow(mpg ~ ., linear_reg()) %>% fit(data = mtcars)
  expect_equal(
    .get_prediction_column_names(ols_fit),
    list(estimate = ".pred",
         probabilities = character(0))
  )
  expect_equal(
    .get_prediction_column_names(ols_fit, syms = TRUE),
    list(estimate = list(quote(.pred)),
         probabilities = list())
  )
})

test_that("determine prediction column names for censored regression", {
  skip_if_not_installed("parsnip", "1.2.1.9004")
  # complement to tidymodels/parsnip#1224

  surv_fit <- survival_reg() %>% fit(Surv(time, status) ~ ., data = lung)
  expect_equal(
    .get_prediction_column_names(surv_fit),
    list(estimate = ".pred_time",
         probabilities = c(".pred"))
  )
  expect_equal(
    .get_prediction_column_names(surv_fit, syms = TRUE),
    list(estimate = list(quote(.pred_time)),
         probabilities = list(quote(.pred)))
  )
})
