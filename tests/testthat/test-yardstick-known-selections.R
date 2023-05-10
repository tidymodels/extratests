library(tune)
library(rsample)
library(parsnip)
library(yardstick)
library(modeldata)

skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("known selections don't kick in outside of catalog env", {
  tune_env <- rlang::ns_env("tune")$tune_env
  expect_null(tune_env$known_selections)

  test_res <- rmse(two_class_dat, A, B)

  expect_null(tune_env$known_selections)
})

test_that("known selections are cleared after tune runs", {
  tune_env <- rlang::ns_env("tune")$tune_env
  expect_null(tune_env$known_selections)

  fit_resamples(
    logistic_reg(),
    Class ~ .,
    bootstraps(two_class_dat, 5)
  )

  expect_null(tune_env$known_selections)
})

test_that("known selections are stored", {
  # only run interactively--see
  # https://github.com/tidymodels/tune/blob/2a6332c35e32b4416ce868815884d8b26c3ff772/R/logging.R#L102-L108
  skip_if(tune:::is_testing())

  extract_known_selections <- function(x) {
    rlang::ns_env("tune")$tune_env$known_selections
  }

  res <-
    fit_resamples(
      logistic_reg(),
      Class ~ .,
      bootstraps(two_class_dat, 5),
      control = control_resamples(extract = extract_known_selections)
    )

  known_selections <- collect_extracts(res)[[5, ".extracts"]]

  expect_equal(
    known_selections,
    list(list(
      Class = "Class",
      .pred_class = ".pred_class",
      `~".pred_Class1"` = ".pred_Class1"
    ))
  )
})
