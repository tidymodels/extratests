# https://github.com/tidymodels/extratests/issues/229
#
# recipe sparsity: yes
# step_dummy(all_nominal_predictors())
# recipe sparsity: no
# step_integer(all_nominal_predictors())
#
# sparsity: high
# no change
# sparsity: low
#   ames <- dplyr::select(
#   ames,
#   Street,
#   where(function(x) is.numeric(x) && sum(x == 0) == 0)
# )
#
# model support: yes
# mod_spec <- boost_tree("regression", "xgboost")
# model support: no
# mod_spec <- linear_reg("regression", "lm")
#
# arg: auto
# step_dummy(all_nominal_predictors())
# arg: no
# step_dummy(all_nominal_predictors(), sparse = "no")
# arg: yes
# step_dummy(all_nominal_predictors(), sparse = "yes")

test_that("id: 1, recipe sparsity: yes, sparsity: high, model support: yes, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors())

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "yes"
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("correct: x is sparse matrix")
      } else {
        stop("wrong: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 2, recipe sparsity: yes, sparsity: high, model support: yes, arg: no", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "no")

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "no"
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 3, recipe sparsity: yes, sparsity: high, model support: yes, arg: yes", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "yes"
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("correct: x is sparse matrix")
      } else {
        stop("wrong: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 4, recipe sparsity: yes, sparsity: high, model support: no, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors())

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "auto"
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 5, recipe sparsity: yes, sparsity: high, model support: no, arg: no", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "no")

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "no"
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 6, recipe sparsity: yes, sparsity: high, model support: no, arg: yes", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    suppressWarnings(
      wf_fit <- fit(wf_spec, ames)
    )
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "yes"
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 7, recipe sparsity: yes, sparsity: low, model support: yes, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors())

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "no"
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 8, recipe sparsity: yes, sparsity: low, model support: yes, arg: no", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "no")

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "no"
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 9, recipe sparsity: yes, sparsity: low, model support: yes, arg: yes", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "yes"
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 10, recipe sparsity: yes, sparsity: low, model support: no, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors())

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "auto"
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 11, recipe sparsity: yes, sparsity: low, model support: no, arg: no", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "no")

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "no"
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 12, recipe sparsity: yes, sparsity: low, model support: no, arg: yes", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    suppressWarnings(
      wf_fit <- fit(wf_spec, ames)
    )
  )

  expect_identical(
    extract_preprocessor(wf_fit)$steps[[1]]$sparse,
    "yes"
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 13, recipe sparsity: no, sparsity: high, model support: yes, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_integer(all_nominal_predictors())

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("correct: x is sparse matrix")
      } else {
        stop("wrong: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 14, recipe sparsity: no, sparsity: high, model support: yes, arg: no", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "no"` if there isn't a step
  # That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 15, recipe sparsity: no, sparsity: high, model support: yes, arg: yes", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "yes"` if there isn't a 
  # step That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 16, recipe sparsity: no, sparsity: high, model support: no, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_integer(all_nominal_predictors())

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 17, recipe sparsity: no, sparsity: high, model support: no, arg: no", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "no"` if there isn't a step
  # That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 18, recipe sparsity: no, sparsity: high, model support: no, arg: yes", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "yes"` if there isn't a 
  # step That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 19, recipe sparsity: no, sparsity: low, model support: yes, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_integer(all_nominal_predictors())

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  local_mocked_bindings(
    xgb_train = function(x, ...) {
      if (methods::is(x, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 20, recipe sparsity: no, sparsity: low, model support: yes, arg: no", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "no"` if there isn't a step
  # That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 21, recipe sparsity: no, sparsity: low, model support: yes, arg: yes", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "yes"` if there isn't a 
  # step That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 22, recipe sparsity: no, sparsity: low, model support: no, arg: auto", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.0.9002")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.2.1.9004")
  skip_if_not_installed("workflows", "1.1.4.9001")

  data("ames", package = "modeldata")
  ames <- dplyr::select(
    ames,
    Street,
    where(function(x) is.numeric(x) && sum(x == 0) == 0)
  )

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_integer(all_nominal_predictors())

  mod_spec <- linear_reg("regression", "lm")

  wf_spec <- workflow(rec_spec, mod_spec)

  expect_no_error(
    wf_fit <- fit(wf_spec, ames)
  )

  local_mocked_bindings(
    lm = function(data, ...) {
      if (methods::is(data, "sparseMatrix")) {
        stop("wrong: x is sparse matrix")
      } else {
        stop("correct: x is dense matrix")
      }
    },
    .package = "stats"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})

test_that("id: 23, recipe sparsity: no, sparsity: low, model support: no, arg: no", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "no"` if there isn't a step
  # That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})

test_that("id: 24, recipe sparsity: no, sparsity: low, model support: no, arg: yes", {
  expect_true(TRUE)
  # Invalid combination since we can't set `sparse = "yes"` if there isn't a 
  # step That has a `sparse`` argument.
  # Keeping for completeness and in case we add an overwrite help function.
})
