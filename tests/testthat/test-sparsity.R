# recipe sparsity: yes
# step_dummy(all_nominal_predictors())
# recipe sparsity: no
# step_integer(all_nominal_predictors())
#
# sparsity: high
# no change
# sparsity: low
# ames <- dplyr::select(ames, Street, where(is.numeric))
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
  skip_if_not_installed("recipes", "1.1.0.9001")
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
        cli::cli_abort("correct: x is sparse matrix")
      } else {
        cli::cli_abort("wrong: x is dense matrix")
      }
    }, 
    .package = "parsnip"
  )

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, ames)
  )
})