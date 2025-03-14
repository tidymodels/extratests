test_that("boost_tree - xgboost - regression", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- boost_tree("regression", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("boost_tree - xgboost - classification", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Street ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- boost_tree("classification", "xgboost")

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("linear_reg - glmnet - regression", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- linear_reg("regression", "glmnet", penalty = 0)

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("logistic_reg - glmnet - classification", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Street ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- logistic_reg("classification", "glmnet", penalty = 0)

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("logistic_reg - LiblineaR - classification", {
  skip_if_not_installed("LiblineaR")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Street ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- logistic_reg("classification", "LiblineaR")

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    suppressMessages(
      wf_fit <- fit(wf_spec, ames)
    )
  )
})

test_that("multinom_reg - glmnet - classification", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Street ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- multinom_reg("classification", "glmnet", penalty = 0)

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("rand_forest - ranger - regression", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- rand_forest("regression", "ranger")

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("rand_forest - ranger - classification", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Street ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- rand_forest("classification", "ranger")

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})

test_that("svm_linear - LiblineaR - regression", {
  skip_if_not_installed("LiblineaR")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", "1.4.1")
  skip_if_not_installed("recipes", "1.1.1.9000")
  skip_if_not_installed("parsnip", "1.3.0")
  skip_if_not_installed("workflows", "1.2.0")

  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) %>%
    step_dummy(all_nominal_predictors(), sparse = "yes")

  mod_spec <- svm_linear("regression", "LiblineaR")

  wf_spec <- workflow(rec_spec, mod_spec)

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  expect_no_warning(
    wf_fit <- fit(wf_spec, ames)
  )
})
