library(tidymodels)
data("Chicago", package = "modeldata")

test_that('recipe with no steps', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  bare_rec <- recipe(ridership ~ ., data = head(Chicago))

  expect_error(
    extract_parameter_dials(bare_rec, id = "none there")
  )
})

test_that('recipe with no tunable parameters', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  rm_rec <-
    recipe(ridership ~ ., data = head(Chicago)) %>%
    step_rm(date, ends_with("away"))

  expect_error(
    extract_parameter_dials(rm_rec, id = "none there")
  )
})

test_that('recipe with tunable parameters', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  spline_rec <-
    recipe(ridership ~ ., data = head(Chicago)) %>%
    step_date(date) %>%
    step_holiday(date) %>%
    step_rm(date, ends_with("away")) %>%
    step_impute_knn(all_predictors(), neighbors = tune("imputation")) %>%
    step_other(all_nominal(), threshold = tune()) %>%
    step_dummy(all_nominal()) %>%
    step_normalize(all_predictors()) %>%
    step_bs(all_predictors(), deg_free = tune(), degree = tune())

  expect_equal(
    extract_parameter_dials(spline_rec, "imputation"),
    dials::neighbors()
  )
  expect_equal(
    extract_parameter_dials(spline_rec, "threshold"),
    dials::threshold(c(0, 1/10))
  )
  expect_equal(
    extract_parameter_dials(spline_rec, "deg_free"),
    dials::spline_degree(range = c(1, 15))
  )
  expect_equal(
    extract_parameter_dials(spline_rec, "degree"),
    dials::degree_int(c(1, 2))
  )
})


test_that('model with no parameters', {
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9004")

  lm_model <- linear_reg() %>% set_engine("lm")

  expect_error(
    extract_parameter_dials(lm_model, id = "none there")
  )
})

test_that('model with main and engine parameters', {
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9004")

  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  expect_equal(
    hardhat::extract_parameter_dials(bst_model, id = "funky name \n"),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(bst_model, id = "rules"),
    NA
  )
})


test_that("workflow with tunable recipe", {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001" ||
            utils::packageVersion("workflows") < "0.2.4.9002")

  spline_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_date(date) %>%
    step_holiday(date) %>%
    step_rm(date, ends_with("away")) %>%
    step_impute_knn(all_predictors(), neighbors = tune("imputation")) %>%
    step_other(all_nominal(), threshold = tune()) %>%
    step_dummy(all_nominal()) %>%
    step_normalize(all_predictors()) %>%
    step_bs(all_predictors(), deg_free = tune(), degree = tune())
  lm_model <- linear_reg() %>%
    set_engine("lm")
  wf_tunable_recipe <- workflow(spline_rec, lm_model)

  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "imputation"),
    dials::neighbors()
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "threshold"),
    dials::threshold(c(0, 1/10))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "deg_free"),
    dials::spline_degree(range = c(1, 15))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "degree"),
    dials::degree_int(c(1, 2))
  )
})

test_that("workflow with tunable model", {
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9004" ||
            utils::packageVersion("workflows") < "0.2.4.9002")

  rm_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_rm(date, ends_with("away"))
  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)
  wf_tunable_model <- workflow(rm_rec, bst_model)

  expect_equal(
    hardhat::extract_parameter_dials(wf_tunable_model, id = "funky name \n"),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_model, id = "rules"),
    NA
  )
})

test_that("workflow with tunable recipe and model", {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001" ||
            utils::packageVersion("parsnip") < "0.1.7.9004" ||
            utils::packageVersion("workflows") < "0.2.4.9002")

  spline_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_date(date) %>%
    step_holiday(date) %>%
    step_rm(date, ends_with("away")) %>%
    step_impute_knn(all_predictors(), neighbors = tune("imputation")) %>%
    step_other(all_nominal(), threshold = tune()) %>%
    step_dummy(all_nominal()) %>%
    step_normalize(all_predictors()) %>%
    step_bs(all_predictors(), deg_free = tune(), degree = tune())
  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)
  wf_tunable <- workflow(spline_rec, bst_model)

  expect_equal(
    extract_parameter_dials(wf_tunable, "imputation"),
    dials::neighbors()
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, "threshold"),
    dials::threshold(c(0, 1/10))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, "deg_free"),
    dials::spline_degree(range = c(1, 15))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, "degree"),
    dials::degree_int(c(1, 2))
  )
  expect_equal(
    hardhat::extract_parameter_dials(wf_tunable, id = "funky name \n"),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, id = "rules"),
    NA
  )
})
