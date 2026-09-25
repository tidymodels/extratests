library(tidymodels)

data("Chicago")


test_that('recipe with no steps', {
  bare_rec <- recipe(ridership ~ ., data = head(Chicago))

  bare_info <- tune_args(bare_rec)
  check_tune_args_tibble(bare_info)
  expect_identical(nrow(bare_info), 0L)
})

test_that('recipe with no tunable parameters', {
  rm_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_rm(date, ends_with("away"))

  rm_info <- tune_args(rm_rec)
  check_tune_args_tibble(rm_info)
  expect_identical(nrow(rm_info), 0L)
})

test_that('recipe with tunable parameters', {
  spline_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_date(date) %>%
    step_holiday(date) %>%
    step_rm(date, ends_with("away")) %>%
    step_impute_knn(all_predictors(), neighbors = tune("imputation")) %>%
    step_other(all_nominal(), threshold = tune()) %>%
    step_dummy(all_nominal()) %>%
    step_normalize(all_predictors()) %>%
    step_bs(all_predictors(), deg_free = tune(), degree = tune())

  spline_info <- tune_args(spline_rec)
  check_tune_args_tibble(spline_info)
  expected_cols <- c('step_impute_knn', 'step_other', 'step_bs', 'step_bs')
  expect_identical(
    spline_info$component,
    expected_cols
  )
  expect_all_equal(spline_info$source, "recipe")
  nms <- c('neighbors', 'threshold', 'deg_free', 'degree')
  expect_identical(spline_info$name, nms)
  ids <- c('imputation', 'threshold', 'deg_free', 'degree')
  expect_identical(spline_info$id, ids)
})


test_that('model with no parameters', {
  lm_model <- linear_reg() %>%
    set_engine("lm")

  lm_info <- tune_args(lm_model)
  check_tune_args_tibble(lm_info)
  expect_identical(nrow(lm_info), 0L)
})

test_that('model with main and engine parameters', {
  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  c5_info <- tune_args(bst_model)
  check_tune_args_tibble(c5_info)
  expect_identical(nrow(c5_info), 2L)
  expect_all_equal(c5_info$source, "model_spec")
  expect_all_equal(c5_info$component, "boost_tree")
  expect_all_equal(c5_info$component_id, NA_character_)
  nms <- c("trees", "rules")
  expect_identical(c5_info$name, nms)
  ids <- c("funky name \n", "rules")
  expect_identical(c5_info$id, ids)
})


test_that("workflow with tunable recipe", {
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

  wf_info <- tune_args(wf_tunable_recipe)
  check_tune_args_tibble(wf_info)
  expect_all_equal(wf_info$source, "recipe")
})

test_that("workflow with tunable model", {
  rm_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_rm(date, ends_with("away"))
  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)
  wf_tunable_model <- workflow(rm_rec, bst_model)

  wf_info <- tune_args(wf_tunable_model)
  check_tune_args_tibble(wf_info)
  expect_identical(nrow(wf_info), 2L)
  expect_all_equal(wf_info$source, "model_spec")
})

test_that("workflow with tunable recipe and model", {
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

  wf_info <- tune_args(wf_tunable)
  check_tune_args_tibble(wf_info)
  expect_identical(
    wf_info$source,
    c(rep("model_spec", 2), rep("recipe", 4))
  )
})
