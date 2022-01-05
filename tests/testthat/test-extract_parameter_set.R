library(tidymodels)
data("Chicago", package = "modeldata")

test_that('recipe with no steps', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  bare_rec <- recipe(ridership ~ ., data = head(Chicago))

  bare_info <- extract_parameter_set_dials(bare_rec)
  check_parameter_set_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that('recipe with no tunable parameters', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  rm_rec <-
    recipe(ridership ~ ., data = head(Chicago)) %>%
    step_rm(date, ends_with("away"))

  rm_info <- extract_parameter_set_dials(rm_rec)
  check_parameter_set_tibble(rm_info)
  expect_equal(nrow(rm_info), 0)
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

  spline_info <- extract_parameter_set_dials(spline_rec)
  check_parameter_set_tibble(spline_info)
  expected_cols <- c('step_impute_knn', 'step_other', 'step_bs', 'step_bs')
  expect_equal(
    spline_info$component,
    expected_cols
  )
  expect_true(all(spline_info$source == "recipe"))
  nms <- c('neighbors', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$name, nms)
  ids <- c('imputation', 'threshold', 'deg_free', 'degree')
  expect_equal(spline_info$id, ids)

  expect_equal(spline_info$object[[1]], dials::neighbors(c(1, 10)))
  expect_equal(spline_info$object[[2]], dials::threshold(c(0, 1/10)))
  expect_equal(spline_info$object[[3]], dials::spline_degree(c(1, 15)))
  expect_equal(spline_info$object[[4]], dials::degree_int(c(1, 2)))

})


test_that('model with no parameters', {
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9005")

  lm_model <- linear_reg() %>% set_engine("lm")

  lm_info <- extract_parameter_set_dials(lm_model)
  check_parameter_set_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that('model with main and engine parameters', {
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9005")

  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  c5_info <- extract_parameter_set_dials(bst_model)
  check_parameter_set_tibble(c5_info)
  expect_equal(nrow(c5_info), 2)
  expect_true(all(c5_info$source == "model_spec"))
  expect_true(all(c5_info$component == "boost_tree"))
  expect_equal(c5_info$component_id, c("main", "engine"))
  nms <- c("trees", "rules")
  expect_equal(c5_info$name, nms)
  ids <- c("funky name \n", "rules")
  expect_equal(c5_info$id, ids)

  expect_equal(c5_info$object[[1]], dials::trees(c(1, 100)))
  expect_equal(c5_info$object[[2]], NA)
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

  wf_info <- extract_parameter_set_dials(wf_tunable_recipe)
  check_parameter_set_tibble(wf_info)
  expect_true(all(wf_info$source == "recipe"))
})

test_that("workflow with tunable model", {
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9005" ||
            utils::packageVersion("workflows") < "0.2.4.9002")

  rm_rec <- recipe(ridership ~ ., data = head(Chicago)) %>%
    step_rm(date, ends_with("away"))
  bst_model <-
    boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)
  wf_tunable_model <- workflow(rm_rec, bst_model)

  wf_info <- extract_parameter_set_dials(wf_tunable_model)
  check_parameter_set_tibble(wf_info)
  expect_equal(nrow(wf_info), 2)
  expect_true(all(wf_info$source == "model_spec"))
})

test_that("workflow with tunable recipe and model", {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001" ||
            utils::packageVersion("parsnip") < "0.1.7.9005" ||
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

  wf_info <- extract_parameter_set_dials(wf_tunable)
  check_parameter_set_tibble(wf_info)
  expect_equal(
    wf_info$source,
    c(rep("model_spec", 2), rep("recipe", 4))
  )
})
