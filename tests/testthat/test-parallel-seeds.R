context("consistent parallel seeds")

# ------------------------------------------------------------------------------

library(testthat)
library(tidymodels)
library(modeldata)
library(doParallel)
data(two_class_dat)

# ------------------------------------------------------------------------------

rf_spec <-
  rand_forest(trees = 25) %>%
  set_engine("ranger") %>%
  set_mode("classification")

set.seed(123)
folds <- vfold_cv(two_class_dat)

test_that('parallel seeds', {
  skip_if(utils::packageVersion("tune") <= "0.1.1.9000")

  library(doParallel)
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)

  set.seed(1)
  res_1 <- fit_resamples(rf_spec, Class ~ ., folds)
  res_1

  set.seed(1)
  res_2 <- fit_resamples(rf_spec, Class ~ ., folds)
  res_2

  expect_equal(
    collect_metrics(res_1),
    collect_metrics(res_2)
  )
})
