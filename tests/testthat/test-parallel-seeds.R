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
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)

  set.seed(1)
  res_1 <- fit_resamples(rf_spec, Class ~ ., folds)
  expect_equal(nrow(res_1$.notes[[1]]), 0)

  set.seed(1)
  res_2 <- fit_resamples(rf_spec, Class ~ ., folds)
  expect_equal(nrow(res_2$.notes[[1]]), 0)

  stopCluster(cl)
  registerDoSEQ() # stopCluster() does not reset the number of workers

  expect_equal(
    collect_metrics(res_1),
    collect_metrics(res_2)
  )
})
