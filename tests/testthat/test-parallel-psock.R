library(testthat)
suppressPackageStartupMessages(library(discrim))
library(themis)
library(tidymodels)
library(modeldata)
suppressPackageStartupMessages(library(doParallel))
data(two_class_dat)

# ------------------------------------------------------------------------------

discrim_mod <- discrim_linear() %>%
  set_engine("MASS")
set.seed(123)
folds <- vfold_cv(two_class_dat)


test_that('LDA parallel test', {
  library(doParallel)
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)

  expect_error(
    res <- fit_resamples(discrim_mod, Class ~ ., folds),
    regex = NA
  )
  stopCluster(cl)
  registerDoSEQ() # stopCluster() does not reset the number of workers

  expect_all_equal(purrr::map_int(res$.notes, nrow), 0L)
})

# ------------------------------------------------------------------------------

rec <-
  recipe(Class ~ ., data = two_class_dat) %>%
  step_smote(Class)

discrim_wflow <-
  workflow() %>%
  add_model(discrim_mod) %>%
  add_recipe(rec)

test_that('recipe-adjacent parallel test', {
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)

  expect_error(
    res <- fit_resamples(discrim_wflow, folds),
    regex = NA
  )
  stopCluster(cl)
  registerDoSEQ()

  expect_all_equal(purrr::map_int(res$.notes, nrow), 0L)
})
