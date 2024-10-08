library(testthat)
library(tidymodels)

withr::local_options(lifecycle_verbosity = "quiet")

rec_1 <- recipe(mpg ~ ., mtcars) %>%
  step_center(all_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = varying()) %>%
  step_pca(all_predictors(), num_comp = varying())

rec_2 <- recipe(mpg ~ ., mtcars) %>%
  step_center(all_predictors()) %>%
  step_impute_knn(all_predictors()) %>%
  step_pca(all_predictors())

rec_3 <- recipe(mpg ~ ., mtcars)

test_that('recipe parameters', {
  withr::local_options(lifecycle_verbosity = "quiet")

  # un-randomify the id names
  rec_1_id <- rec_1
  rec_1_id$steps[[1]]$id <- "center_1"
  rec_1_id$steps[[2]]$id <- "knnimpute_1"
  rec_1_id$steps[[3]]$id <- "pca_1"

  rec_res_1 <- varying_args(rec_1_id)

  exp_1 <- tibble(
    name = c("na_rm", "neighbors", "options", "num_comp",
             "threshold", "options", "keep_original_cols"),
    varying = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
    id = c("center_1", rep("knnimpute_1", 2), rep("pca_1", 4)),
    type = rep("step", 7)
  )

  expect_equal(rec_res_1, exp_1)

  # un-randomify the id names
  rec_2_id <- rec_2
  rec_2_id$steps[[1]]$id <- "center_1"
  rec_2_id$steps[[2]]$id <- "knnimpute_1"
  rec_2_id$steps[[3]]$id <- "pca_1"

  rec_res_2 <- varying_args(rec_2_id)
  exp_2 <- exp_1
  exp_2$varying <- FALSE
  expect_equal(rec_res_2, exp_2)

  rec_res_3 <- varying_args(rec_3)

  exp_3 <- tibble(
    name = character(),
    varying = logical(),
    id = character(),
    type = character()
  )

  expect_equal(rec_res_3, exp_3)
})

test_that("recipe steps with non-varying args error if specified as varying()", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip("not applicable")

  rec_bad_varying <- rec_1
  rec_bad_varying$steps[[1]]$skip <- varying()

  expect_snapshot(
    varying_args(rec_bad_varying),
    error = TRUE
  )
})

test_that("`full = FALSE` returns only varying arguments", {
  withr::local_options(lifecycle_verbosity = "quiet")

  x_spec <- rand_forest(min_n = varying())  %>%
    set_engine("ranger", sample.fraction = varying())

  x_rec <- rec_1

  expect_equal(
    varying_args(x_spec, full = FALSE)$name,
    c("min_n", "sample.fraction")
  )

  expect_equal(
    varying_args(x_rec, full = FALSE)$name,
    c("neighbors", "num_comp")
  )

})

