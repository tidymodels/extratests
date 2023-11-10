library(tidymodels)
data(two_class_dat, package = "modeldata")

set.seed(2067)
folds <- vfold_cv(two_class_dat)

tree_mod <-
  decision_tree(min_n = tune()) %>%
  set_engine("C5.0", noGlobalPruning = tune()) %>%
  set_mode("classification")

boost_mod <-
  boost_tree(min_n = tune(), trees = 3) %>%
  set_engine("C5.0", noGlobalPruning = tune()) %>%
  set_mode("classification")

grid <-
  expand.grid(
    noGlobalPruning = c(TRUE, FALSE),
    min_n = c(2, 10)
  )

## -----------------------------------------------------------------------------

test_that('single tree grid search', {
  expect_error(
    tree_tuned <-
      tree_mod %>%
      tune_grid(Class ~ ., resamples = folds, grid = grid),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(tree_tuned)), 8)
})


test_that('single tree Bayesian search', {
  set.seed(2893)
  expect_error(
    tree_search <-
      tree_mod %>%
      tune_bayes(Class ~ ., resamples = folds, initial = 3, iter = 2) %>%
      suppressMessages(),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(tree_search)), 10)
})

## -----------------------------------------------------------------------------

test_that('boosted tree grid search', {
  expect_error(
    boost_tuned <-
      boost_mod %>%
      tune_grid(Class ~ ., resamples = folds, grid = grid),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(boost_tuned)), 8)
})

test_that('boosted tree Bayesian search', {
  set.seed(2893)
  expect_error(
    boost_search <-
      boost_mod %>%
      tune_bayes(Class ~ ., resamples = folds, initial = 3, iter = 2) %>%
      suppressMessages(),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(boost_search)), 10)
})



