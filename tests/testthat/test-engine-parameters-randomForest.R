skip_if_not_installed("tune", minimum_version = "1.3.0.9005")

library(tidymodels)

rf_mod <-
  parsnip::rand_forest(min_n = tune(), trees = 12) %>%
  parsnip::set_engine(
    "randomForest",
    maxnodes = tune()
  ) %>%
  set_mode("regression")

set.seed(192)
rs <- bootstraps(mtcars, times = 5)

## -----------------------------------------------------------------------------

test_that('grid search', {
  set.seed(2893)
  expect_error(
    rf_tune <-
      rf_mod %>%
      tune_grid(mpg ~ ., resamples = rs, grid = 4) %>%
      suppressMessages(),
    regex = NA
  )
  num_mtrc <- nrow(as_tibble(.get_tune_metrics(rf_tune)))
  expect_equal(nrow(collect_metrics(rf_tune)), num_mtrc * 4)
})


## -----------------------------------------------------------------------------

test_that('Bayes search', {
  set.seed(2893)
  expect_error(
    rf_search <-
      rf_mod %>%
      tune_bayes(mpg ~ ., resamples = rs, initial = 3, iter = 2) %>%
      suppressMessages(),
    regex = NA
  )
  num_mtrc <- nrow(as_tibble(.get_tune_metrics(rf_search)))
  expect_equal(nrow(collect_metrics(rf_search)), num_mtrc * 5)
})

