context("engine-specific parameters - ranger")

library(tidymodels)

rf_mod <-
  parsnip::rand_forest(min_n = tune(), trees = 12) %>%
  parsnip::set_engine(
    "ranger",
    regularization.factor = tune(),
    regularization.usedepth = tune()
  ) %>%
  set_mode("regression")

set.seed(192)
rs <- bootstraps(mtcars, times = 5)

## -----------------------------------------------------------------------------

test_that('grid search', {
  skip_if(utils::packageVersion("dials") <= "0.0.7")
  skip_if(utils::packageVersion("tune")  <= "0.1.0")

  set.seed(2893)
  expect_error(
    rf_tune <-
      rf_mod %>%
      tune_grid(mpg ~ ., resamples = rs, grid = 4),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(rf_tune)), 8)
})


## -----------------------------------------------------------------------------

test_that('Bayes search', {
  skip_if(utils::packageVersion("dials") <= "0.0.7")
  skip_if(utils::packageVersion("tune")  <= "0.1.0")

  set.seed(2893)
  expect_error(
    rf_search <-
      rf_mod %>%
      tune_bayes(mpg ~ ., resamples = rs, initial = 3, iter = 2),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(rf_search)), 10)
})

