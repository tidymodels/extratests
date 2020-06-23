context("engine-specific parameters with earth")

library(tidymodels)

mars_mod <-
  mars(num_terms = tune()) %>%
  set_engine("earth", nk = tune()) %>%
  set_mode("regression")

set.seed(192)
rs <- bootstraps(mtcars, times = 5)

## -----------------------------------------------------------------------------

test_that('grid search', {
  skip_if(utils::packageVersion("dials") <= "0.0.7")
  skip_if(utils::packageVersion("tune")  <= "0.1.0")

  set.seed(2893)
  expect_error(
    mars_tune <-
      mars_mod %>%
      tune_grid(mpg ~ ., resamples = rs, grid = 4),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(mars_tune)), 8)
})


## -----------------------------------------------------------------------------

test_that('Bayes search', {
  skip_if(utils::packageVersion("dials") <= "0.0.7")
  skip_if(utils::packageVersion("tune")  <= "0.1.0")

  set.seed(2893)
  expect_error(
    mars_search <-
      mars_mod %>%
      tune_bayes(mpg ~ ., resamples = rs, initial = 3, iter = 2),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(mars_search)), 10)
})




