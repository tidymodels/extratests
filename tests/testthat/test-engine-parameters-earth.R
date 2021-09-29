library(tidymodels)

mars_mod <-
  mars(num_terms = tune()) %>%
  set_engine("earth", nk = tune()) %>%
  set_mode("regression")

set.seed(192)
rs <- bootstraps(mtcars, times = 5)

## -----------------------------------------------------------------------------

test_that('grid search', {
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
  set.seed(2893)
  expect_error(
    mars_search <-
      mars_mod %>%
      tune_bayes(mpg ~ ., resamples = rs, initial = 3, iter = 2),
    regex = NA
  )
  expect_equal(nrow(collect_metrics(mars_search)), 10)
})




