library(tidymodels)

rf_mod <-
  parsnip::rand_forest(min_n = tune(), trees = 12) %>%
  parsnip::set_engine(
    "ranger",
    regularization.factor = tune(),
    regularization.usedepth = tune()
  ) %>%
  set_mode("regression")

rf_param <-
  rf_mod %>%
  extract_parameter_set_dials() %>%
  update(regularization.factor = regularization_factor(c(.1, 1)))

set.seed(192)
rs <- bootstraps(mtcars, times = 5)

## -----------------------------------------------------------------------------

test_that('grid search', {
  skip_if_not_installed("dials", minimum_version = "1.3.0.9001")

  set.seed(2893)
  expect_error(
    rf_tune <-
      rf_mod %>%
      tune_grid(mpg ~ ., resamples = rs, grid = 4, param_info = rf_param) %>%
      suppressMessages(),
    regex = NA
  )
  num_mtrc <- nrow(as_tibble(.get_tune_metrics(rf_tune)))
  expect_equal(nrow(collect_metrics(rf_tune)), num_mtrc * 4)
})


## -----------------------------------------------------------------------------

test_that('Bayes search', {
  skip_if_not_installed("dials", minimum_version = "1.3.0.9001")

  set.seed(2893)
  expect_error(
    rf_search <-
      rf_mod %>%
      tune_bayes(
        mpg ~ .,
        resamples = rs,
        initial = 3,
        iter = 2,
        param_info = rf_param
      ) %>%
      suppressMessages(),
    regex = NA
  )
  num_mtrc <- nrow(as_tibble(.get_tune_metrics(rf_search)))
  expect_equal(nrow(collect_metrics(rf_search)), num_mtrc * 5)
})

