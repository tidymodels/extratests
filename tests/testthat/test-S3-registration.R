context("S3 registration")

# ------------------------------------------------------------------------------

library(testthat)
library(tidymodels)

test_recipe <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_pls(all_predictors(), outcome = "mpg")


# ------------------------------------------------------------------------------

test_that('recipe tunable methods', {
  pls_info <- tunable(test_recipe)
  expect_true(tibble::is_tibble(pls_info))
  expect_true(nrow(pls_info) > 0)
})

test_that('recipe required_pkgs methods', {
  skip_if(utils::packageVersion("tune") < "0.1.1.9000")
  skip_if(utils::packageVersion("recipes") < "0.1.13.9000")
  pls_pkgs <- required_pkgs(test_recipe, FALSE)

  expect_equal(pls_pkgs, "mixOmics")
})
