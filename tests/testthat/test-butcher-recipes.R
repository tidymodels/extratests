library(recipes)
library(butcher)
suppressPackageStartupMessages(library(Matrix)) # Waiting for fix in RcppML

# Data sets used for testing
data(biomass)
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

terms_empty_env <- function(axed, step_number) {
  expect_identical(
    attr(axed$steps[[step_number]]$terms[[1]], ".Environment"),
    rlang::base_env()
  )
}

test_that("recipe + step_nnmf_sparse + axe_env() works", {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")
  rec <- recipe(HHV ~ ., data = biomass_tr) %>%
    step_nnmf_sparse(all_numeric_predictors(), num_comp = 2, seed = 473)
  x <- axe_env(rec)
  terms_empty_env(x, 1)
})

test_that("recipe + step_nnmf_sparse + bake() works", {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")
  rec <- recipe(HHV ~ ., data = biomass_tr) %>%
    step_nnmf_sparse(all_numeric_predictors(), num_comp = 2, seed = 473) %>%
    prep()
  x <- butcher(rec)
  expect_equal(bake(rec, biomass_te), bake(x, biomass_te))
})


test_that("recipe + step_ica + axe_env() works", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_ica(Petal.Width, Sepal.Width, num_comp = 2)
  x <- axe_env(rec)
  terms_empty_env(x, 1)
})

test_that("recipe + step_ica + bake() works", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_ica(Petal.Width, Sepal.Width, num_comp = 2) %>%
    prep()
  x <- butcher(rec)
  expect_equal(bake(rec, iris), bake(x, iris))
})
