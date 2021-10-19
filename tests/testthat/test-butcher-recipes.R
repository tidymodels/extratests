library(recipes)
library(butcher)

# Data sets used for testing
data(biomass)
biomass_tr <- biomass[biomass$dataset == "Training",]

terms_empty_env <- function(axed, step_number) {
  expect_identical(attr(axed$steps[[step_number]]$terms[[1]], ".Environment"),
                   rlang::base_env())
}

test_that("recipe + step_nnmf + axe_env() works", {
  skip_if_not_installed("NMF")
  rec <- recipe(HHV ~ ., data = biomass_tr) %>%
    step_nnmf(all_predictors(), num_comp = 2, seed = 473, num_run = 2)
  x <- axe_env(rec)
  terms_empty_env(x, 1)
})


test_that("recipe + step_ica + axe_env() works", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_ica(Petal.Width, Sepal.Width, num_comp = 2)
  x <- axe_env(rec)
  terms_empty_env(x, 1)
})
