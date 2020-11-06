library(extratests)
library(testthat)
library(discrim)
library(tidymodels)
library(modeldata)
library(doParallel)
data(two_class_dat)

# ------------------------------------------------------------------------------

discrim_mod <- discrim_linear() %>%
  set_engine("MASS")
set.seed(123)
folds <- vfold_cv(two_class_dat)
ctrl_extra <- control_resamples(pkgs = "extratests")

test_that('LDA parallel test', {
  skip_if(utils::packageVersion("discrim") <= "0.1.0.9000")

  library(doParallel)
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)
  parallel::clusterEvalQ(cl, library(extratests))

  expect_error(
    res <- fit_resamples(discrim_mod, Class ~ ., folds, control = ctrl_extra),
    regex = NA
  )

  expect_equal(res$.notes[[1]]$.notes, character(0))
  expect_true(all(purrr::map_lgl(res$.notes, ~ nrow(.x) == 0)))
})
