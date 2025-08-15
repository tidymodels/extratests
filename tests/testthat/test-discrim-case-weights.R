# theses are needed for all case-weights tests
skip_if_not_installed("parsnip", "1.0.1")
skip_if_not_installed("hardhat", "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes", "1.0.0")

# load all extension packages to register the engines
suppressPackageStartupMessages(library(discrim))

# discrim_flexible --------------------------------------------------------

test_that('discrim_flexible - earth case weights', {
  skip_if_not_installed("discrim", "1.0.0")

  dat <- make_two_class_wts()

  expect_error(
    {
      wt_fit <-
        discrim_flexible(prune_method = "none") %>%
        set_engine("earth") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    discrim_flexible(prune_method = "none") %>%
    set_engine("earth") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(unwt_fit$fit$fit$coefficients, wt_fit$fit$fit$coefficients)
})


# discrim_linear ----------------------------------------------------------

test_that('LDA - sda case weights', {
  skip_if_not_installed("discrim", "1.0.0")

  dat <- make_two_class_wts()

  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error(
    {
      wt_fit <-
        discrim_linear(penalty = 0.0001) %>%
        set_engine("mda") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    discrim_linear(penalty = 0.0001) %>%
    set_engine("mda") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(wt_fit$fit$call)
  expect_unequal(unwt_fit$fit$fit, wt_fit$fit$fit)
})
