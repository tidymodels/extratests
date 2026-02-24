# theses are needed for all case-weights tests
skip_if_not_installed("parsnip", "1.0.1")
skip_if_not_installed("hardhat", "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes", "1.0.0")

suppressPackageStartupMessages(library(rules))

# C5_rules ----------------------------------------------------------------

test_that('C5_rules - C50 case weights', {
  skip_if_not_installed("rules", "1.0.0")

  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error(
    {
      wt_fit <-
        C5_rules(trees = 5) %>%
        set_engine("C5.0") %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = wts)
    },
    regexp = NA
  )

  unwt_fit <-
    C5_rules(trees = 5) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$caseWeights)
  expect_unequal(unwt_fit$fit$rules, wt_fit$fit$rules)
})


# cubist_rules ------------------------------------------------------------

test_that('cubist case weights', {
  skip_if_not_installed("rules", "1.0.0")

  dat <- make_ames_wts()

  expect_error(
    wt_fit <-
      cubist_rules() %>%
      fit(
        Sale_Price ~ Longitude + Latitude,
        data = dat$full,
        case_weights = dat$wts
      ),
    regexp = NA
  )

  unwt_fit <-
    cubist_rules() %>%
    fit(Sale_Price ~ Longitude + Latitude, data = dat$full)

  expect_unequal(wt_fit$fit$model, unwt_fit$fit$model)
  expect_true(wt_fit$fit$caseWeights)
})
