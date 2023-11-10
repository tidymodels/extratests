# theses are needed for all case-weights tests
skip_if_not_installed("parsnip",   "1.0.1")
skip_if_not_installed("hardhat",   "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes",   "1.0.0")

library(censored)

# bagged trees ------------------------------------------------------------

test_that('bag_tree - rpart censored case weights', {
  skip_if_not_installed("censored", "0.1.0")

  dat <- make_cens_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      bag_tree() %>%
      set_engine("rpart") %>%
      set_mode("censored regression") %>%
      fit(Surv(time, event) ~ ., data = dat$full, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    bag_tree() %>%
    set_engine("rpart") %>%
    set_mode("censored regression") %>%
    fit(Surv(time, event) ~ ., data = dat$full)

  # the resulting `$mtrees` objects are the same but
  # weights is included in the call
  expect_snapshot(wt_fit$fit$call)
})

# boosted trees -----------------------------------------------------------

test_that("boost_tree - mboost censored case weights", {
  skip_if_not_installed("censored", "0.1.0")

  dat <- make_cens_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      boost_tree() %>%
      set_engine("mboost") %>%
      set_mode("censored regression") %>%
      fit(Surv(time, event) ~ ., data = dat$full, case_weights = dat$wts)
  },
  regexp = NA)

  expect_equal(wt_fit$fit$`(weights)`, as.vector(dat$wts))
})
