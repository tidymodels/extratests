# theses are needed for all case-weights tests
skip_if_not_installed("parsnip",   "1.0.1")
skip_if_not_installed("hardhat",   "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes",   "1.0.0")

suppressPackageStartupMessages(library(censored))

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


# proportional_hazards ----------------------------------------------------

test_that('proportional_hazards - survival censored case weights', {
  skip_if_not_installed("censored", "0.1.0")

  # survival engine can only take weights > 0
  data(time_to_million, package = "censored", envir = rlang::current_env())

  set.seed(1)
  dat <- time_to_million[1:100, c("time", "event", "released_theaters", "rated")]
  wts <- runif(nrow(dat))
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      proportional_hazards() %>%
      set_engine("survival") %>%
      set_mode("censored regression") %>%
      fit(Surv(time, event) ~ ., data = dat, case_weights = wts)
  },
  regexp = NA)

  expect_equal(wt_fit$fit$weights, as.vector(wts))
})

test_that('proportional_hazards - glmnet censored case weights', {
  skip_if_not_installed("censored", "0.1.1.9001")

  dat <- make_cens_wts()

  expect_error({
    wt_fit <-
      proportional_hazards(penalty = 0.1) %>%
      set_engine("glmnet") %>%
      set_mode("censored regression") %>%
      fit(Surv(time, event) ~ ., data = dat$full, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    proportional_hazards(penalty = 0.1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression") %>%
    fit(Surv(time, event) ~ ., data = dat$full)

  expect_snapshot(wt_fit$fit$fit$call)
  expect_unequal(coef(unwt_fit$fit$fit), coef(wt_fit$fit$fit))
})
