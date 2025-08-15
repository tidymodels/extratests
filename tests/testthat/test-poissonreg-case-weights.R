# theses are needed for all case-weights tests
skip_if_not_installed("parsnip", "1.0.1")
skip_if_not_installed("hardhat", "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes", "1.0.0")

library(poissonreg)

# poisson_reg -------------------------------------------------------------

test_that('poisson_reg - glm case weights', {
  skip_if_not_installed("poissonreg", "1.0.1")

  dat <- make_biochem_wts()

  expect_error(
    {
      wt_fit <-
        poisson_reg() %>%
        fit(art ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    poisson_reg() %>%
    fit(art ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('poisson_reg - hurdle case weights', {
  skip_if_not_installed("poissonreg", "1.0.1")

  dat <- make_biochem_wts()

  expect_error(
    {
      wt_fit <-
        poisson_reg() %>%
        set_engine("hurdle") %>%
        fit(art ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    poisson_reg() %>%
    set_engine("hurdle") %>%
    fit(art ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(sub_fit$fit))
})

test_that('poisson_reg - zeroinfl case weights', {
  skip_if_not_installed("poissonreg", "1.0.1")

  dat <- make_biochem_wts()

  expect_error(
    {
      wt_fit <-
        poisson_reg() %>%
        set_engine("zeroinfl") %>%
        fit(art ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    poisson_reg() %>%
    set_engine("zeroinfl") %>%
    fit(art ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('poisson_reg - glmnet case weights', {
  skip_if_not_installed("poissonreg", "1.0.1")

  dat <- make_biochem_wts()

  expect_error(
    {
      wt_fit <-
        poisson_reg(penalty = 0.001) %>%
        set_engine("glmnet", path_values = 10^(-4:-1)) %>%
        fit(art ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    poisson_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(art ~ ., data = dat$full)

  expect_unequal(unwt_fit$fit$beta, wt_fit$fit$beta)
  skip_if_not_installed("parsnip", "1.0.4.9000")
  expect_snapshot(print(wt_fit$fit$call))
})

test_that('poisson_reg - stan case weights', {
  skip_if_not_installed("poissonreg", "1.0.1")

  dat <- make_biochem_wts()

  expect_error(
    {
      wt_fit <-
        poisson_reg() %>%
        set_engine("stan", seed = 1, refresh = 0) %>%
        fit(art ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    poisson_reg() %>%
    set_engine("stan", seed = 1, refresh = 0) %>%
    fit(art ~ ., data = dat$full)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})
