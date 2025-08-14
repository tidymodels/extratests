# theses are needed for all case-weights tests
skip_if_not_installed("parsnip",   "1.0.1")
skip_if_not_installed("hardhat",   "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes",   "1.0.0")

library(multilevelmod)

# linear_reg --------------------------------------------------------------

test_that('linear_reg - stan_glmer case weights', {
  skip_if_not_installed("multilevelmod", "1.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  dat <- make_msa_wts()

  suppressWarnings({
      set.seed(1)
      wt_fit <-
        linear_reg() %>%
        set_engine("stan_glmer") %>%
        fit(value ~ (1|id), data = msa_data, case_weights = dat$wts)
  })

  set.seed(1)
  unwt_fit <-
    linear_reg() %>%
    set_engine("stan_glmer") %>%
    fit(value ~ (1|id), data = msa_data)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})

test_that('linear_reg - lme4::lmer case weights', {
  skip_if_not_installed("multilevelmod", "1.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  dat <- make_msa_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      linear_reg() %>%
      set_engine("lmer") %>%
      fit(value ~ (1|id), data = msa_data, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    linear_reg() %>%
    set_engine("lmer") %>%
    fit(value ~ (1|id), data = msa_data)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit@call))
})


# logistic_reg ------------------------------------------------------------

test_that('logistic_reg - stan_glmer case weights', {
  skip_if_not_installed("multilevelmod", "1.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  data("two_class_dat", package = "modeldata")

  set.seed(1)
  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1/5, 0, 1)
  two_class_dat$id <- rpois(nrow(two_class_dat), 3) + 1
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)

  suppressWarnings({
      wt_fit <-
        logistic_reg() %>%
        set_engine("stan_glmer", seed = 1) %>%
        fit(Class ~ A + B + (1|id), data = two_class_dat, case_weights = wts)
  })

  unwt_fit <-
    logistic_reg() %>%
    set_engine("stan_glmer", seed = 1) %>%
    fit(Class ~ A + B + (1|id), data = two_class_dat)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})

test_that('logistic_reg - lme4::glmer case weights', {
  skip_if_not_installed("multilevelmod", "1.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  data("two_class_dat", package = "modeldata")

  set.seed(1)
  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1/5, 0, 1)
  two_class_dat$id <- round(two_class_dat$A * 3, 0)
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      logistic_reg() %>%
      set_engine("glmer") %>%
      fit(Class ~ A + B + (1|id), data = two_class_dat, case_weights = wts)
  },
  regexp = NA)

  unwt_fit <-
    logistic_reg() %>%
    set_engine("glmer") %>%
    fit(Class ~ A + B + (1|id), data = two_class_dat)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit@call))
})


# poisson_reg -------------------------------------------------------------

test_that('poisson_reg - stan_glmer case weights', {
  skip_if_not_installed("multilevelmod", "1.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  data(bioChemists, package = "pscl", envir = rlang::current_env())

  set.seed(1)
  wts <- runif(nrow(bioChemists))
  wts <- ifelse(wts < 1/5, 0, 1)
  bioChemists$id <- rpois(nrow(bioChemists), 3) + 1
  bioChemists_subset <- bioChemists[wts != 0, ]
  wts <- importance_weights(wts)


  expect_error({
    wt_fit <-
      poisson_reg() %>%
      set_engine("stan_glmer", seed = 1) %>%
      fit(art ~ (1|id), data = bioChemists, case_weights = wts)
  },
  regexp = NA)

  unwt_fit <-
    poisson_reg() %>%
    set_engine("stan_glmer", seed = 1) %>%
    fit(art ~ (1|id), data = bioChemists)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})

test_that('poisson_reg - lme4::glmer case weights', {
  skip_if_not_installed("multilevelmod", "1.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  data(bioChemists, package = "pscl", envir = rlang::current_env())

  set.seed(1)
  wts <- runif(nrow(bioChemists))
  wts <- ifelse(wts < 1/5, 0, 1)
  bioChemists$id <- rpois(nrow(bioChemists), 3) + 1
  bioChemists_subset <- bioChemists[wts != 0, ]
  wts <- importance_weights(wts)


  expect_error({
    set.seed(1)
    wt_fit <-
      poisson_reg() %>%
      set_engine("glmer") %>%
      fit(art ~ (1|id), data = bioChemists, case_weights = wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    poisson_reg() %>%
    set_engine("glmer") %>%
    fit(art ~ (1|id), data = bioChemists)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit@call))
})
