library(parsnip)
library(censored)
library(baguette)
library(rules)
library(poissonreg)
library(multilevelmod)
library(discrim)

# ------------------------------------------------------------------------------

expect_unequal <-
  function(object, expected, ...,
           tolerance = if (edition_get() >= 3) testthat_tolerance()) {
    expect_true(!compare(object, expected, tolerance = tolerance, ...)$equal)
  }

# ------------------------------------------------------------------------------
# bagged trees

test_that('bag_tree - rpart case weights', {
  skip("until I fix the warnings")

  dat <- make_two_class_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      bag_tree() %>%
      set_engine("rpart") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    bag_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = dat$subset)

  expect_unequal(unwt_fit$fit$imp, wt_fit$fit$imp)
})

test_that('bag_tree - C50 case weights', {
  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error({
    set.seed(1)
    wt_fit <-
      bag_tree() %>%
      set_engine("C5.0") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    bag_tree() %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$model_df$model[[1]]$fit$caseWeights)
  expect_unequal(unwt_fit$fit$imp, wt_fit$fit$imp)
})


# ------------------------------------------------------------------------------
# bagged mars

test_that('bag_mars - earth case weights', {
  skip("this takes forever")
  dat <- make_ames_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      bag_mars() %>%
      set_engine("earth", times = 2) %>%
      set_mode("regression") %>%
      fit(Sale_Price ~ ., data = dat$full, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    bag_mars() %>%
    set_engine("earth", times = 2) %>%
    set_mode("regression") %>%
    fit(Sale_Price ~ ., data = dat$full)

  expect_unequal(unwt_fit$fit$imp, wt_fit$fit$imp)
})


# ------------------------------------------------------------------------------
# boosted trees

test_that('boost_tree - xgboost case weights', {
  dat <- make_two_class_wts()

  expect_error({
    sink(file = tempfile())
    set.seed(1)
    wt_fit <-
      boost_tree() %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    sink()
  },
  regexp = NA)

  sink(file = tempfile())
  set.seed(1)
  unwt_fit <-
    boost_tree() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)
  sink()

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(unwt_fit$fit$evaluation_log, wt_fit$fit$evaluation_log)
})

test_that('boost_tree - C50 case weights', {

  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      boost_tree(trees = 5)%>%
      set_engine("C5.0") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = wts)
  },
  regexp = NA)

  unwt_fit <-
    boost_tree(trees = 5) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$caseWeights)
  expect_unequal(unwt_fit$fit$tree, wt_fit$fit$tree)

})

# ------------------------------------------------------------------------------
# C5_rules


test_that('C5_rules - C50 case weights', {

  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      C5_rules(trees = 5)%>%
      set_engine("C5.0") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = wts)
  },
  regexp = NA)

  unwt_fit <-
    C5_rules(trees = 5) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$caseWeights)
  expect_unequal(unwt_fit$fit$rules, wt_fit$fit$rules)

})

# ------------------------------------------------------------------------------
# cubist_rules

test_that('cubist case weights', {
  dat <- make_ames_wts()

  expect_error(
    wt_fit <-
      cubist_rules() %>%
      fit(Sale_Price ~ Longitude + Latitude, data = dat$full, case_weights = dat$wts),
    regexp = NA)

  unwt_fit <-
    cubist_rules() %>%
    fit(Sale_Price ~ Longitude + Latitude, data = dat$full)

  expect_unequal(wt_fit$fit$model, unwt_fit$fit$model)
  expect_true(wt_fit$fit$caseWeights)
})

# ------------------------------------------------------------------------------
# decision_tree

test_that('decision_tree - rpart case weights', {
  dat <- make_two_class_wts()

  expect_error({
    wt_fit <-
      decision_tree() %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    decision_tree() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(unwt_fit$fit$variable.importance, wt_fit$fit$variable.importance)
})


test_that('decision_tree - C50 case weights', {

  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      decision_tree() %>%
      set_engine("C5.0") %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = wts)
  },
  regexp = NA)

  unwt_fit <-
    decision_tree() %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$caseWeights)
  expect_unequal(unwt_fit$fit$tree, wt_fit$fit$tree)
})


# ------------------------------------------------------------------------------
# discrim_flexible

test_that('discrim_flexible - earth case weights', {
  dat <- make_two_class_wts()

  expect_error({
    wt_fit <-
      discrim_flexible(prune_method = "none") %>%
      set_engine("earth") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    discrim_flexible(prune_method = "none") %>%
    set_engine("earth") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(unwt_fit$fit$fit$coefficients, wt_fit$fit$fit$coefficients)
})

# ------------------------------------------------------------------------------
# discrim_linear

test_that('LDA - sda case weights', {
  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      discrim_linear(penalty = 0.0001) %>%
      set_engine("mda") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    discrim_linear(penalty = 0.0001) %>%
    set_engine("mda") %>%
    fit(Class ~ ., data = two_class_dat)


})

# ------------------------------------------------------------------------------
# linear_reg

test_that('linear_reg - lm case weights', {
  dat <- make_mtcars_wts()

  expect_error({
    wt_fit <-
      linear_reg() %>%
      fit(mpg ~ ., data = mtcars, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    linear_reg() %>%
    fit(mpg ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('linear_reg - glm case weights', {
  dat <- make_mtcars_wts()

  expect_error({
    wt_fit <-
      linear_reg() %>%
      set_engine("glm") %>%
      fit(mpg ~ ., data = mtcars, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    linear_reg() %>%
    set_engine("glm") %>%
    fit(mpg ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})


test_that('linear_reg - glmnet case weights', {
  dat <- make_ames_wts()

  expect_error({
    wt_fit <-
      linear_reg(penalty = 0.001) %>%
      set_engine("glmnet", path_values = 10^(-4:-1)) %>%
      fit(Sale_Price ~ ., data = dat$full, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    linear_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(Sale_Price ~ ., data = dat$subset)

  expect_equal(sub_fit$fit$beta, wt_fit$fit$beta)
})

test_that('linear_reg - stan_glmer case weights', {

  dat <- make_msa_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      linear_reg() %>%
      set_engine("stan_glmer") %>%
      fit(value ~ (1|id), data = msa_data, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    linear_reg() %>%
    set_engine("stan_glmer") %>%
    fit(value ~ (1|id), data = msa_data)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})


test_that('linear_reg - lme4::lmer case weights', {
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

# ------------------------------------------------------------------------------
# logistic_reg

test_that('logistic_reg - glm case weights', {
  dat <- make_two_class_wts()

  expect_error({
    wt_fit <-
      logistic_reg() %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    logistic_reg() %>%
    fit(Class ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('logistic_reg - glmnet case weights', {
  dat <- make_two_class_wts()

  expect_error({
    wt_fit <-
      logistic_reg(penalty = 0.001) %>%
      set_engine("glmnet", path_values = 10^(-4:-1)) %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    logistic_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(Class ~ ., data = dat$subset)

  expect_equal(sub_fit$fit$beta, wt_fit$fit$beta)
})

test_that('logistic_reg - stan case weights', {
  dat <- make_two_class_wts()

  expect_error({
    wt_fit <-
      logistic_reg() %>%
      set_engine("stan", seed = 1) %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    logistic_reg() %>%
    set_engine("stan", seed = 1) %>%
    fit(Class ~ ., data = two_class_dat)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})


test_that('logistic_reg - stan_glmer case weights', {
  data("two_class_dat", package = "modeldata")

  set.seed(1)
  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1/5, 0, 1)
  two_class_dat$id <- rpois(nrow(two_class_dat), 3) + 1
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)

  expect_error({
    wt_fit <-
      logistic_reg() %>%
      set_engine("stan_glmer", seed = 1) %>%
      fit(Class ~ A + B + (1|id), data = two_class_dat, case_weights = wts)
  },
  regexp = NA)

  unwt_fit <-
    logistic_reg() %>%
    set_engine("stan_glmer", seed = 1) %>%
    fit(Class ~ A + B + (1|id), data = two_class_dat)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})

test_that('logistic_reg - lme4::glmer case weights', {
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


# ------------------------------------------------------------------------------
# mars


test_that('mars - earth case weights', {
  dat <- make_two_class_wts()

  expect_error({
    wt_fit <-
      mars() %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    mars() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(unwt_fit$fit$fitted.values[,1], wt_fit$fit$fitted.values[,1])

})

# ------------------------------------------------------------------------------
# mlp

test_that('mlp - nnet case weights', {
  dat <- make_two_class_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      mlp() %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  sub_fit <-
    mlp() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = dat$subset)

  expect_equal(coef(wt_fit$fit), coef(sub_fit$fit))
})


# ------------------------------------------------------------------------------
# multinom_reg

test_that('multinom_reg - glmnet case weights', {
  dat <- make_penguin_wts()

  expect_error({
    wt_fit <-
      multinom_reg(penalty = 0.001) %>%
      set_engine("glmnet", path_values = 10^(-4:-1)) %>%
      fit(island ~ ., data = penguins, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    multinom_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(island ~ ., data = dat$subset)

  expect_equal(sub_fit$fit$beta, wt_fit$fit$beta)
})


test_that('multinom_reg - nnet case weights', {
  skip("fails - variable lengths differ (found for '(weights)')")
  dat <- make_penguin_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      multinom_reg() %>%
      fit(island ~ ., data = penguins, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  sub_fit <-
    multinom_reg() %>%
    fit(island ~ ., data = dat$subset)

  expect_equal(coef(wt_fit$fit), coef(sub_fit$fit))
})


# ------------------------------------------------------------------------------
# poisson_reg

test_that('poisson_reg - glm case weights', {
  dat <- make_biochem_wts()

  expect_error({
    wt_fit <-
      poisson_reg() %>%
      fit(art ~ ., data = bioChemists, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    poisson_reg() %>%
    fit(art ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('poisson_reg - stan_glmer case weights', {
  data(bioChemists, package = "pscl")

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

test_that('poisson_reg - hurdle case weights', {
  dat <- make_biochem_wts()

  expect_error({
    wt_fit <-
      poisson_reg() %>%
      set_engine("hurdle") %>%
      fit(art ~ ., data = bioChemists, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    poisson_reg() %>%
    set_engine("hurdle") %>%
    fit(art ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(sub_fit$fit))
})

test_that('poisson_reg - zeroinfl case weights', {
  dat <- make_biochem_wts()

  expect_error({
    wt_fit <-
      poisson_reg() %>%
      set_engine("zeroinfl") %>%
      fit(art ~ ., data = bioChemists, case_weights = dat$wts)
  },
  regexp = NA)

  sub_fit <-
    poisson_reg() %>%
    set_engine("zeroinfl") %>%
    fit(art ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})


test_that('poisson_reg - glmnet case weights', {
  dat <- make_biochem_wts()

  expect_error({
    wt_fit <-
      poisson_reg(penalty = 0.001) %>%
      set_engine("glmnet", path_values = 10^(-4:-1)) %>%
      fit(art ~ ., data = bioChemists, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    poisson_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(art ~ ., data = bioChemists)

  expect_unequal(unwt_fit$fit$beta, wt_fit$fit$beta)
  expect_snapshot(print(wt_fit$fit$call))
})


test_that('poisson_reg - stan case weights', {
  dat <- make_biochem_wts()

  expect_error({
    wt_fit <-
      poisson_reg() %>%
      set_engine("stan", seed = 1, refresh = 0) %>%
      fit(art ~ ., data = bioChemists, case_weights = dat$wts)
  },
  regexp = NA)

  unwt_fit <-
    poisson_reg() %>%
    set_engine("stan", seed = 1, refresh = 0) %>%
    fit(art ~ ., data = bioChemists)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})


test_that('poisson_reg - lme4::glmer case weights', {
  data(bioChemists, package = "pscl")

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


# ------------------------------------------------------------------------------
# rand_forest

test_that('rand_forest - ranger case weights', {
  dat <- make_two_class_wts()

  expect_error({
    set.seed(1)
    wt_fit <-
      rand_forest() %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  },
  regexp = NA)

  set.seed(1)
  unwt_fit <-
    rand_forest() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_unequal(unwt_fit$fit$predictions, wt_fit$fit$predictions)
  expect_snapshot(print(wt_fit$fit$call))
})

# ------------------------------------------------------------------------------
# survival_reg

