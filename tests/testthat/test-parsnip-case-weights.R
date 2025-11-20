# theses are needed for all case-weights tests
skip_if_not_installed("parsnip", "1.0.1")
skip_if_not_installed("hardhat", minimum_version = "1.2.0")
skip_if_not_installed("yardstick", minimum_version = "1.0.0")
skip_if_not_installed("workflows", minimum_version = "1.0.0")
skip_if_not_installed("recipes", minimum_version = "1.0.0")
skip_if_not_installed("sparklyr", minimum_version = "1.9.1.9000")

# load all extension packages to register the engines
library(parsnip)
suppressPackageStartupMessages(library(sparklyr))

# boosted trees -----------------------------------------------------------

test_that('boost_tree - xgboost case weights', {
  dat <- make_two_class_wts()

  expect_error(
    {
      sink(file = tempfile())
      set.seed(1)
      wt_fit <-
        boost_tree() %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
      sink()
    },
    regexp = NA
  )

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

  expect_error(
    {
      wt_fit <-
        boost_tree(trees = 5) %>%
        set_engine("C5.0") %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = wts)
    },
    regexp = NA
  )

  unwt_fit <-
    boost_tree(trees = 5) %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$caseWeights)
  expect_unequal(unwt_fit$fit$tree, wt_fit$fit$tree)
})


# decision_tree -----------------------------------------------------------

test_that('decision_tree - rpart case weights', {
  dat <- make_two_class_wts()

  expect_error(
    {
      wt_fit <-
        decision_tree() %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    decision_tree() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(
    unwt_fit$fit$variable.importance,
    wt_fit$fit$variable.importance
  )
})

test_that('decision_tree - C50 case weights', {
  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error(
    {
      wt_fit <-
        decision_tree() %>%
        set_engine("C5.0") %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = wts)
    },
    regexp = NA
  )

  unwt_fit <-
    decision_tree() %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$caseWeights)
  expect_unequal(unwt_fit$fit$tree, wt_fit$fit$tree)
})


# linear_reg --------------------------------------------------------------

test_that('linear_reg - lm case weights', {
  dat <- make_mtcars_wts()

  expect_error(
    {
      wt_fit <-
        linear_reg() %>%
        fit(mpg ~ ., data = mtcars, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    linear_reg() %>%
    fit(mpg ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('linear_reg - glm case weights', {
  dat <- make_mtcars_wts()

  expect_error(
    {
      wt_fit <-
        linear_reg() %>%
        set_engine("glm") %>%
        fit(mpg ~ ., data = mtcars, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    linear_reg() %>%
    set_engine("glm") %>%
    fit(mpg ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('linear_reg - glmnet case weights', {
  dat <- make_ames_wts()

  expect_error(
    {
      wt_fit <-
        linear_reg(penalty = 0.001) %>%
        set_engine("glmnet", path_values = 10^(-4:-1)) %>%
        fit(Sale_Price ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    linear_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(Sale_Price ~ ., data = dat$subset)

  expect_equal(sub_fit$fit$beta, wt_fit$fit$beta)
})

test_that('linear_reg - spark case weights', {
  skip_if_not_installed("sparklyr")

  sc <- try(spark_test_connection(), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  dat <- make_mtcars_wts()

  mtcars_wts <- copy_to(
    sc,
    mtcars %>% mutate(wts = as.double(dat$wts)),
    "dat_wts",
    overwrite = TRUE
  )

  mtcars_subset <- copy_to(
    sc,
    dat$subset,
    "mtcars_subset",
    overwrite = TRUE
  )

  expect_error(
    {
      wt_fit <-
        linear_reg() %>%
        set_engine("spark") %>%
        fit(
          mpg ~ . - wts,
          data = mtcars_wts,
          case_weights = "wts"
        )
    },
    regexp = NA
  )

  sub_fit <-
    linear_reg() %>%
    set_engine("spark") %>%
    fit(mpg ~ ., data = mtcars_subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))

})


# logistic_reg ------------------------------------------------------------

test_that('logistic_reg - glm case weights', {
  dat <- make_two_class_wts()

  expect_error(
    {
      wt_fit <-
        logistic_reg() %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    logistic_reg() %>%
    fit(Class ~ ., data = dat$subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))
})

test_that('logistic_reg - glmnet case weights', {
  dat <- make_two_class_wts()

  expect_error(
    {
      wt_fit <-
        logistic_reg(penalty = 0.001) %>%
        set_engine("glmnet", path_values = 10^(-4:-1)) %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    logistic_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(Class ~ ., data = dat$subset)

  expect_equal(sub_fit$fit$beta, wt_fit$fit$beta)
})

test_that('logistic_reg - stan case weights', {
  dat <- make_two_class_wts()

  expect_error(
    {
      wt_fit <-
        logistic_reg() %>%
        set_engine("stan", seed = 1) %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    logistic_reg() %>%
    set_engine("stan", seed = 1) %>%
    fit(Class ~ ., data = two_class_dat)

  expect_unequal(coef(unwt_fit$fit), coef(wt_fit$fit))
  expect_snapshot(print(wt_fit$fit$call))
})

test_that('logistic_reg - spark case weights', {
  skip_if_not_installed("sparklyr")

  sc <- try(spark_test_connection(), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  dat <- make_two_class_wts()

  two_class_dat_wts <- copy_to(
    sc,
    two_class_dat %>% mutate(wts = as.double(dat$wts)),
    "two_class_dat_wts",
    overwrite = TRUE
  )

  dat_subset <- copy_to(
    sc,
    dat$subset,
    "dat_subset",
    overwrite = TRUE
  )

  expect_error(
    {
      wt_fit <-
        logistic_reg() %>%
        set_engine("spark") %>%
        fit(
          Class ~ . - wts,
          data = two_class_dat_wts,
          case_weights = "wts"
        )
    },
    regexp = NA
  )

  sub_fit <-
    logistic_reg() %>%
    set_engine("spark") %>%
    fit(Class ~ ., data = dat_subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))

})


# mars --------------------------------------------------------------------

test_that('mars - earth case weights', {
  suppressPackageStartupMessages(library(earth))

  dat <- make_two_class_wts()

  expect_error(
    {
      wt_fit <-
        mars() %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  unwt_fit <-
    mars() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_snapshot(print(wt_fit$fit$call))
  expect_unequal(unwt_fit$fit$fitted.values[, 1], wt_fit$fit$fitted.values[, 1])
})


# mlp ---------------------------------------------------------------------

test_that('mlp - nnet case weights', {
  dat <- make_two_class_wts()

  expect_snapshot_error(
    mlp() %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
  )
})


# multinom_reg ------------------------------------------------------------

test_that('multinom_reg - glmnet case weights', {
  dat <- make_penguin_wts()

  expect_error(
    {
      wt_fit <-
        multinom_reg(penalty = 0.001) %>%
        set_engine("glmnet", path_values = 10^(-4:-1)) %>%
        fit(island ~ ., data = penguins, case_weights = dat$wts)
    },
    regexp = NA
  )

  sub_fit <-
    multinom_reg(penalty = 0.001) %>%
    set_engine("glmnet", path_values = 10^(-4:-1)) %>%
    fit(island ~ ., data = dat$subset)

  expect_equal(sub_fit$fit$beta, wt_fit$fit$beta)
})

test_that('multinom_reg - spark case weights', {
  skip_if_not_installed("sparklyr")

  sc <- try(spark_test_connection(), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  dat <- make_penguin_wts()

  penguin_wts <- copy_to(
    sc,
    penguins[complete.cases(penguins), ] %>% mutate(wts = as.double(dat$wts)),
    "penguin_wts",
    overwrite = TRUE
  )

  penguin_subset <- copy_to(
    sc,
    dat$subset,
    "penguin_subset",
    overwrite = TRUE
  )

  expect_error(
    {
      wt_fit <-
        multinom_reg() %>%
        set_engine("spark") %>%
        fit(
          island ~ . - wts,
          data = penguin_wts,
          case_weights = "wts"
        )
    },
    regexp = NA
  )

  sub_fit <-
    multinom_reg() %>%
    set_engine("spark") %>%
    fit(island ~ ., data = penguin_subset)

  expect_equal(coef(sub_fit$fit), coef(wt_fit$fit))

})


# rand_forest -------------------------------------------------------------

test_that('rand_forest - ranger case weights', {
  dat <- make_two_class_wts()

  expect_error(
    {
      set.seed(1)
      wt_fit <-
        rand_forest() %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  set.seed(1)
  unwt_fit <-
    rand_forest() %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_unequal(unwt_fit$fit$predictions, wt_fit$fit$predictions)

  expect_snapshot(print(wt_fit$fit$call))
})
