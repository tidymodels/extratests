# theses are needed for all case-weights tests
skip_if_not_installed("parsnip", "1.0.1")
skip_if_not_installed("hardhat", "1.2.0")
skip_if_not_installed("yardstick", "1.0.0")
skip_if_not_installed("workflows", "1.0.0")
skip_if_not_installed("recipes", "1.0.0")

library(baguette)

# bagged trees ------------------------------------------------------------

test_that('bag_tree - rpart case weights', {
  skip_if_not_installed("baguette", "1.0.0")

  dat <- make_two_class_wts()

  expect_error(
    {
      set.seed(1)
      wt_fit <-
        bag_tree() %>%
        set_engine("rpart") %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = dat$wts)
    },
    regexp = NA
  )

  set.seed(1)
  unwt_fit <-
    bag_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_unequal(unwt_fit$fit$imp, wt_fit$fit$imp)
})

test_that('bag_tree - C50 case weights', {
  skip_if_not_installed("baguette", "1.0.0")

  data("two_class_dat", package = "modeldata")
  wts <- order(-two_class_dat$B)
  wts <- importance_weights(wts)

  expect_error(
    {
      set.seed(1)
      wt_fit <-
        bag_tree() %>%
        set_engine("C5.0") %>%
        set_mode("classification") %>%
        fit(Class ~ ., data = two_class_dat, case_weights = wts)
    },
    regexp = NA
  )

  set.seed(1)
  unwt_fit <-
    bag_tree() %>%
    set_engine("C5.0") %>%
    set_mode("classification") %>%
    fit(Class ~ ., data = two_class_dat)

  expect_true(wt_fit$fit$model_df$model[[1]]$fit$caseWeights)
  expect_unequal(unwt_fit$fit$imp, wt_fit$fit$imp)
})


# bagged mars -------------------------------------------------------------

test_that('bag_mars - earth case weights', {
  skip_if_not_installed("baguette", "1.0.0")

  suppressPackageStartupMessages(library(earth))

  dat <- make_ames_wts()

  expect_error(
    {
      set.seed(1)
      wt_fit <-
        bag_mars() %>%
        set_engine("earth", times = 2) %>%
        set_mode("regression") %>%
        fit(
          Sale_Price ~ .,
          data = dat$full[1:100, ],
          case_weights = dat$wts[1:100]
        )
    },
    regexp = NA
  )

  set.seed(1)
  unwt_fit <-
    bag_mars() %>%
    set_engine("earth", times = 2) %>%
    set_mode("regression") %>%
    fit(Sale_Price ~ ., data = dat$full[1:100, ])

  expect_unequal(unwt_fit$fit$imp, wt_fit$fit$imp)
})
