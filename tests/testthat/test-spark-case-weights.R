skip_if_no_spark()

test_that("linear_reg - spark case weights", {
  sc <- spark_test_connection()
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

test_that("logistic_reg - spark case weights", {
  sc <- spark_test_connection()

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

test_that("multinom_reg - spark case weights", {
  sc <- spark_test_connection()

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
