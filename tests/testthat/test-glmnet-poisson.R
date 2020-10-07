
context("engine - glmnet - Poisson regression")

## -----------------------------------------------------------------------------

library(rlang)
library(poissonreg)
library(tidyr)

# ------------------------------------------------------------------------------

ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) < 0

senior_ind <- model.matrix(~ ., data = seniors)[, -1]
senior_ind <- tibble::as_tibble(senior_ind)

glm_spec <- poisson_reg() %>% set_engine("glm")
glmn_spec <- poisson_reg(penalty = .01, mixture = .3) %>% set_engine("glmnet")
stan_spec <- poisson_reg() %>% set_engine("stan", refresh = 0)
hurdle_spec <- poisson_reg() %>% set_engine("hurdle")
zeroinfl_spec <- poisson_reg() %>% set_engine("zeroinfl")

# ------------------------------------------------------------------------------

test_that('glmnet execution', {
  skip_on_cran()
  skip_if_not_installed("glmnet")

  expect_error(
    res <- fit_xy(
      glmn_spec,
      control = ctrl,
      x = senior_ind[, 1:3],
      y = senior_ind$count
    ),
    regexp = NA
  )

  expect_false(has_multi_predict(res))

  expect_error(
    fit(
      glmn_spec,
      iris_bad_form,
      data = iris,
      control = ctrl
    )
  )

  expect_warning(
    glmnet_xy_catch <- fit_xy(
      glmn_spec,
      x = senior_ind[, 1:3],
      y = factor(senior_ind$count),
      control = caught_ctrl
    )
  )
  expect_true(inherits(glmnet_xy_catch$fit, "try-error"))

})

test_that('glmnet prediction, single lambda', {
  skip_on_cran()
  skip_if_not_installed("glmnet")

  res_xy <- fit_xy(
    glmn_spec,
    control = ctrl,
    x = senior_ind[, 1:3],
    y = senior_ind$count
  )

  uni_pred <- c(538.72595867201, 735.342850038907, 283.116497320853, 386.444515401008,
                92.1247665366651)

  expect_equal(uni_pred, predict(res_xy, senior_ind[1:5, 1:3])$.pred, tolerance = 0.0001)

  res_form <- fit(
    glmn_spec,
    count ~ .,
    data = seniors,
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_form, seniors[1:5, 1:3])$.pred, tolerance = 0.0001)
})


test_that('error traps', {
  skip_on_cran()
  skip_if_not_installed("glmnet")

  expect_error(
    poisson_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ], penalty = 0:1)
  )
  expect_error(
    poisson_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ])
  )

})

