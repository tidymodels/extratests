library(testthat)
library(parsnip)
library(modeldata)

## -----------------------------------------------------------------------------

ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

## -----------------------------------------------------------------------------

data("hpc_data")
hpc <- hpc_data[, c(2:5, 8)]

# ------------------------------------------------------------------------------


num_pred <- c("compounds", "iterations", "num_pending")
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- linear_reg() %>%
  set_engine("stan", seed = 10, chains = 1)

ctrl <- control_parsnip(verbosity = 0L, catch = FALSE)
caught_ctrl <- control_parsnip(verbosity = 0L, catch = TRUE)
quiet_ctrl <- control_parsnip(verbosity = 0L, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('stan_glm execution', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  expect_error(
    res <- fit(
      hpc_basic,
      compounds ~ log(input_fields) + class,
      data = hpc,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$input_fields,
      control = ctrl
    ),
    regexp = NA
  )

  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_error(
    res <- fit(
      hpc_basic,
      class ~ term,
      data = hpc,
      control = ctrl
    )
  )

})


test_that('stan prediction', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  uni_pred <- c(1691.46306020449, 1494.27323520418, 1522.36011539284, 1493.39683598195,
                1494.93053462084)
  inl_pred <- c(429.164145548939, 256.32488428038, 254.949927688403, 255.007333947447,
                255.336665165556)

  res_xy <- fit_xy(
    linear_reg() %>%
      set_engine("stan", seed = 10, chains = 1),
    x = hpc[, num_pred],
    y = hpc$input_fields,
    control = quiet_ctrl
  )

  set.seed(383)
  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred, tolerance = 0.1)

  res_form <- fit(
    hpc_basic,
    compounds ~ log(input_fields) + class,
    data = hpc,
    control = quiet_ctrl
  )
  expect_equal(inl_pred, predict(res_form, hpc[1:5, ])$.pred, tolerance = 0.1)
})


test_that('stan intervals', {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  res_xy <- fit_xy(
    linear_reg() %>%
      set_engine("stan", seed = 1333, chains = 10, iter = 5000),
    x = hpc[, num_pred],
    y = hpc$input_fields,
    control = quiet_ctrl
  )

  set.seed(1231)
  confidence_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "conf_int",
            level = 0.93)

  set.seed(1231)
  prediction_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "pred_int",
            level = 0.93)

  test <-
    rstanarm::posterior_epred(res_xy$fit, hpc[1:5,]) %>%
    apply(2, quantile, prob = (1 - .93)/2) %>%
    unname()

  ci_lower <- c(1577.25718753727, 1382.58210286254, 1399.96490471468, 1381.56774986889,
                1383.25519963864)
  ci_upper <- c(1809.28331613624, 1609.11912475981, 1646.44852457781, 1608.3327281785,
                1609.4796390366)

  pi_lower <- rstanarm::posterior_predict(res_xy$fit, hpc[1:5,], seed = 1198) %>%
    apply(2, quantile, prob = 0.035) %>%
    unname()
  pi_upper <- rstanarm::posterior_predict(res_xy$fit, hpc[1:5,], seed = 1198) %>%
    apply(2, quantile, prob = 0.965) %>%
    unname()

  expect_equal(confidence_parsnip$.pred_lower, ci_lower, ignore_attr = TRUE, tolerance = 1e-2)
  expect_equal(confidence_parsnip$.pred_upper, ci_upper, ignore_attr = TRUE, tolerance = 1e-2)

  expect_equal(prediction_parsnip$.pred_lower,
               pi_lower,
               ignore_attr = TRUE,
               tolerance = .1)
  expect_equal(prediction_parsnip$.pred_upper,
               pi_upper,
               ignore_attr = TRUE,
               tolerance = .1)
})



