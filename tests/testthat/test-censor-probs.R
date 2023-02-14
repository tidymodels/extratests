
test_that("reverse Kaplan-Meier curves", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("survival")
  skip_if_not_installed("censored")
  skip_if_not_installed("parsnip", minimum_version = "1.0.3.9001")

  library(survival)
  library(censored)
  library(prodlim)

  lung <- lung[complete.cases(lung), ]

  mod_fit <-
    survival_reg() %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_true(any(names(mod_fit) == "censor_probs"))
  expect_s3_class(
    mod_fit$censor_probs,
    c("censoring_model_reverse_km", "censoring_model")
  )
  expect_equal(
    names(mod_fit$censor_probs),
    c("formula", "fit", "label", "required_pkgs")
  )
  expect_true(inherits(mod_fit$censor_probs$fit, "prodlim"))
  expect_equal(
    mod_fit$censor_probs$formula,
    Surv(time, status) ~ age + sex,
    ignore_formula_env = TRUE
  )
  expect_equal(
    mod_fit$censor_probs$fit$formula,
    Surv(time, status) ~ 1,
    ignore_formula_env = TRUE
  )
  expect_equal(
    environment(mod_fit$censor_probs$formula),
    rlang::base_env()
  )
  expect_equal(
    environment(mod_fit$censor_probs$fit$formula),
    rlang::base_env()
  )
  expect_equal(mod_fit$censor_probs$label, "reverse_km")
  expect_equal(mod_fit$censor_probs$required_pkgs, "prodlim")
  expect_snapshot(print(mod_fit$censor_probs))

  # ----------------------------------------------------------------------------
  # prediction

  pred_times <- (7:10) * 100
  parsnip_df_pred <- predict(mod_fit$censor_probs, time = pred_times)
  parsnip_vec_pred <-
    predict(mod_fit$censor_probs, time = pred_times, as_vector = TRUE)
  pl_pred <-
    predict(mod_fit$censor_probs$fit, times = pred_times, type = "surv")

  expect_true(inherits(parsnip_df_pred, "tbl_df"))
  expect_equal(names(parsnip_df_pred), ".prob_censored")
  expect_equal(nrow(parsnip_df_pred), length(pred_times))
  expect_equal(parsnip_df_pred[[1]], pl_pred)
  expect_equal(parsnip_vec_pred, pl_pred)
  expect_true(inherits(parsnip_vec_pred, "numeric"))
})
