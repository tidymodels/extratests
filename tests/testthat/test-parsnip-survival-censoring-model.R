skip_if_not_installed("censored")
skip_if_not_installed("prodlim")

library(censored)

test_that("`reverse_km()`: fit reverse Kaplan-Meier curves", {
  mod_fit <- survival_reg() %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_true(any(names(mod_fit) == "censor_probs"))

  expect_s3_class(
    mod_fit$censor_probs,
    c("censoring_model_reverse_km", "censoring_model")
  )
  expect_named(
    mod_fit$censor_probs,
    c("formula", "fit", "label", "required_pkgs")
  )
  expect_s3_class(mod_fit$censor_probs$fit, "prodlim")
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
  expect_equal(mod_fit$censor_probs$label, "reverse_km")
  expect_equal(mod_fit$censor_probs$required_pkgs, "prodlim")

  rev_km_fit <- as_tibble(mod_fit$censor_probs$fit[1:6])
  exp_rev_km_fit <- prodlim::prodlim(
    Surv(time, status) ~ 1,
    data = lung,
    reverse = TRUE,
    type = "surv"
  )
  exp_rev_km_fit <- as_tibble(exp_rev_km_fit[1:6])
  expect_equal(rev_km_fit, exp_rev_km_fit)
})

test_that("print reverse Kaplan-Meier curves", {
  mod_fit <-
    survival_reg() %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  # For testing purposes
  attr(mod_fit$censor_probs$formula, ".Environment") <- rlang::base_env()

  expect_snapshot(mod_fit$censor_probs)
})

test_that("predict with reverse Kaplan-Meier curves", {
  mod_fit <-
    survival_reg() %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  pred_times <- (7:10) * 100
  exp_pred <- predict(mod_fit$censor_probs$fit, times = pred_times, type = "surv")
  pred_df <- predict(mod_fit$censor_probs, time = pred_times)

  expect_s3_class(pred_df, "tbl_df")
  expect_named(pred_df, ".prob_censored")
  expect_equal(nrow(pred_df), length(pred_times))
  expect_equal(pred_df[[1]], exp_pred)

  pred_vec <- predict(mod_fit$censor_probs, time = pred_times,
                      as_vector = TRUE)
  expect_type(pred_vec, "double")
  expect_equal(pred_vec, exp_pred)
})

test_that("predict can handle NA times", {
  mod_fit <-
    survival_reg() %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  pred_times <- (7:10) * 100
  pred_miss <- predict(mod_fit$censor_probs,
                       time = c(NA_real_, pred_times),
                       as_vector = TRUE)
  expect_equal(length(pred_miss), length(pred_times) + 1)
  expect_equal(sum(is.na(pred_miss)), 1L)
  expect_equal(which(is.na(pred_miss)), 1)
})

test_that("Handle unknown censoring model", {
  mod_fit <-
    survival_reg() %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  alt_obj <- mod_fit$censor_probs
  class(alt_obj) <- "censoring_model"
  expect_snapshot(error = TRUE, predict(alt_obj, time = 100))
})

test_that("`reverse_km()` returns NULL for unrelated modes", {
  expect_equal(
    parsnip:::reverse_km(linear_reg(), eval_env = NULL),
    list()
  )
})
