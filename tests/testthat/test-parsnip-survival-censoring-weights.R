
test_that('calculate weight time', {
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9006")
  skip_if_not_installed("censored", minimum_version = "0.1.1.9002")

  library(tidymodels)
  library(censored)

  times <- 1:10
  cens <- rep(0:1, times = 5)

  surv_obj <- Surv(times, cens)
  n <- length(surv_obj)

  eval_0 <- parsnip:::graf_weight_time_vec(surv_obj, eval_time = rep(0, n))
  eval_05 <- parsnip:::graf_weight_time_vec(surv_obj, eval_time = rep(5, n), eps = 1)
  eval_11 <- parsnip:::graf_weight_time_vec(surv_obj, eval_time = rep(11, n), eps = 0)

  na_05 <- is.na(eval_05)
  na_11 <- is.na(eval_11)

  expect_equal(eval_0, rep(0, 10))

  expect_equal(
    which(na_05),
    which(times <= 5 & cens == 0)
  )
  expect_equal(
    eval_05[!na_05],
    ifelse(times[!na_05] - 1 < 5, times[!na_05] - 1, 4)
  )

  expect_equal(
    which(na_11),
    which(cens == 0)
  )
  expect_equal(
    eval_11[!na_11],
    seq(2, 10, by = 2)
  )

})

test_that('compute Graf weights', {
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9006")
  skip_if_not_installed("censored", minimum_version = "0.1.1.9002")

  library(tidymodels)
  library(censored)

  times <- c(9, 1:9)
  cens <- rep(0:1, 5)
  surv_obj <- Surv(times, cens)
  n <- length(surv_obj)
  df <- data.frame(surv = surv_obj, x = -1:8)
  fit <- survival_reg() %>% fit(surv ~ x, data = df)
  wflow_fit <-
    workflow() %>%
    add_model(survival_reg(), formula = surv ~ x) %>%
    add_variables(surv, x) %>%
    fit(data = df)
  mod_fit <- extract_fit_parsnip(wflow_fit)

  eval_times <- c(5, 1:4)

  pred_surv <-
    predict(mod_fit, df, type = "survival", eval_time = eval_times) %>%
    bind_cols(
      predict(mod_fit, df, type = "time"),
      df
    ) %>%
    slice(5)

  wt_times <-
    parsnip:::graf_weight_time_vec(pred_surv$surv,
                                   eval_time = pred_surv$.pred[[1]]$.eval_time)
  expect_equal(wt_times, c(NA, 0.9999999999, 1.9999999999, 2.9999999999, NA), tolerance = 0.01)

  cens_probs <- predict(fit$censor_probs, time = wt_times, as_vector = TRUE)

  wts <- .censoring_weights_graf(fit, pred_surv)
  expect_equal(names(wts), names(pred_surv))
  expect_equal(nrow(wts), nrow(pred_surv))
  expect_equal(dim(wts$.pred[[1]]), c(length(eval_times), 5))
  expect_equal(wts$.pred[[1]]$.eval_time, eval_times)
  expect_equal(
    names(wts$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_time", ".pred_censored", ".weight_censored"))

  wts2 <- wts %>% unnest(.pred)
  expect_equal(wts2$.weight_censored, 1 / cens_probs)

})

test_that("error messages in context of .censoring_weights_graf()", {
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9006")
  skip_if_not_installed("censored", minimum_version = "0.1.1.9002")

  lung2 <- lung %>%
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  expect_snapshot(error = TRUE, .censoring_weights_graf("nothing useful"))

  expect_snapshot(error = TRUE, .censoring_weights_graf(workflows::workflow()))

  # trigger `.check_censor_model()`
  wrong_model <- fit(linear_reg(), mpg ~ ., data = mtcars)
  expect_snapshot(error = TRUE, .censoring_weights_graf(wrong_model, lung2)) # FIXME

  # trigger `.find_surv_col()`
  cox_model <- proportional_hazards() %>% fit(surv ~ ., data = lung2)
  expect_snapshot(error = TRUE, .censoring_weights_graf(cox_model, lung))

  # trigger `.check_censored_right()`
  expect_snapshot(error = TRUE, {
    lung_left <- lung[1,, drop = FALSE]
    lung_left$surv <- Surv(10, 0, type = "left")
    .censoring_weights_graf(cox_model, lung_left)
  })

  # trigger `.check_pred_col()`
  expect_snapshot(error = TRUE, .censoring_weights_graf(cox_model, lung2))

  # trigger warning directly in `.censoring_weights_graf()`
  preds <- predict(
    cox_model,
    new_data = lung2[1:3, ],
    type = "survival",
    eval_time = c(100, 200)
  )
  preds$surv <- lung2$surv[1:3]
  expect_snapshot({
    .censoring_weights_graf(
      cox_model,
      preds,
      cens_predictors = "shouldn't be using this anyway!"
    )
  })
})
