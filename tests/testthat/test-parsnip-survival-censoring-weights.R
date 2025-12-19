test_that("graf_weight_time_vec() calculates weight time", {
  skip_if_not_installed("survival")

  eval_time_10 <- 10

  # Graf et al (1999) Category 1
  event_before_or_at_eval_time <- survival::Surv(
    time = c(eval_time_10 - 1, eval_time_10),
    event = c(1, 1)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      event_before_or_at_eval_time,
      eval_time = eval_time_10
    ),
    c(eval_time_10 - 1, eval_time_10)
  )

  # Graf et al (1999) Category 2
  observed_time_gt_eval_time <- survival::Surv(
    time = c(eval_time_10 + 1, eval_time_10 + 1),
    event = c(1, 0)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      observed_time_gt_eval_time,
      eval_time = eval_time_10
    ),
    rep(eval_time_10, 2)
  )

  # Graf et al (1999) Category 3
  censoring_before_or_at_eval_time <- survival::Surv(
    time = c(eval_time_10 - 1, eval_time_10),
    event = c(0, 0)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      censoring_before_or_at_eval_time,
      eval_time = eval_time_10
    ),
    c(NA, NA)
  )
})

test_that("graf_weight_time_vec() guards against information leakage via `eps`", {
  skip_if_not_installed("survival")

  eval_time_10 <- 10

  # Graf et al (1999) Category 1
  time_before_or_at_eval_time <- c(eval_time_10 - 1, eval_time_10)
  event_before_or_at_eval_time <- survival::Surv(
    time = time_before_or_at_eval_time,
    event = c(1, 1)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      event_before_or_at_eval_time,
      eval_time = eval_time_10,
      eps = 2
    ),
    time_before_or_at_eval_time - 2
  )

  # Graf et al (1999) Category 2
  observed_time_gt_eval_time <- survival::Surv(
    time = c(eval_time_10 + 1, eval_time_10 + 1),
    event = c(1, 0)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      observed_time_gt_eval_time,
      eval_time = eval_time_10,
      eps = 2
    ),
    rep(eval_time_10, 2) - 2
  )

  # Graf et al (1999) Category 3
  # weight time is NA, thus no modification
})

test_that("graf_weight_time_vec() does not return negative weight times", {
  skip_if_not_installed("survival")

  # Graf et al (1999) Category 1
  # unmodified weight time is event_time < eps
  event_time_lt_eps <- survival::Surv(time = c(-1, 0), event = c(1, 1))
  expect_identical(
    parsnip:::graf_weight_time_vec(event_time_lt_eps, eval_time = 10),
    c(0, 0)
  )

  # Graf et al (1999) Category 2
  # unmodified weight time is eval_time < eps
  eval_time_lt_eps <- c(-1, 0)
  observed_time_gt_eval_time <- survival::Surv(time = c(1, 1), event = c(1, 0))
  expect_identical(
    parsnip:::graf_weight_time_vec(
      observed_time_gt_eval_time,
      eval_time = eval_time_lt_eps
    ),
    c(0, 0)
  )

  # Graf et al (1999) Category 3
  # weight time is NA, thus no check here
})

test_that("graf_weight_time_vec() handles eval_time of Inf", {
  skip_if_not_installed("survival")

  eval_time_inf <- Inf

  # Graf et al (1999) Category 1
  event_before_or_at_eval_time <- survival::Surv(
    time = c(1, eval_time_inf),
    event = c(1, 1)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      event_before_or_at_eval_time,
      eval_time = eval_time_inf
    ),
    c(1, eval_time_inf)
  )

  # Graf et al (1999) Category 2
  # for eval_time = Inf there is no "after"

  # Graf et al (1999) Category 3
  censoring_before_or_at_eval_time <- survival::Surv(
    time = c(1, eval_time_inf),
    event = c(0, 0)
  )
  expect_identical(
    parsnip:::graf_weight_time_vec(
      censoring_before_or_at_eval_time,
      eval_time = eval_time_inf
    ),
    c(NA, NA)
  )
})

test_that('compute Graf weights', {
  library(tidymodels)
  suppressPackageStartupMessages(library(censored))

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
    dplyr::slice(5)

  wt_times <-
    parsnip:::graf_weight_time_vec(
      pred_surv$surv,
      eval_time = pred_surv$.pred[[1]]$.eval_time
    )
  expect_identical(
    wt_times,
    c(NA, 0.9999999999, 1.9999999999, 2.9999999999, NA),
    tolerance = 0.01
  )

  cens_probs <- predict(fit$censor_probs, time = wt_times, as_vector = TRUE)

  wts <- .censoring_weights_graf(fit, pred_surv)
  expect_identical(names(wts), names(pred_surv))
  expect_identical(nrow(wts), nrow(pred_surv))
  expect_identical(dim(wts$.pred[[1]]), c(length(eval_times), 5L))
  expect_identical(wts$.pred[[1]]$.eval_time, eval_times)
  expect_identical(
    names(wts$.pred[[1]]),
    c(
      ".eval_time",
      ".pred_survival",
      ".weight_time",
      ".pred_censored",
      ".weight_censored"
    )
  )

  wts2 <- wts %>% unnest(.pred)
  expect_identical(wts2$.weight_censored, 1 / cens_probs)
})

test_that("error messages in context of .censoring_weights_graf()", {
  skip_if_not_installed("parsnip", minimum_version = "1.2.1.9002")

  lung2 <- lung %>%
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  expect_snapshot(error = TRUE, .censoring_weights_graf("nothing useful"))

  # temporarily moved to its own test below to allow skipping of (only) this test
  # based on dev version number
  #expect_snapshot(error = TRUE, .censoring_weights_graf(workflows::workflow()))

  # temporarily moved to its own test below to allow skipping of (only) this test
  # based on dev version number
  # # trigger `.check_censor_model()`
  # wrong_model <- fit(linear_reg(), mpg ~ ., data = mtcars)
  # expect_snapshot(error = TRUE, .censoring_weights_graf(wrong_model, mtcars))

  # trigger `.find_surv_col()`
  cox_model <- proportional_hazards() %>% fit(surv ~ ., data = lung2)
  expect_snapshot(error = TRUE, .censoring_weights_graf(cox_model, lung))

  # trigger `.check_censored_right()`
  expect_snapshot(error = TRUE, {
    lung_left <- lung[1, , drop = FALSE]
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

test_that("error for .censoring_weights_graf.workflow()", {
  # temporarily its own test, see above
  skip_if_not_installed("workflows", minimum_version = "1.1.3.9001")
  expect_snapshot(error = TRUE, .censoring_weights_graf(workflows::workflow()))
})

test_that("error for .censoring_weights_graf() from .check_censor_model()", {
  # temporarily its own test, see above
  skip_if_not_installed("parsnip", minimum_version = "1.2.1.9002")
  wrong_model <- fit(linear_reg(), mpg ~ ., data = mtcars)
  expect_snapshot(error = TRUE, .censoring_weights_graf(wrong_model, mtcars))
})

test_that("no names in weight values", {
  # See tidymodels/parsnip#1023 tidymodels/parsnip#1024
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9002")

  times <- c(9, 13, 13, 18, 23, 28)
  cens <- c(1, 1, 0, 1, 1, 0)
  surv_obj <- survival::Surv(times, cens)

  row_1 <- parsnip:::graf_weight_time_vec(surv_obj[1, , drop = FALSE], 1.0)
  row_5 <- parsnip:::graf_weight_time_vec(surv_obj, 1.0)
  expect_null(names(row_1))
  expect_null(names(row_5))
})
