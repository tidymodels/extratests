library(testthat)
library(parsnip)

R_version_too_small_for_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0
skip_if(R_version_too_small_for_glmnet)

test_that("glmnet execution and model object", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term,
                                 data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y,
                            family = "binomial")

  lr_spec <- logistic_reg(penalty = 0.123) %>% set_engine("glmnet")
  expect_error(
    f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
                 data = lending_club),
    NA
  )
  expect_error(
    xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y),
    NA
  )

  expect_equal(f_fit$fit, xy_fit$fit)
  # removing call element
  expect_equal(f_fit$fit[-12], exp_fit[-12])
})

test_that("glmnet prediction: type class", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term,
                                 data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y,
                            family = "binomial")
  exp_pred <- predict(exp_fit, lending_club_x, s = 0.123, type = "class")

  lr_spec <- logistic_reg(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)
  xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y)

  f_pred <- predict(f_fit, lending_club, type = "class")
  xy_pred <- predict(xy_fit, lending_club_x, type = "class")
  expect_equal(f_pred, xy_pred)
  expect_equal(
    f_pred$.pred_class %>% as.character(),
    exp_pred %>% as.vector()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_class")
  expect_equal(nrow(f_pred), nrow(lending_club))

  # single prediction
  f_pred_1 <- predict(f_fit, lending_club[1, ])
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, lending_club_x[1, , drop = FALSE])
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("glmnet prediction: column order of `new_data` irrelevant", {
  skip_if_not_installed("glmnet")

  data(lending_club, package = "modeldata", envir = rlang::current_env())
  lending_club <- head(lending_club, 200)
  num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1) %>% set_engine("glmnet"),
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  expect_equal(
    predict(xy_fit, lending_club[1:7, sample(num_pred)]),
    predict(xy_fit, lending_club[1:7, num_pred])
  )
})

test_that("glmnet prediction: type prob", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term, data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y, family = "binomial")
  exp_pred <- predict(exp_fit, lending_club_x, s = 0.123, type = "response")

  lr_spec <- logistic_reg(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)
  xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y)

  f_pred <- predict(f_fit, lending_club, type = "prob")
  xy_pred <- predict(xy_fit, lending_club_x, type = "prob")
  expect_equal(f_pred, xy_pred)
  expect_equal(
    f_pred$.pred_good,
    exp_pred %>% as.vector()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), c(".pred_bad", ".pred_good"))
  expect_equal(nrow(f_pred), nrow(lending_club))

  # single prediction
  f_pred_1 <- predict(f_fit, lending_club[1, ], type = "prob")
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, lending_club_x[1, , drop = FALSE], type = "prob")
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("glmnet prediction: type raw", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term, data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y, family = "binomial")
  exp_pred <- predict(exp_fit, lending_club_x, s = 0.123)

  lr_spec <- logistic_reg(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)
  xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y)

  f_pred <- predict(f_fit, lending_club, type = "raw")
  xy_pred <- predict(xy_fit, lending_club_x, type = "raw")
  expect_equal(f_pred, xy_pred)
  parsnip_version_without_bug_fix <-
    utils::packageVersion("parsnip") < "1.0.3.9001"
  if (parsnip_version_without_bug_fix) {
    exp_pred <- predict(exp_fit, lending_club_x)
    expect_equal(f_pred, exp_pred)
  } else {
    expect_equal(f_pred, exp_pred)
  }

  # single prediction
  f_pred_1 <- predict(f_fit, lending_club[1, ], type = "raw")
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, lending_club_x[1, , drop = FALSE], type = "raw")
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("formula interface can deal with missing values", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club$funded_amnt[1] <- NA

  lr_spec <- logistic_reg(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)

  f_pred <- predict(f_fit, lending_club)
  expect_equal(nrow(f_pred), nrow(lending_club))
  # no expectation for the first value to be NA because glmnet itself
  # returns a non-NA value
})

test_that("glmnet multi_predict(): type class", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term, data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  penalty_values <- c(0.01, 0.1)

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y,
                            family = "binomial")
  exp_pred <- predict(exp_fit, lending_club_x, s = penalty_values, type = "class")

  lr_spec <- logistic_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)
  xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y)

  expect_true(has_multi_predict(xy_fit))
  expect_equal(multi_predict_args(xy_fit), "penalty")

  f_pred <- multi_predict(f_fit, lending_club, penalty = penalty_values, type = "class")
  xy_pred <- multi_predict(xy_fit, lending_club_x, penalty = penalty_values, type = "class") # FIXME make this work with just hpc instead of hpc_x?
  expect_equal(f_pred, xy_pred)

  f_pred_001 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.01) %>%
    dplyr::pull(.pred_class)
  f_pred_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.1) %>%
    dplyr::pull(.pred_class)
  expect_equal(as.character(f_pred_001), unname(exp_pred[,1]))
  expect_equal(as.character(f_pred_01), unname(exp_pred[,2]))

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lending_club))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c("penalty", ".pred_class"))))
  )

  # single prediction
  f_pred_1 <- multi_predict(f_fit, lending_club[1, ], penalty = c(0.123, 0.5), type = "class")
  xy_pred_1 <- multi_predict(xy_fit, lending_club_x[1, , drop = FALSE], penalty = c(0.123, 0.5), type = "class")
  expect_equal(f_pred_1, xy_pred_1)
  expect_equal(nrow(f_pred_1), 1)
  expect_equal(nrow(f_pred_1$.pred[[1]]), 2)
})

test_that("glmnet multi_predict(): type prob", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term, data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  penalty_values <- c(0.01, 0.1)

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y, family = "binomial")
  exp_pred <- predict(exp_fit, lending_club_x, s = penalty_values, type = "response")

  lr_spec <- logistic_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)
  xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y)

  f_pred <- multi_predict(f_fit, lending_club, penalty = penalty_values, type = "prob")
  xy_pred <- multi_predict(xy_fit, lending_club_x, penalty = penalty_values, type = "prob") # FIXME make this work with just hpc instead of hpc_x?
  expect_equal(f_pred, xy_pred)

  f_pred_001 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.01) %>%
    dplyr::pull(.pred_good)
  f_pred_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.1) %>%
    dplyr::pull(.pred_good)
  expect_equal(f_pred_001, unname(exp_pred[,1]))
  expect_equal(f_pred_01, unname(exp_pred[,2]))

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lending_club))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 3))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c("penalty", ".pred_bad", ".pred_good"))))
  )

  # single prediction
  f_pred_1 <- multi_predict(f_fit, lending_club[1, ], penalty = penalty_values,
                            type = "prob")
  xy_pred_1 <- multi_predict(xy_fit, lending_club_x[1, , drop = FALSE],
                             penalty = penalty_values, type = "prob")
  expect_equal(f_pred_1, xy_pred_1)
  expect_equal(nrow(f_pred_1), 1)
  expect_equal(nrow(f_pred_1$.pred[[1]]), 2)
})

test_that('multi_predict() with default or single penalty value', {

  skip_if_not_installed("glmnet")

  data(wa_churn, package = "modeldata", envir = rlang::current_env())

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    logistic_reg(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  pred_glmn <- predict(class_fit$fit, as.matrix(wa_churn[1:4, vars]), s = .1,
                       type = "response")

  # Can predict using default penalty. See #108
  expect_error(
    multi_predict(class_fit, new_data = wa_churn[1:4, vars]),
    NA
  )

  # Can deal with single penalty value
  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars],
                          penalty = 0.1, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], unname(pred_glmn[,1]))

  expect_snapshot(error = TRUE, {
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], type = "prob")
  })
})

test_that("class predictions are factors with all levels", {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]
  lending_club_x <- model.matrix(~ log(funded_amnt) + int_rate + term,
                                 data = lending_club)[, -1]
  lending_club_y <- lending_club$Class

  exp_fit <- glmnet::glmnet(x = lending_club_x, y = lending_club_y,
                            family = "binomial")
  exp_pred <- predict(exp_fit, lending_club_x, s = 0.123, type = "class")

  lr_spec <- logistic_reg(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(lr_spec, Class ~ log(funded_amnt) + int_rate + term,
               data = lending_club)
  xy_fit <- fit_xy(lr_spec, x = lending_club_x, y = lending_club_y)

  f_pred <- predict(f_fit, lending_club, type = "class")
  xy_pred <- predict(xy_fit, lending_club_x, type = "class")
  expect_equal(f_pred, xy_pred)
  expect_s3_class(f_pred$.pred_class, "factor")
  expect_equal(levels(f_pred$.pred_class), levels(lending_club$Class))

  f_pred <- multi_predict(f_fit, lending_club, type = "class")
  xy_pred <- multi_predict(xy_fit, lending_club_x, type = "class")
  expect_equal(f_pred, xy_pred)
  expect_s3_class(f_pred$.pred[[1]]$.pred_class, "factor")
  expect_equal(levels(f_pred$.pred[[1]]$.pred_class), levels(lending_club$Class))
})

test_that('error traps', {
  skip_if_not_installed("glmnet")

  data("lending_club", package = "modeldata", envir = rlang::current_env())

  expect_snapshot(error = TRUE, {
    logistic_reg(penalty = 0.01) %>%
      set_engine("glmnet") %>%
      fit(Class ~ log(funded_amnt) + int_rate + term,
          data = lending_club) %>%
      predict(lending_club, penalty = 0:1)
  })
  expect_snapshot(error = TRUE, {
    logistic_reg() %>%
      set_engine("glmnet") %>%
      fit(Class ~ log(funded_amnt) + int_rate + term,
          data = lending_club)
  })
})

test_that("base-R families: type class", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9002")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]

  # quasibinomial() as an example for a base-R family
  spec <- logistic_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15, family = stats::quasibinomial())
  f_fit <- fit(spec, Class ~ log(funded_amnt) + int_rate + term, data = lending_club)

  expect_true(has_multi_predict(f_fit))
  expect_equal(multi_predict_args(f_fit), "penalty")

  pred <- predict(f_fit, lending_club[1:5,], type = "class")
  pred_005 <- predict(f_fit, lending_club[1:5,], type = "class", penalty = 0.05)
  mpred <- multi_predict(f_fit, lending_club[1:5,], type = "class")
  mpred_005 <- multi_predict(f_fit, lending_club[1:5,], type = "class",
                             penalty = 0.05)

  expect_identical(names(pred), ".pred_class")
  expect_true(
    all(purrr::map_lgl(mpred$.pred,
                       ~ all(names(.x) == c("penalty", ".pred_class"))))
  )
  expect_identical(
    pred$.pred_class,
    mpred %>% tidyr::unnest(cols = .pred) %>% pull(.pred_class)
  )
  expect_identical(
    pred_005$.pred_class,
    mpred_005 %>% tidyr::unnest(cols = .pred) %>% pull(.pred_class)
  )

  mpred <- multi_predict(f_fit, lending_club[1:5,], type = "class",
                         penalty = c(0.05, 0.1))

  expect_true(
    all(purrr::map_lgl(mpred$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
})

test_that("base-R families: type prob", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9002")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]

  # quasibinomial() as an example for a base-R family
  spec <- logistic_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15, family = stats::quasibinomial())
  f_fit <- fit(spec, Class ~ log(funded_amnt) + int_rate + term, data = lending_club)

  expect_true(has_multi_predict(f_fit))
  expect_equal(multi_predict_args(f_fit), "penalty")

  pred <- predict(f_fit, lending_club[1:5,], type = "prob")
  pred_005 <- predict(f_fit, lending_club[1:5,], type = "prob", penalty = 0.05)
  mpred <- multi_predict(f_fit, lending_club[1:5,], type = "prob")
  mpred_005 <- multi_predict(f_fit, lending_club[1:5,], type = "prob",
                             penalty = 0.05)

  expect_identical(names(pred), c(".pred_bad", ".pred_good"))
  expect_true(
    all(purrr::map_lgl(mpred$.pred,
                       ~ all(names(.x) == c("penalty", ".pred_bad", ".pred_good"))))
  )
  expect_identical(
    pred$.pred_bad,
    mpred %>% tidyr::unnest(cols = .pred) %>% pull(.pred_bad)
  )
  expect_identical(
    pred_005$.pred_bad,
    mpred_005 %>% tidyr::unnest(cols = .pred) %>% pull(.pred_bad)
  )

  mpred <- multi_predict(f_fit, lending_club[1:5,], type = "prob",
                         penalty = c(0.05, 0.1))

  expect_true(
    all(purrr::map_lgl(mpred$.pred,
                       ~ all(dim(.x) == c(2, 3))))
  )
})

test_that("base-R families: type NULL", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9002")

  data("lending_club", package = "modeldata", envir = rlang::current_env())
  lending_club <- lending_club[1:200, ]

  # quasibinomial() as an example for a base-R family
  spec <- logistic_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15, family = stats::quasibinomial())
  f_fit <- fit(spec, Class ~ log(funded_amnt) + int_rate + term, data = lending_club)

  pred <- predict(f_fit, lending_club[1:5,])
  pred_class <- predict(f_fit, lending_club[1:5,], type = "class")
  expect_identical(pred, pred_class)

  mpred <- multi_predict(f_fit, lending_club[1:5,])
  mpred_class <- multi_predict(f_fit, lending_club[1:5,], type = "class")
  expect_identical(mpred, mpred_class)
})
