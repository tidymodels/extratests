library(testthat)
library(parsnip)

R_version_too_small_for_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0
skip_if(R_version_too_small_for_glmnet)

test_that("glmnet execution and model object", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(~ protocol + log(compounds) + input_fields,
                        data = hpc_data)[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  expect_error(
    f_fit <- fit(mr_spec, class ~ protocol + log(compounds) + input_fields,
                 data = hpc_data),
    NA
  )
  expect_error(
    xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y),
    NA
  )

  expect_equal(f_fit$fit, xy_fit$fit)
  # removing call element
  expect_equal(f_fit$fit[-14], exp_fit[-14])
})

test_that("glmnet prediction: column order of `new_data` irrelevant", {

  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[, c(2:5, 8)]
  rows <- c(1, 51, 101)

  xy_fit <- fit_xy(
    multinom_reg(penalty = 0.1) %>% set_engine("glmnet"),
    x = hpc[, 1:4],
    y = hpc$class
  )

  expect_equal(
    predict(xy_fit, hpc[rows, c(3:1, 4)], type = "class"),
    predict(xy_fit, hpc[rows, 1:4], type = "class")
  )
})

test_that("glmnet prediction: type class", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(~ protocol + log(compounds) + input_fields,
                        data = hpc_data)[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1, type = "class")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(mr_spec, class ~ protocol + log(compounds) + input_fields,
               data = hpc_data)
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc_data, type = "class")
  xy_pred <- predict(xy_fit, hpc_x, type = "class")
  expect_equal(f_pred, xy_pred)
  expect_equal(
    f_pred$.pred_class %>% as.character(),
    exp_pred %>% as.vector()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_class")
  expect_equal(nrow(f_pred), nrow(hpc_data))

  # single prediction
  f_pred_1 <- predict(f_fit, hpc_data[1, ])
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE])
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("glmnet prediction: type prob", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(~ protocol + log(compounds) + input_fields,
                        data = hpc_data)[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1, type = "response")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(mr_spec, class ~ protocol + log(compounds) + input_fields,
               data = hpc_data)
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc_data, type = "prob")
  xy_pred <- predict(xy_fit, hpc_x, type = "prob")
  expect_equal(f_pred, xy_pred)
  expect_equal(
    as.matrix(f_pred) %>% unname(),
    exp_pred[, , 1] %>% unname()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), c(".pred_VF", ".pred_F", ".pred_M", ".pred_L"))
  expect_equal(nrow(f_pred), nrow(hpc_data))

  # single prediction
  f_pred_1 <- predict(f_fit, hpc_data[1, ], type = "prob")
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE], type = "prob")
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("glmnet prediction: type raw", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(~ protocol + log(compounds) + input_fields,
                        data = hpc_data)[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1)

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(mr_spec, class ~ protocol + log(compounds) + input_fields,
               data = hpc_data)
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc_data, type = "raw")
  xy_pred <- predict(xy_fit, hpc_x, type = "raw")
  expect_equal(f_pred, xy_pred)

  # single prediction
  f_pred_1 <- predict(f_fit, hpc_data[1, ], type = "raw")
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE], type = "raw")
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("formula interface can deal with missing values", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc_data$compounds[1] <- NA

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(mr_spec, class ~ log(compounds) + input_fields, data = hpc_data)

  f_pred <- predict(f_fit, hpc_data, type = "class")
  expect_equal(nrow(f_pred), nrow(hpc_data))
  expect_true(is.na(f_pred$.pred_class[1]))

  f_pred <- predict(f_fit, hpc_data, type = "prob")
  expect_equal(nrow(f_pred), nrow(hpc_data))
  expect_true(all(is.na(f_pred[1,])))
})

test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[, c(2:5, 8)]
  rows <- c(1, 51, 101)
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)

  lams <- c(0.01, 0.1)

  xy_fit <- fit_xy(
    multinom_reg(penalty = 0.1) %>% set_engine("glmnet"),
    control = ctrl,
    x = hpc[, 1:4],
    y = hpc$class
  )

  expect_error(predict(xy_fit, hpc[rows, 1:4], type = "class"), NA)
  expect_error(predict(xy_fit, hpc[rows, 1:4], type = "prob"), NA)

  mult_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(hpc[rows, 1:4]),
            s = lams, type = "response")
  mult_pred <- apply(mult_pred, 3, as_tibble)
  mult_pred <- dplyr:::bind_rows(mult_pred)
  mult_probs <- mult_pred
  names(mult_pred) <- paste0(".pred_", names(mult_pred))
  mult_pred$penalty <- rep(lams, each = 3)
  mult_pred$row <- rep(1:3, 2)
  mult_pred <- mult_pred[order(mult_pred$row, mult_pred$penalty),]
  mult_pred <- split(mult_pred[, -5], mult_pred$row)
  names(mult_pred) <- NULL
  mult_pred <- tibble(.pred = mult_pred)

  multi_pred_res <- multi_predict(xy_fit, hpc[rows, 1:4], penalty = lams, type = "prob")

  for (i in seq_along(multi_pred_res$.pred)) {
    expect_equal(
      mult_pred      %>% dplyr::slice(i) %>% pull(.pred) %>% purrr::pluck(1) %>% dplyr::select(starts_with(".pred")),
      multi_pred_res %>% dplyr::slice(i) %>% pull(.pred) %>% purrr::pluck(1) %>% dplyr::select(starts_with(".pred"))
    )
  }

  mult_class <- factor(names(mult_probs)[apply(mult_probs, 1, which.max)],
                       levels = xy_fit$lvl)
  mult_class <- tibble(
    .pred_class = mult_class,
    penalty = rep(lams, each = 3),
    row = rep(1:3, 2)
  )
  mult_class <- mult_class[order(mult_class$row, mult_class$penalty),]
  mult_class <- split(mult_class[, -3], mult_class$row)
  names(mult_class) <- NULL
  mult_class <- tibble(.pred = mult_class)

  mult_class_res <- multi_predict(xy_fit, hpc[rows, 1:4], penalty = lams)

  for (i in seq_along(mult_class_res$.pred)) {
    expect_equal(
      mult_class     %>% dplyr::slice(i) %>% pull(.pred) %>% purrr::pluck(1) %>% dplyr::select(starts_with(".pred")),
      mult_class_res %>% dplyr::slice(i) %>% pull(.pred) %>% purrr::pluck(1) %>% dplyr::select(starts_with(".pred"))
    )
  }

  expect_error(
    multi_predict(xy_fit, newdata = hpc[rows, 1:4], penalty = lams),
    "Did you mean"
  )

  # Can predict probs with default penalty. See #108
  expect_error(
    multi_predict(xy_fit, new_data = hpc[rows, 1:4], type = "prob"),
    NA
  )

})

test_that("class predictions are factors with all levels", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[, c(2:5, 8)]

  basic <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet") %>% fit(class ~ ., data = hpc)
  nd <- hpc[hpc$class == "VF", ]
  yhat <- predict(basic, new_data = nd, penalty = .1)
  yhat_multi <- multi_predict(basic, new_data =  nd, penalty = .1)$.pred
  expect_s3_class(yhat_multi[[1]]$.pred_class, "factor")
  expect_equal(levels(yhat_multi[[1]]$.pred_class), levels(hpc$class))
})
