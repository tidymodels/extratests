library(testthat)
library(parsnip)

R_version_too_small_for_glmnet <- utils::compareVersion(
  '3.6.0',
  as.character(getRversion())
) >
  0
skip_if(R_version_too_small_for_glmnet)

test_that("glmnet execution and model object", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(
    ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  expect_error(
    f_fit <- fit(
      mr_spec,
      class ~ protocol + log(compounds) + input_fields,
      data = hpc_data
    ),
    NA
  )
  expect_error(
    xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y),
    NA
  )

  expect_identical(f_fit$fit, xy_fit$fit)
  # removing call element
  expect_identical(f_fit$fit[-14], exp_fit[-14])
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

  expect_identical(
    predict(xy_fit, hpc[rows, c(3:1, 4)], type = "class"),
    predict(xy_fit, hpc[rows, 1:4], type = "class")
  )
})

test_that("glmnet prediction: type class", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(
    ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1, type = "class")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(
    mr_spec,
    class ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc_data, type = "class")
  xy_pred <- predict(xy_fit, hpc_x, type = "class")
  expect_identical(f_pred, xy_pred)
  expect_identical(
    f_pred$.pred_class %>% as.character(),
    exp_pred %>% as.vector()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_identical(names(f_pred), ".pred_class")
  expect_identical(nrow(f_pred), nrow(hpc_data))

  # single prediction
  f_pred_1 <- predict(f_fit, hpc_data[1, ])
  expect_identical(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE])
  expect_identical(nrow(xy_pred_1), 1)
})

test_that("glmnet prediction: type prob", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(
    ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1, type = "response")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(
    mr_spec,
    class ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc_data, type = "prob")
  xy_pred <- predict(xy_fit, hpc_x, type = "prob")
  expect_identical(f_pred, xy_pred)
  expect_identical(
    as.matrix(f_pred) %>% unname(),
    exp_pred[,, 1] %>% unname()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_identical(
    names(f_pred),
    c(".pred_VF", ".pred_F", ".pred_M", ".pred_L")
  )
  expect_identical(nrow(f_pred), nrow(hpc_data))

  # single prediction
  f_pred_1 <- predict(f_fit, hpc_data[1, ], type = "prob")
  expect_identical(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE], type = "prob")
  expect_identical(nrow(xy_pred_1), 1)
})

test_that("glmnet prediction: type raw", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(
    ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )[, -1]
  hpc_y <- hpc_data$class

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1)

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(
    mr_spec,
    class ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc_data, type = "raw")
  xy_pred <- predict(xy_fit, hpc_x, type = "raw")
  expect_identical(f_pred, xy_pred)
  parsnip_version_without_bug_fix <-
    utils::packageVersion("parsnip") < "1.0.3.9001"
  if (parsnip_version_without_bug_fix) {
    exp_pred <- predict(exp_fit, hpc_x)
    expect_identical(f_pred, exp_pred)
  } else {
    expect_identical(f_pred, exp_pred)
  }

  # single prediction
  f_pred_1 <- predict(f_fit, hpc_data[1, ], type = "raw")
  expect_identical(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE], type = "raw")
  expect_identical(nrow(xy_pred_1), 1)
})

test_that("formula interface can deal with missing values", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc_data$compounds[1] <- NA

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(mr_spec, class ~ log(compounds) + input_fields, data = hpc_data)

  f_pred <- predict(f_fit, hpc_data, type = "class")
  expect_identical(nrow(f_pred), nrow(hpc_data))
  expect_identical(
    f_pred$.pred_class[1],
    factor(NA_character_, levels = c("VF", "F", "M", "L"))
  )

  f_pred <- predict(f_fit, hpc_data, type = "prob")
  expect_identical(nrow(f_pred), nrow(hpc_data))
  expect_all_equal(unlist(f_pred[1, ]), NA_real_)
})

test_that("glmnet multi_predict(): type class", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(
    ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )[, -1]
  hpc_y <- hpc_data$class

  penalty_values <- c(0.01, 0.1)

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = penalty_values, type = "class")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(
    mr_spec,
    class ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  expect_true(has_multi_predict(xy_fit))
  expect_identical(multi_predict_args(xy_fit), "penalty")

  f_pred <- multi_predict(
    f_fit,
    hpc_data,
    penalty = penalty_values,
    type = "class"
  )
  xy_pred <- multi_predict(
    xy_fit,
    hpc_x,
    penalty = penalty_values,
    type = "class"
  )
  expect_identical(f_pred, xy_pred)

  f_pred_001 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.01) %>%
    dplyr::pull(.pred_class)
  f_pred_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.1) %>%
    dplyr::pull(.pred_class)
  expect_identical(as.character(f_pred_001), unname(exp_pred[, 1]))
  expect_identical(as.character(f_pred_01), unname(exp_pred[, 2]))

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_identical(names(f_pred), ".pred")
  expect_identical(nrow(f_pred), nrow(hpc_data))
  expect_all_equal(purrr::map(f_pred$.pred, dim), list(c(2, 2)))
  parsnip_version_without_bug_fix <-
    utils::packageVersion("parsnip") < "1.0.3.9002"
  if (parsnip_version_without_bug_fix) {
    expect_all_equal(
      purrr::map(f_pred$.pred, names),
      list(c(".pred_class", "penalty"))
    )
  } else {
    expect_all_equal(
      purrr::map(f_pred$.pred, names),
      list(c("penalty", ".pred_class"))
    )
  }

  # single prediction
  f_pred_1 <- multi_predict(
    f_fit,
    hpc_data[1, ],
    penalty = penalty_values,
    type = "class"
  )
  xy_pred_1 <- multi_predict(
    xy_fit,
    hpc_x[1, , drop = FALSE],
    penalty = penalty_values,
    type = "class"
  )
  expect_identical(f_pred_1, xy_pred_1)
  expect_identical(nrow(f_pred_1), 1)
  expect_identical(nrow(f_pred_1$.pred[[1]]), 2)
})

test_that("glmnet multi_predict(): type prob", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  hpc_x <- model.matrix(
    ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )[, -1]
  hpc_y <- hpc_data$class

  penalty_values <- c(0.01, 0.1)

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "multinomial")
  exp_pred <- predict(exp_fit, hpc_x, s = penalty_values, type = "response")

  mr_spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(
    mr_spec,
    class ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )
  xy_fit <- fit_xy(mr_spec, x = hpc_x, y = hpc_y)

  f_pred <- multi_predict(
    f_fit,
    hpc_data,
    penalty = penalty_values,
    type = "prob"
  )
  xy_pred <- multi_predict(
    xy_fit,
    hpc_x,
    penalty = penalty_values,
    type = "prob"
  )
  expect_identical(f_pred, xy_pred)

  f_pred_001 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.01) %>%
    dplyr::select(-penalty)
  f_pred_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.1) %>%
    dplyr::select(-penalty)
  expect_identical(
    as.matrix(f_pred_001) %>% unname(),
    exp_pred[,, 1] %>% unname()
  )
  expect_identical(
    as.matrix(f_pred_01) %>% unname(),
    exp_pred[,, 2] %>% unname()
  )

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_identical(names(f_pred), ".pred")
  expect_identical(nrow(f_pred), nrow(hpc_data))
  expect_all_equal(purrr::map(f_pred$.pred, dim), list(c(2, 5)))
  parsnip_version_without_bug_fix <-
    utils::packageVersion("parsnip") < "1.0.3.9002"
  if (parsnip_version_without_bug_fix) {
    expect_all_equal(
      purrr::map(f_pred$.pred, names),
      list(c(".pred_VF", ".pred_F", ".pred_M", ".pred_L", "penalty"))
    )
  } else {
    expect_all_equal(
      purrr::map(f_pred$.pred, names),
      list(c("penalty", ".pred_VF", ".pred_F", ".pred_M", ".pred_L"))
    )
  }

  # single prediction
  f_pred_1 <- multi_predict(
    f_fit,
    hpc_data[1, ],
    penalty = penalty_values,
    type = "prob"
  )
  xy_pred_1 <- multi_predict(
    xy_fit,
    hpc_x[1, , drop = FALSE],
    penalty = penalty_values,
    type = "prob"
  )
  expect_identical(f_pred_1, xy_pred_1)
  expect_identical(nrow(f_pred_1), 1)
  expect_identical(nrow(f_pred_1$.pred[[1]]), 2)
})

test_that("glmnet multi_predict(): type NULL", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  spec <- multinom_reg(penalty = 0.1) %>% set_engine("glmnet")
  f_fit <- fit(
    spec,
    class ~ protocol + log(compounds) + input_fields,
    data = hpc_data
  )

  pred <- predict(f_fit, hpc_data[1:5, ])
  pred_class <- predict(f_fit, hpc_data[1:5, ], type = "class")
  expect_identical(pred, pred_class)

  mpred <- multi_predict(f_fit, hpc_data[1:5, ])
  mpred_class <- multi_predict(f_fit, hpc_data[1:5, ], type = "class")
  expect_identical(mpred, mpred_class)
})

test_that("multi_predict() with default or single penalty value", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[, c(2:5, 8)]
  rows <- c(1, 51, 101)

  xy_fit <- fit_xy(
    multinom_reg(penalty = 0.1) %>% set_engine("glmnet"),
    x = hpc[, 1:4],
    y = hpc$class
  )

  # Can deal with single penalty value
  expect_error(
    multi_predict(
      xy_fit,
      new_data = hpc[rows, 1:4],
      penalty = 0.1,
      type = "class"
    ),
    NA
  )
  expect_error(
    multi_predict(
      xy_fit,
      new_data = hpc[rows, 1:4],
      penalty = 0.1,
      type = "prob"
    ),
    NA
  )
  # Can predict with default penalty. See #108
  expect_error(
    multi_predict(xy_fit, new_data = hpc[rows, 1:4], type = "class"),
    NA
  )
  expect_error(
    multi_predict(xy_fit, new_data = hpc[rows, 1:4], type = "prob"),
    NA
  )

  skip_if(packageVersion("parsnip") < "1.0.4.9002")

  expect_snapshot(error = TRUE, {
    multi_predict(xy_fit, newdata = hpc[rows, 1:4], penalty = c(0.1, 0.5))
  })
})

test_that("class predictions are factors with all levels", {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[, c(2:5, 8)]

  basic <- multinom_reg(penalty = 0.1) %>%
    set_engine("glmnet") %>%
    fit(class ~ ., data = hpc)
  nd <- hpc[hpc$class == "VF", ]
  yhat <- predict(basic, new_data = nd, penalty = .1)
  yhat_multi <- multi_predict(basic, new_data = nd, penalty = .1)$.pred
  expect_s3_class(yhat_multi[[1]]$.pred_class, "factor")
  expect_identical(levels(yhat_multi[[1]]$.pred_class), levels(hpc$class))
})

test_that('error traps', {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip", minimum_version = "1.2.1.9003")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())

  expect_snapshot(error = TRUE, {
    multinom_reg(penalty = 0.01) %>%
      set_engine("glmnet") %>%
      fit(class ~ ., data = hpc_data) %>%
      predict(hpc_data, penalty = 0:1)
  })
  expect_snapshot(error = TRUE, {
    multinom_reg() %>%
      set_engine("glmnet") %>%
      fit(class ~ ., data = hpc_data)
  })
  expect_snapshot(error = TRUE, {
    multinom_reg(penalty = 0.01) %>%
      set_engine("glmnet") %>%
      fit(class ~ ., data = hpc_data) %>%
      multi_predict(hpc_data, type = "numeric")
  })
})
