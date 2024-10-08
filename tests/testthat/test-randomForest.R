library(testthat)
library(parsnip)
library(tibble)

# ------------------------------------------------------------------------------

source(test_path("helper-objects.R"))

# ------------------------------------------------------------------------------

lending_club <- head(lending_club, 200)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")

lc_basic <- rand_forest(mode = "classification") %>%
  set_engine("randomForest")
bad_rf_cls <- rand_forest(mode = "classification") %>%
  set_engine("randomForest", sampsize = -10)

# ------------------------------------------------------------------------------

test_that('randomForest classification execution', {

  skip_if_not_installed("randomForest")

  # check: passes interactively but not on R CMD check
  # expect_error(
  #   fit(
  #     lc_basic,
  #     Class ~ funded_amnt + term,
  #     data = lending_club,
  #     control = ctrl
  #   ),
  #   regexp = NA
  # )

  expect_error(
    fit_xy(
      lc_basic,
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  expect_error(
    fit(
      bad_rf_cls,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  # check: passes interactively but not on R CMD check
  # randomForest_form_catch <- fit(
  #   bad_rf_cls,
  #   funded_amnt ~ term,
  #   data = lending_club,
  #   control = caught_ctrl
  # )
  # expect_true(inherits(randomForest_form_catch$fit, "try-error"))

  expect_error(
    fit_xy(
      bad_rf_cls,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il,
      control = caught_ctrl
    )
  )

})


test_that('randomForest classification prediction', {

  skip_if_not_installed("randomForest")

  xy_fit <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = lending_club[1:6, num_pred])
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict(xy_fit, new_data = lending_club[1:6, num_pred])$.pred_class)

  form_fit <- fit(
    lc_basic,
    Class ~ funded_amnt + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = lending_club[1:6, c("funded_amnt", "int_rate")])
  form_pred <- unname(form_pred)
  expect_equal(
    form_pred,
    predict(form_fit, new_data = lending_club[1:6, c("funded_amnt", "int_rate")])$.pred_class
  )
})

test_that('randomForest classification probabilities', {

  skip_if_not_installed("randomForest")

  xy_fit <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = lending_club[1:6, num_pred], type = "prob")
  xy_pred <- as_tibble(as.data.frame(xy_pred))
  names(xy_pred) <- paste0(".pred_", names(xy_pred))
  expect_equal(xy_pred, predict(xy_fit, new_data = lending_club[1:6, num_pred], type = "prob"))

  one_row <- predict(xy_fit, new_data = lending_club[1, num_pred], type = "prob")
  expect_equal(xy_pred[1,], one_row)

  form_fit <- fit(
    lc_basic,
    Class ~ funded_amnt + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_pred <- predict(form_fit$fit, newdata = lending_club[1:6, c("funded_amnt", "int_rate")], type = "prob")
  form_pred <- as_tibble(as.data.frame(form_pred))
  names(form_pred) <- paste0(".pred_", names(form_pred))
  expect_equal(
    form_pred,
    predict(form_fit, new_data = lending_club[1:6, c("funded_amnt", "int_rate")], type = "prob")
  )
})


# ------------------------------------------------------------------------------

car_form <- as.formula(mpg ~ .)
num_pred <- names(mtcars)[3:6]

car_basic <- rand_forest(mode = "regression") %>% set_engine("randomForest")

bad_ranger_reg <- rand_forest(mode = "regression") %>%
  set_engine("randomForest", min.node.size = -10)
bad_rf_reg <- rand_forest(mode = "regression") %>%
  set_engine("randomForest", sampsize = -10)

# ------------------------------------------------------------------------------

test_that('randomForest regression execution', {

  skip_if_not_installed("randomForest")

  expect_error(
    fit(
      car_basic,
      car_form,
      data = mtcars,
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    fit_xy(
      car_basic,
      x = mtcars,
      y = mtcars$mpg,
      control = ctrl
    ),
    regexp = NA
  )

  randomForest_form_catch <- fit(
    bad_rf_reg,
    car_form,
    data = mtcars,
    control = caught_ctrl
  )
  expect_true(inherits(randomForest_form_catch$fit, "try-error"))

  randomForest_xy_catch <- fit_xy(
    bad_rf_reg,
    x = mtcars,
    y = mtcars$mpg,
    control = caught_ctrl
  )
  expect_true(inherits(randomForest_xy_catch$fit, "try-error"))

})

test_that('randomForest regression prediction', {

  skip_if_not_installed("randomForest")

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars,
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(xy_fit$fit, newdata = tail(mtcars))
  xy_pred <- unname(xy_pred)

  expect_equal(xy_pred, predict(xy_fit, new_data = tail(mtcars))$.pred)

})

## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  skip_if_not_installed("randomForest")
  skip_if_not_installed("parsnip", minimum_version = "1.2.1.9002")

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    rand_forest(mtry = 1000, min_n = 1000, trees = 5) %>%
    set_engine("randomForest") %>%
    set_mode("regression")

  expect_snapshot(
    f_fit  <- spec %>% fit(body_mass_g ~ ., data = penguins),
  )

  expect_snapshot(
    xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g),
  )

  expect_equal(f_fit$fit$mtry, 6)
  expect_equal(f_fit$fit$call$nodesize, rlang::expr(min_rows(~1000, x)), ignore_attr = TRUE)
  expect_equal(xy_fit$fit$mtry, 6)
  expect_equal(xy_fit$fit$call$nodesize, rlang::expr(min_rows(~1000, x)), ignore_attr = TRUE)

})


