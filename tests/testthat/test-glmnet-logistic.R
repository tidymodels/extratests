library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(tidyr)
library(modeldata)

# ------------------------------------------------------------------------------

ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0

## -----------------------------------------------------------------------------

data(lending_club)
data(wa_churn)
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)
lc_basic <- logistic_reg(penalty = 0.1) %>% set_engine("glmnet")

# ------------------------------------------------------------------------------

test_that('glmnet execution', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  expect_error(
    res <- fit_xy(
      lc_basic,
      control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "penalty")

  expect_error(
    glmnet_xy_catch <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il,
      control = caught_ctrl
    )
  )
})

test_that('glmnet prediction, one lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1) %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = 0.1, type = "response")[,1]
  uni_pred <- ifelse(uni_pred >= 0.5, "good", "bad")
  uni_pred <- factor(uni_pred, levels = levels(lending_club$Class))
  uni_pred <- unname(uni_pred)

  expect_equal(uni_pred, predict(xy_fit, lending_club[1:7, num_pred])$.pred_class)
  expect_equal(uni_pred[2], predict(xy_fit, lending_club[2, num_pred])$.pred_class)
  expect_equal(
    predict(xy_fit, lending_club[1:7, num_pred]),
    predict(xy_fit, lending_club[1:7, sample(num_pred)])
  )


  res_form <- fit(
    logistic_reg(penalty = 0.1) %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    predict(res_form$fit,
            newx = form_mat,
            s = 0.1, type = "response")[,1]
  form_pred <- ifelse(form_pred >= 0.5, "good", "bad")
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  form_pred <- unname(form_pred)

  expect_equal(
    form_pred,
    predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "class")$.pred_class
  )

})


test_that('glmnet prediction, mulitiple lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  lams <- c(0.01, 0.1)

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1) %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  expect_equal(
    tibble(penalty = rep(lams, 7), .pred_class = factor(rep("good", 14), levels = c("bad", "good"))),
    multi_predict(xy_fit, lending_club[1:7, num_pred], type = "class", penalty = lams) %>% unnest(cols = c(.pred))
  )

  res_form <- fit(
    logistic_reg(penalty = 0.01) %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  expect_equal(
    tibble(penalty = rep(lams, 7), .pred_class = factor(rep("good", 14), levels = c("bad", "good"))),
    multi_predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], penalty = lams) %>% unnest(cols = c(.pred))
  )

})


test_that('glmnet probabilities, one lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.1)  %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  uni_pred <-
    predict(xy_fit$fit,
            newx = as.matrix(lending_club[1:7, num_pred]),
            s = 0.1, type = "response")[,1]
  uni_pred <- tibble(.pred_bad = 1 - uni_pred, .pred_good = uni_pred)

  expect_equal(
    uni_pred,
    predict(xy_fit, lending_club[1:7, num_pred], type = "prob")
  )

  res_form <- fit(
    logistic_reg(penalty = 0.1)  %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  form_mat <- model.matrix(Class ~ log(funded_amnt) + int_rate, data = lending_club)
  form_mat <- form_mat[1:7, -1]

  form_pred <-
    unname(predict(res_form$fit,
                   newx = form_mat,
                   s = 0.1, type = "response")[, 1])
  form_pred <- tibble(.pred_bad = 1 - form_pred, .pred_good = form_pred)

  expect_equal(
    form_pred,
    predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "prob")
  )

  one_row <- predict(res_form, lending_club[1, c("funded_amnt", "int_rate")], type = "prob")
  expect_equal(form_pred[1,], one_row, ignore_attr = TRUE)

})

test_that('glmnet probabilities, mulitiple lambda', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  lams <- c(0.01, 0.1)

  xy_fit <- fit_xy(
    logistic_reg(penalty = 0.01)  %>% set_engine("glmnet"),
    control = ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )

  mult_pred <-
    structure(list(penalty = c(0.01, 0.1, 0.01, 0.1, 0.01, 0.1, 0.01,
                               0.1, 0.01, 0.1, 0.01, 0.1, 0.01, 0.1),
                   .pred_bad = c(0.0248234347196115,
                                 0.0549999999999999, 0.0539668350997529, 0.0549999999999999, 0.0410602871701227,
                                 0.0549999999999999, 0.0614587344673951, 0.0549999999999999, 0.0246284512328244,
                                 0.0549999999999999, 0.0275287859173489, 0.0549999999999999, 0.0361787791778279,
                                 0.0549999999999999),
                   .pred_good = c(0.975176565280389, 0.945,
                                  0.946033164900247, 0.945, 0.958939712829877, 0.945, 0.938541265532605,
                                  0.945, 0.975371548767176, 0.945, 0.972471214082651, 0.945, 0.963821220822172,
                                  0.945)), row.names = c(NA, -14L),
              class = c("tbl_df", "tbl",
                        "data.frame"))

  expect_equal(
    mult_pred,
    multi_predict(xy_fit, lending_club[1:7, num_pred], lambda = lams, type = "prob", penalty = lams) %>%
      unnest(cols = c(.pred)),
    tolerance = 0.0001
  )

  res_form <- fit(
    logistic_reg(penalty = 0.01)  %>% set_engine("glmnet"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )


  form_pred <-
    structure(list(penalty = c(0.01, 0.1, 0.01, 0.1, 0.01, 0.1, 0.01,
                               0.1, 0.01, 0.1, 0.01, 0.1, 0.01, 0.1),
                   .pred_bad = c(0.0578012324911684,
                                 0.0549999999999999, 0.0637405836452112, 0.0549999999999999, 0.0632245768025071,
                                 0.0549999999999999, 0.0562134258323885, 0.0549999999999999, 0.00637189559769558,
                                 0.0549999999999999, 0.0271083211970798, 0.0549999999999999, 0.00952896597808395,
                                 0.0549999999999999),
                   .pred_good = c(0.942198767508832, 0.945,
                                  0.936259416354789, 0.945, 0.936775423197493, 0.945, 0.943786574167611,
                                  0.945, 0.993628104402304, 0.945, 0.97289167880292, 0.945, 0.990471034021916,
                                  0.945)), row.names = c(NA, -14L),
              class = c("tbl_df", "tbl",
                        "data.frame"))

  expect_equal(
    form_pred,
    multi_predict(res_form, lending_club[1:7, c("funded_amnt", "int_rate")], type = "prob", penalty = lams) %>%
      unnest(cols = c(.pred)),
    tolerance = 0.0001
  )

})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")
  skip_if(run_glmnet)

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    logistic_reg(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  pred_glmn <- predict(class_fit$fit, as.matrix(wa_churn[1:4, vars]), s = .1, type = "response")

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], penalty = .1, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], unname(pred_glmn[,1]))

  expect_error(
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], penalty = .1, type = "prob"),
    "Did you mean"
  )

  # Can predict using default penalty. See #108
  expect_error(
    multi_predict(class_fit, new_data = wa_churn[1:4, vars]),
    NA
  )

})
