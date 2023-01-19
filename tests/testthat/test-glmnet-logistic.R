library(testthat)
library(parsnip)

R_version_too_small_for_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0
skip_if(R_version_too_small_for_glmnet)

test_that("glmnet execution and model object", {
  skip_if_not_installed("glmnet")

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

test_that('glmnet prediction, mulitiple lambda', {

  skip_if_not_installed("glmnet")

  data(lending_club, package = "modeldata", envir = rlang::current_env())
  lending_club <- head(lending_club, 200)
  num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)

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

  data(lending_club, package = "modeldata", envir = rlang::current_env())
  lending_club <- head(lending_club, 200)
  num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)

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

  data(lending_club, package = "modeldata", envir = rlang::current_env())
  lending_club <- head(lending_club, 200)
  num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)

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

  data(wa_churn, package = "modeldata", envir = rlang::current_env())

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
