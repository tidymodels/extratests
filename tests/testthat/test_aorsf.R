test_that('aorsf - regression', {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("bonsai")
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9003")

  spec <- rand_forest(
    trees = 15,
    mtry = 1,
    mode = "regression",
    engine = "aorsf"
  )
  set.seed(815)
  fit <- fit(spec, mpg ~ ., data = mtcars)
  expect_s3_class(fit$fit, c("ObliqueForestRegression", "ObliqueForest", "R6"))
  pred <- predict(fit, mtcars[1:3, -1])
  expect_s3_class(pred, c("tbl_df", "tbl", "data.frame"))
  expect_named(pred, ".pred")
  expect_equal(nrow(pred), 3L)
})

test_that('aorsf - classification', {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("bonsai")
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9003")

  spec <- rand_forest(
    trees = 15,
    mtry = 1,
    mode = "classification",
    engine = "aorsf"
  )
  set.seed(815)
  fit <- fit(spec, Class ~ ., data = modeldata::two_class_dat)
  expect_s3_class(
    fit$fit,
    c("ObliqueForestClassification", "ObliqueForest", "R6")
  )
  pred_cls <- predict(fit, modeldata::two_class_dat[1:3, -3])
  expect_s3_class(pred_cls, c("tbl_df", "tbl", "data.frame"))
  expect_named(pred_cls, ".pred_class")
  expect_equal(nrow(pred_cls), 3L)
  pred_prob <- predict(fit, modeldata::two_class_dat[1:3, -3], type = "prob")
  expect_s3_class(pred_prob, c("tbl_df", "tbl", "data.frame"))
  expect_named(pred_prob, c(".pred_Class1", ".pred_Class2"))
  expect_equal(nrow(pred_prob), 3L)
})


test_that('aorsf - censored regression', {
  skip_if_not_installed("aorsf")
  skip_if_not_installed("censored")
  skip_if_not_installed("prodlim")
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9003")

  set.seed(156)
  sim_dat <- prodlim::SimSurv(100) %>%
    mutate(
      event_time = survival::Surv(time, event),
      X3 = rnorm(100),
      X4 = runif(100)
    ) %>%
    select(event_time, X1, X2, X3, X4)

  spec <- rand_forest(
    trees = 15,
    mtry = 1,
    mode = "censored regression",
    engine = "aorsf"
  )

  set.seed(815)
  fit <- fit(spec, event_time ~ ., data = sim_dat)
  expect_s3_class(fit$fit, c("ObliqueForestSurvival", "ObliqueForest", "R6"))

  pred_time <- predict(fit, sim_dat[1:3, -1], type = "time")
  expect_s3_class(pred_time, c("tbl_df", "tbl", "data.frame"))
  expect_named(pred_time, ".pred_time")
  expect_equal(nrow(pred_time), 3L)

  pred_prob <- predict(
    fit,
    sim_dat[1:3, -1],
    type = "survival",
    eval_time = 1:2
  )
  expect_s3_class(pred_prob, c("tbl_df", "tbl", "data.frame"))
  expect_named(pred_prob, c(".pred"))
  expect_equal(nrow(pred_prob), 3L)
  expect_named(pred_prob$.pred[[1]], c(".eval_time", ".pred_survival"))
  expect_equal(nrow(pred_prob$.pred[[1]]), 2L)
})
