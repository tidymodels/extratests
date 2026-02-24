# To go with https://github.com/tidymodels/parsnip/pull/1323
test_that('mlp execution, quantile regression', {
  skip_if_not_installed("parsnip", minimum_version = "1.4.1.9001")
  skip_if_not_installed("qrnn")
  skip_if_not_installed("modeldata")
  skip_on_cran()

  set.seed(10)
  dat <- sim_regression(200)

  spec_1 <-
    mlp() |>
    set_engine("qrnn") |>
    set_mode("quantile regression", quantile_levels = (1:9) / 10)
  expect_snapshot(spec_1)

  set.seed(372)
  qnt_fit_1 <- fit(spec_1, outcome ~ ., data = dat)
  # The outcome is unclassed and has no print method :-O
  exp_names <-
    c(
      "weights",
      "lower",
      "eps.seq",
      "tau",
      "Th",
      "x.center",
      "x.scale",
      "y.center",
      "y.scale",
      "monotone",
      "additive"
    )
  expect_named(qnt_fit_1$fit, exp_names)
  expect_snapshot(print(body(qnt_fit_1$fit$Th)))

  spec_2 <-
    mlp(activation = "relu") |>
    set_engine("qrnn") |>
    set_mode("quantile regression", quantile_levels = (1:9) / 10)
  expect_snapshot(spec_2)

  set.seed(372)
  qnt_fit_2 <- fit(spec_2, outcome ~ ., data = dat)
  expect_named(qnt_fit_2$fit, exp_names)
  expect_snapshot(print(body(qnt_fit_2$fit$Th)))

  spec_3 <-
    mlp(activation = "relu", epochs = 20, penalty = 0.1, hidden_units = 4) |>
    set_engine("qrnn", method = "adam", ) |>
    set_mode("quantile regression", quantile_levels = (1:9) / 10)
  expect_snapshot(spec_3)

  set.seed(372)
  qnt_fit_3 <- fit(spec_3, outcome ~ ., data = dat)
  expect_named(qnt_fit_3$fit, exp_names)

  expect_snapshot(
    mlp(activation = "POTAAATO") |>
      set_engine("qrnn") |>
      set_mode("quantile regression", quantile_levels = (1:9) / 10) |>
      fit(outcome ~ ., data = dat),
    error = TRUE
  )

  expect_snapshot(
    mlp() |>
      set_engine("qrnn", Th.prime = "something") |>
      set_mode("quantile regression", quantile_levels = (1:9) / 10) |>
      fit(outcome ~ ., data = dat),
    error = TRUE
  )
})
