test_that('multi_predict helpers work on fitted workflows', {
  # tidymodels/parsnip#1410
  skip_if_not_installed("parsnip", minimum_version = "1.6.0.9002")
  skip_if_not_installed("workflows")
  skip_if_not_installed("kknn")

  library(parsnip)
  library(workflows)

  knn_fit <-
    workflow() |>
    add_formula(mpg ~ .) |>
    add_model(
      nearest_neighbor(neighbors = 7) |>
        set_engine("kknn") |>
        set_mode("regression")
    ) |>
    fit(data = mtcars)

  expect_identical(multi_predict_args(knn_fit), "neighbors")
  expect_identical(has_multi_predict(knn_fit), TRUE)

  # the workflow and the parsnip fit it wraps agree
  expect_identical(
    multi_predict_args(knn_fit),
    multi_predict_args(extract_fit_parsnip(knn_fit))
  )

  lm_fit <-
    workflow() |>
    add_formula(mpg ~ .) |>
    add_model(linear_reg() |> set_engine("lm")) |>
    fit(data = mtcars)

  expect_identical(multi_predict_args(lm_fit), NA_character_)
  expect_identical(has_multi_predict(lm_fit), FALSE)
})

test_that('multi_predict helpers error on untrained workflows', {
  # tidymodels/parsnip#1410
  skip_if_not_installed("parsnip", minimum_version = "1.6.0.9002")
  skip_if_not_installed("workflows")

  library(parsnip)
  library(workflows)

  wf <-
    workflow() |>
    add_formula(mpg ~ .) |>
    add_model(linear_reg() |> set_engine("lm"))

  expect_snapshot(error = TRUE, multi_predict_args(wf))
  expect_snapshot(error = TRUE, has_multi_predict(wf))
})
