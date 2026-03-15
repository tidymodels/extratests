test_that("misspelled engine names give informative error for extension packages", {
  # See issue https://github.com/tidymodels/parsnip/issues/1110
  # This test only works if poissonreg hasn't been loaded yet.
  # It should be located before any calls to `library(poissonreg)`
  skip_if_not_installed("poissonreg")
  skip_if_not_installed("parsnip", "1.4.1.9004")

  expect_snapshot(
    poisson_reg() |>
      set_engine("gml") |>
      set_mode("regression"),
    error = TRUE
  )

  expect_snapshot(
    poisson_reg() |>
      set_mode("regression") |>
      set_engine("gml"),
    error = TRUE
  )

  expect_snapshot(
    poisson_reg() |>
      set_engine("gml"),
    error = TRUE
  )
})
