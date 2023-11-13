test_that('error without model formula (workflow, no tune)', {
  skip_if_not_installed("parsnip", min_version = "1.1.1.9001")

  library(parsnip)
  library(workflows)

  gam_wflow <- workflow() %>%
    add_formula(mpg ~ .) %>%
    add_model(gen_additive_mod("regression"))

  expect_snapshot(
    error = TRUE,
    gam_fit <- gam_wflow %>% fit(mtcars)
  )
})

test_that('error without model formula (workflow, with tune)', {
  skip_if_not_installed("parsnip", min_version = "1.1.1.9001")

  library(parsnip)
  library(workflows)
  library(tune)
  library(rsample)

  gam_wflow <- workflow() %>%
    add_formula(mpg ~ .) %>%
    add_model(gen_additive_mod("regression"))

  expect_warning(
    gam_res <- gam_wflow %>% fit_resamples(bootstraps(mtcars))
  )

  expect_snapshot(show_notes(gam_res))
})

test_that('error without model formula (no workflow, with tune)', {
  skip_if_not_installed("parsnip", min_version = "1.1.1.9001")

  library(parsnip)
  library(tune)
  library(rsample)

  gam_spec <- gen_additive_mod("regression")

  expect_warning(
    gam_res <- gam_spec %>% fit_resamples(mpg ~ ., bootstraps(mtcars))
  )

  expect_snapshot(show_notes(gam_res))
})
