library(testthat)

# ------------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")
set.seed(7898)
data_folds <- vfold_cv(two_class_dat, repeats = 3)

test_that("tuning for mars() -- submodels *and* no submodels", {
  skip_if_not_installed("tune", minimum_version = "1.3.0.9005")
  expect_error(
    mars_spec <-
      mars(num_terms = tune(), prod_degree = tune(), prune_method = tune()) %>%
      set_mode("classification") %>%
      set_engine("earth"),
    regexp = NA
  )

  expect_error(
    mars_wf <-
      workflow() %>%
      add_formula(Class ~ A + B) %>%
      add_model(mars_spec),
    NA
  )

  set.seed(123)
  params_grid <- grid_space_filling(
    num_terms() %>% range_set(c(1L, 12L)),
    prod_degree(),
    prune_method(values = c("backward", "none", "forward")),
    size = 7,
    type = "latin_hypercube")

  expect_equal(nrow(params_grid), 7)

  expect_error(
    mars_smol_grid <- min_grid(mars_spec, params_grid),
    NA
  )

  expect_length(
    smol_vec <- mars_smol_grid %>%
      unnest(.submodels, keep_empty = TRUE) %>%
      pull(.submodels) %>%
      map_int(length),
    5
  )

  expect_equal(sum(smol_vec), 2)

  expect_error(
    rs <- tune_grid(
      mars_wf,
      resample = data_folds,
      grid = params_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(save_pred = TRUE)
    ),
    NA
  )

  expect_error(mars_metrics <- collect_metrics(rs), NA)
  expect_equal(mars_metrics$.config, paste0("pre0_mod", 1:7, "_post0"))
  expect_equal(unique(mars_metrics$.metric), "roc_auc")
  expect_true(all(names(params_grid) %in% names(mars_metrics)))

  expect_error(mars_preds <- collect_predictions(rs, summarize = TRUE), NA)
  expect_equal(count(mars_preds, .row, .config)$n, rep(1L, 5537))

})

