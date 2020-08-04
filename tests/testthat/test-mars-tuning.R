library(testthat)

# ------------------------------------------------------------------------------

context("engine - earth with submodels *and* no submodels - tuning")

# ------------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")
set.seed(7898)
data_folds <- vfold_cv(two_class_dat, repeats = 3)

test_that("tuning for mars() -- submodels *and* no submodels", {

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
  params_grid <- grid_latin_hypercube(
    num_terms() %>% range_set(c(1L, 12L)),
    prod_degree(),
    prune_method(values = c("backward", "none", "forward")),
    size = 7)

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
      metrics = metric_set(roc_auc)
    ),
    NA
  )

  expect_error(mars_metrics <- collect_metrics(rs), NA)
  expect_equal(mars_metrics$.config, paste0("Model", 1:7))
  expect_equal(unique(mars_metrics$.metric), "roc_auc")
  expect_true(all(names(params_grid) %in% names(mars_metrics)))

})

