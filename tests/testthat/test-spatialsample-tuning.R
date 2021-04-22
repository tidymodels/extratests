library(testthat)

# ------------------------------------------------------------------------------

library(spatialsample)
data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))
set.seed(7898)
folds <- spatial_clustering_cv(ames, coords = c("Latitude", "Longitude"), v = 5)

tree_spec <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("regression")


test_that("can tune with spatialsample object", {

  expect_error(
    rs <- workflow() %>%
      add_model(tree_spec) %>%
      add_formula(Sale_Price ~ Year_Built + Gr_Liv_Area +  Bldg_Type) %>%
      tune_grid(resamples = folds, grid = 5, metrics = metric_set(rmse)),
    NA
  )

  expect_error(tree_metrics <- collect_metrics(rs), NA)
  expect_equal(tree_metrics$.config, paste0("Preprocessor1_Model", 1:5))
  expect_equal(unique(tree_metrics$.metric), "rmse")

})
