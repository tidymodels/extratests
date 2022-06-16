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

test_that("can tune with sf-based spatialsample object", {
  skip_if(utils::packageVersion("spatialsample") < "0.1.0.9000")

  set.seed(7898)
  block <- spatial_block_cv(boston_canopy, v = 20, radius = 1, buffer = 1)

  expect_error(
    rs <- workflow() %>%
      add_model(tree_spec) %>%
      add_formula(mean_heat_index ~ change_canopy_percentage + canopy_percentage_2019 +  land_area) %>%
      tune_grid(resamples = block, grid = 5, metrics = metric_set(rmse)),
    NA
  )
  expect_error(tree_metrics <- collect_metrics(rs), NA)
  expect_equal(tree_metrics$.config, paste0("Preprocessor1_Model", 1:5))
  expect_equal(unique(tree_metrics$.metric), "rmse")

  # ensure that:
  # 1. the splits used by tune are the splits created by spatialsample
  # 2. these splits respect the exclusion buffer
  data_first_split <- block$splits[[1]]
  rs_first_split <- rs$splits[[1]]
  expect_equal(
    nrow(analysis(rs_first_split)),
    nrow(analysis(data_first_split))
  )
  expect_equal(
    nrow(assessment(rs_first_split)),
    nrow(assessment(data_first_split))
  )
  expect_true(
    nrow(assessment(rs_first_split)) +
      nrow(analysis(rs_first_split)) <
      nrow(boston_canopy)
  )

})
