# load packages
library(testthat)

library(stacks)
library(recipes)
library(workflows)
library(parsnip)
library(yardstick)
library(tune)
library(rsample)
library(modeldata)

library(tidyr)
library(dplyr)

test_that("stacks can accommodate outcome levels that are not valid colnames", {
  skip_if_not_installed("stacks", "1.1.1.9001")

  data("penguins")

  # change the species so that it has a - in it
  dat <- penguins %>%
    mutate(species = paste0(species, "-1"))

  # tune a glmnet model w 3 penalties
  tuned <- recipe(
    species ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
    data = dat
  ) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    workflow(multinom_reg(engine = "glmnet", penalty = tune())) %>%
    tune_grid(
      dat %>% vfold_cv(3),
      metrics = metric_set(mn_log_loss),
      grid = crossing(penalty = c(.0001, .001, .01)),
      control = control_stack_grid()
    )

  # stack em!
  expect_silent(
    data_st <- stacks() %>%
      add_candidates(tuned)
  )

  expect_s3_class(data_st, "data_stack")
  expect_in(".pred_Adelie.1_tuned_0_2_0", colnames(data_st))

  # glmnet will likely present warnings
  suppressMessages(
    blended <- data_st %>%
      blend_predictions()
  )

  # ...though model fitting should work as expected
  expect_silent(
    fitted <- blended %>%
      fit_members()
  )

  # the column map should point to colnames adapted from make.names
  # rather than the original outcome names
  expect_all_true(grepl(".1", fitted$cols_map$tuned))

  # prediction correctly points to new outcome levels
  expect_silent(
    preds <- predict(fitted, dat)
  )

  expect_s3_class(preds, "tbl_df")
})
