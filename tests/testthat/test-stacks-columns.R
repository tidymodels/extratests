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
  # skip on pre-0.2.2
  skip_if(utils::packageVersion("stacks") < "0.2.1.9000")

  data("penguins")

  # change the species so that it has a - in it
  dat <- penguins %>%
    mutate(species = paste0(species, "-1"))

  # tune a glmnet model w 3 penalties
  tuned <- recipe(species ~ bill_length_mm + bill_depth_mm +
                    flipper_length_mm, data = dat) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    workflow(multinom_reg(engine = "glmnet", penalty = tune())) %>%
    tune_grid(dat %>% vfold_cv(3),
              metrics = metric_set(mn_log_loss),
              grid = crossing(penalty = c(.0001, .001, .01)),
              control = control_stack_grid())

  # stack em!
  expect_silent(
    data_st <- stacks() %>%
      add_candidates(tuned)
  )

  expect_true(inherits(data_st, "data_stack"))
  expect_true(".pred_Adelie.1_tuned_1_1" %in% colnames(data_st))

  # glmnet will likely present warnings
  blended <- data_st %>%
    blend_predictions()

  # ...though model fitting should work as expected
  expect_silent(
    fitted <- blended %>%
      fit_members()
  )

  # the column map should point to colnames adapted from make.names
  # rather than the original outcome names
  expect_true(all(grepl(".1", fitted$cols_map$tuned)))

  # prediction correctly points to new outcome levels
  expect_silent(
    preds <- predict(fitted, dat)
  )

  expect_true(inherits(preds, "tbl_df"))
})
