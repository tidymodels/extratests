suppressPackageStartupMessages(library(workflowsets))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(finetune))
suppressPackageStartupMessages(library(discrim))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(klaR))
suppressPackageStartupMessages(library(stacks))

# ------------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(1)
folds <- bootstraps(two_class_dat, times = 12)

# ------------------------------------------------------------------------------

discrim_regularized_klaR_spec <-
  discrim::discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
  set_engine('klaR')

rand_forest_ranger_spec <-
  rand_forest(min_n = tune(), trees = 20) %>%
  set_engine('ranger') %>%
  set_mode('classification')

# ------------------------------------------------------------------------------

rec <- recipe(Class ~ ., data = two_class_dat) %>%
  step_mutate(A = 1/A, B = 1/B)

f <- Class ~ .

# ------------------------------------------------------------------------------
# TODO remove "_" in RF name when https://github.com/tidymodels/stacks/issues/69
# is resolved.

ws <- workflow_set(
  preproc = list(f = f, rec = rec),
  models = list(rda = discrim_regularized_klaR_spec,
                "rand_forest" = rand_forest_ranger_spec),
  cross = TRUE
)

# ------------------------------------------------------------------------------

grid_ctrl <- control_grid(save_pred = TRUE, save_workflow = TRUE)
race_ctrl <- control_race(save_pred = TRUE, save_workflow = TRUE)

grid_res <-
  workflow_map(
    ws,
    resamples = folds,
    grid = 3,
    seed = 1,
    control = grid_ctrl
  )
race_res <-
  workflow_map(
    ws,
    "tune_race_anova",
    resamples = folds,
    grid = 3,
    seed = 1,
    control = race_ctrl
  )
race_candidates <-       # configurations that survived the full race per model
  race_res %>%
  tune::collect_metrics(summarize = FALSE) %>%
  dplyr::group_by(wflow_id, .config) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == 24) # <- 12 resamples x 2 metrics

# ------------------------------------------------------------------------------

test_that('stacking a workflow set using grid search', {
  expect_error(
    grid_ens <-
      stacks() %>%
      add_candidates(grid_res),
    regex = NA
  )
  # class col + (2 models x 2 pp x 2 classes) = 25 cols
  expect_true(is.data.frame(grid_ens))
  expect_true(ncol(grid_ens) == 25)

  expect_error({
    set.seed(2)
    grid_fit <- blend_predictions(grid_ens, penalty = .01)
  },
  regex = NA
  )
  grid_betas <-
    stacks:::.get_glmn_coefs(grid_fit$coefs$fit,
                             penalty = grid_fit$penalty$penalty) %>%
    dplyr::filter(terms != "(Intercept)" & estimate != 0)

  expect_error({
    set.seed(3)
    grid_members <- fit_members(grid_fit)
    },
    regex = NA
  )
  expect_true(nrow(grid_betas) == length(grid_members$member_fits))
})


# ------------------------------------------------------------------------------


test_that('stacking a workflow set using racing', {
  expect_error(
    race_ens <-
      stacks() %>%
      add_candidates(race_res),
    regex = NA
  )
  # class col + (nrow(race_candidates) x 2 classes) = 11 cols
  expect_true(is.data.frame(race_ens))
  expect_true(ncol(race_ens) == 11)

  expect_error({
    set.seed(2)
    race_fit <- blend_predictions(race_ens, penalty = .01)
  },
  regex = NA
  )
  race_betas <-
    stacks:::.get_glmn_coefs(race_fit$coefs$fit,
                             penalty = race_fit$penalty$penalty) %>%
    dplyr::filter(terms != "(Intercept)" & estimate != 0)

  expect_error({
    set.seed(3)
    race_members <- fit_members(race_fit)
  },
  regex = NA
  )
  expect_true(nrow(race_betas) == length(race_members$member_fits))
})

