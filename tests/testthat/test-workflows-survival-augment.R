test_that("augment survival workflows with eval_time", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9002")
  skip_if_not_installed("workflows", minimum_version = "1.1.3.9000")

  library(tidymodels)
  library(censored)

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  wflow_fit <-
    workflow() %>%
    add_model(proportional_hazards()) %>%
    add_formula(event_time ~ .) %>%
    fit(data = sim_dat)

  expect_snapshot(
    augment(wflow_fit, new_data = sim_dat),
    error = TRUE
  )

  exp_pred_col <- structure(
    list(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_time = numeric(0),
      .pred_censored = numeric(0),
      .weight_censored = numeric(0)
    ),
    row.names = integer(0),
    class = c("tbl_df", "tbl", "data.frame")
  )

  times <- c(2.0, 1.0)
  res <- augment(wflow_fit, new_data = head(sim_dat), eval_time = times)
  expect_identical(nrow(res), nrow(head(sim_dat)))
  expect_identical(
    names(res),
    c(".pred", ".pred_time", "event_time", "X1", "X2")
  )
  expect_type(res$.pred_time, "double")
  expect_type(res$.pred, "list")
  expect_ptype(res$.pred[[1]], exp_pred_col)
  expect_identical(nrow(res$.pred[[2]]), length(times))
  expect_identical(res$.pred[[3]]$.eval_time, times)

  # Predicting a single row and eval time
  res_1_row <- augment(
    wflow_fit,
    new_data = head(sim_dat, 1),
    eval_time = times[1]
  )
  expect_identical(nrow(res_1_row), 1L)
  expect_identical(
    names(res_1_row),
    c(".pred", ".pred_time", "event_time", "X1", "X2")
  )
  expect_type(res_1_row$.pred_time, "double")
  expect_type(res_1_row$.pred, "list")
  expect_ptype(res_1_row$.pred[[1]], exp_pred_col)
  expect_identical(nrow(res_1_row$.pred[[1]]), 1L)
  expect_identical(res_1_row$.pred[[1]]$.eval_time, times[1])

  ## Obligatory glmnet example for "what could go wrong" coverage:
  ## This will need to be updated when https://github.com/tidymodels/workflows/issues/209 is resolved
  skip("until workflows/#209 is fixed")
  expect_snapshot(
    workflow() %>%
      add_model(
        proportional_hazards(penalty = 0.001) %>% set_engine("glmnet")
      ) %>%
      add_formula(event_time ~ .) %>%
      fit(data = sim_dat) %>%
      augment(new_data = sim_dat),
    error = TRUE
  )
})
