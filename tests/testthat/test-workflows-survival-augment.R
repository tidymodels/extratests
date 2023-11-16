test_that("augment survival workflows with eval_time", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip",   minimum_version = "1.1.0.9002")
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
    class = c("tbl_df", "tbl", "data.frame"))

  times <- c(2.0, 1.0)
  res <- augment(wflow_fit, new_data = head(sim_dat), eval_time = times)
  expect_equal(nrow(res), nrow(head(sim_dat)))
  expect_equal(
    names(res),
    c(".pred", ".pred_time", "event_time", "X1", "X2")
  )
  expect_true(is.numeric(res$.pred_time))
  expect_true(is.list(res$.pred))
  expect_equal(res$.pred[[1]][0,], exp_pred_col)
  expect_equal(nrow(res$.pred[[2]]), length(times))
  expect_equal(res$.pred[[3]]$.eval_time, times)

  # Predicting a single row and eval time
  res_1_row <- augment(wflow_fit, new_data = head(sim_dat, 1), eval_time = times[1])
  expect_true(nrow(res_1_row) == 1)
  expect_equal(
    names(res_1_row),
    c(".pred", ".pred_time", "event_time", "X1", "X2")
  )
  expect_true(is.numeric(res_1_row$.pred_time))
  expect_true(is.list(res_1_row$.pred))
  expect_equal(res_1_row$.pred[[1]][0,], exp_pred_col)
  expect_equal(nrow(res_1_row$.pred[[1]]), 1)
  expect_equal(res_1_row$.pred[[1]]$.eval_time, times[1])

  ## Obligatory glmnet example for "what could go wrong" coverage:
  ## This will need to be updated when https://github.com/tidymodels/workflows/issues/209 is resolved
  skip("until workflows/#209 is fixed")
  expect_snapshot(
    workflow() %>%
      add_model(proportional_hazards(penalty = 0.001) %>% set_engine("glmnet")) %>%
      add_formula(event_time ~ .) %>%
      fit(data = sim_dat) %>%
      augment(new_data = sim_dat),
    error = TRUE
  )
})
