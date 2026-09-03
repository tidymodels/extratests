# evaluation time argument to tune objects

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = reg_mtr)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      linear_reg() %>% tune_grid(age ~ ., resamples = rs, metrics = reg_mtr,
      eval_time = 1)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).
      Warning:
      No tuning parameters have been detected, performance will be evaluated using the resamples with no tuning. Did you want to [tune()] parameters?
    Output
      # Tuning results
      # 10-fold cross-validation using stratification 
      # A tibble: 10 x 4
         splits           id     .metrics         .notes          
         <list>           <chr>  <list>           <list>          
       1 <split [164/20]> Fold01 <tibble [1 x 4]> <tibble [0 x 3]>
       2 <split [165/19]> Fold02 <tibble [1 x 4]> <tibble [0 x 3]>
       3 <split [165/19]> Fold03 <tibble [1 x 4]> <tibble [0 x 3]>
       4 <split [166/18]> Fold04 <tibble [1 x 4]> <tibble [0 x 3]>
       5 <split [166/18]> Fold05 <tibble [1 x 4]> <tibble [0 x 3]>
       6 <split [166/18]> Fold06 <tibble [1 x 4]> <tibble [0 x 3]>
       7 <split [166/18]> Fold07 <tibble [1 x 4]> <tibble [0 x 3]>
       8 <split [166/18]> Fold08 <tibble [1 x 4]> <tibble [0 x 3]>
       9 <split [166/18]> Fold09 <tibble [1 x 4]> <tibble [0 x 3]>
      10 <split [166/18]> Fold10 <tibble [1 x 4]> <tibble [0 x 3]>

---

    Code
      no_usable_times <- spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs,
      metrics = mtr, eval_time = c(-1, Inf))
    Condition
      Error:
      ! There were no usable evaluation times (finite, non-missing, and >= 0).

# eval time inputs are checked for censored regression models

    Code
      check_eval_time_arg(NULL, met_stc)
    Output
      NULL

---

    Code
      check_eval_time_arg(NULL, met_dyn)
    Condition
      Error:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_stc_dyn)
    Condition
      Error:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_stc_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_dyn_stc)
    Condition
      Error:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_dyn_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int_stc)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int_dyn)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(2, met_stc)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).
    Output
      NULL

---

    Code
      check_eval_time_arg(2, met_dyn)
    Output
      [1] 2

---

    Code
      check_eval_time_arg(2, met_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_stc_dyn)
    Output
      [1] 2

---

    Code
      check_eval_time_arg(2, met_stc_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_dyn_stc)
    Output
      [1] 2

---

    Code
      check_eval_time_arg(2, met_dyn_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_int_stc)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_int_dyn)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(1:3, met_stc)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).
    Output
      NULL

---

    Code
      check_eval_time_arg(1:3, met_dyn)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_int)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_stc_dyn)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_stc_int)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_dyn_stc)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_dyn_int)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_int_stc)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_int_dyn)
    Output
      [1] 1 2 3

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc)

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn)
    Condition
      Error in `fit_resamples()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_dyn)
    Condition
      Error in `fit_resamples()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_int)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_stc)
    Condition
      Error in `fit_resamples()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_int)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_stc)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_dyn)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_int, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_int, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_dyn)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_int)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_stc)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_int)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_stc)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_dyn)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 1:3)

---

    Code
      last_fit(wflow, split, metrics = met_dyn)
    Condition
      Error in `last_fit()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_stc_dyn)
    Condition
      Error in `last_fit()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_stc_int)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_dyn_stc)
    Condition
      Error in `last_fit()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_dyn_int)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int_stc)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int_dyn)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc, eval_time = 2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_int, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      last_fit(wflow, split, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      last_fit(wflow, split, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_int, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_int, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_int, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_int_stc, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_int_dyn, eval_time = 1:3)

