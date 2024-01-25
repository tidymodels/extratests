# race tuning (win_loss) survival models with integrated metric

    Code
      num_final_wl <- show_best(wl_integrated_res, metric = "brier_survival_integrated",
        eval_time = 5) %>% pluck("cost_complexity") %>% unique()
    Condition
      Warning in `show_best()`:
      An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).

# race tuning (win_loss) survival models with dynamic metrics

    Code
      set.seed(2193)
      wl_dyn_res <- mod_spec %>% tune_race_win_loss(event_time ~ X1 + X2, resamples = sim_rs,
      grid = grid, metrics = dyn_mtrc, eval_time = time_points, control = rctrl)
    Condition
      Warning in `tune_race_win_loss()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      expect_snapshot_plot(print(autoplot(wl_dyn_res)), "dyn-wl-race-0-times")
    Condition
      Warning in `filter_plot_eval_time()`:
      No evaluation time was set; a value of 5 was used.

# race tuning (win_loss) survival models with mixture of metric types

    Code
      set.seed(2193)
      wl_mixed_res <- mod_spec %>% tune_race_win_loss(event_time ~ X1 + X2,
      resamples = sim_rs, grid = grid_ties, metrics = mix_mtrc, eval_time = time_points,
      control = rctrl)
    Condition
      Warning in `tune_race_win_loss()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      expect_snapshot_plot(print(autoplot(wl_mixed_res)), "mix-wl-race-0-times")
    Condition
      Warning in `filter_plot_eval_time()`:
      No evaluation time was set; a value of 5 was used.

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival") %>% select(-.estimator,
        -.config)
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 6
        cost_complexity .metric        .eval_time  mean     n std_err
                  <dbl> <chr>               <dbl> <dbl> <int>   <dbl>
      1        7.94e-11 brier_survival         10 0.633    30  0.0110
      2        8.41e-11 brier_survival         10 0.633    30  0.0110
      3        8.91e-11 brier_survival         10 0.633    30  0.0110
      4        9.44e-11 brier_survival         10 0.633    30  0.0110
      5        1   e-10 brier_survival         10 0.633    30  0.0110

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = 1) %>% select(
        -.estimator, -.config)
    Output
      # A tibble: 5 x 6
        cost_complexity .metric        .eval_time  mean     n std_err
                  <dbl> <chr>               <dbl> <dbl> <int>   <dbl>
      1        7.94e-11 brier_survival          1 0.177    30 0.00707
      2        8.41e-11 brier_survival          1 0.177    30 0.00707
      3        8.91e-11 brier_survival          1 0.177    30 0.00707
      4        9.44e-11 brier_survival          1 0.177    30 0.00707
      5        1   e-10 brier_survival          1 0.177    30 0.00707

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1.1)) %>%
        select(-.estimator, -.config)
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1, 3)) %>%
        select(-.estimator, -.config)
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 5 x 6
        cost_complexity .metric        .eval_time  mean     n std_err
                  <dbl> <chr>               <dbl> <dbl> <int>   <dbl>
      1        7.94e-11 brier_survival          1 0.177    30 0.00707
      2        8.41e-11 brier_survival          1 0.177    30 0.00707
      3        8.91e-11 brier_survival          1 0.177    30 0.00707
      4        9.44e-11 brier_survival          1 0.177    30 0.00707
      5        1   e-10 brier_survival          1 0.177    30 0.00707

---

    Code
      res <- show_best(wl_mixed_res, metric = "unused_metric", eval_time = c(1, 3)) %>%
        select(-.estimator, -.config)
    Condition
      Error in `show_best()`:
      ! "unused_metric" was not in the metric set. Please choose from: "brier_survival", "brier_survival_integrated", and "concordance_survival".

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival_integrated") %>% select(
        -.estimator, -.config)
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
    Output
      # A tibble: 5 x 6
        cost_complexity .metric                   .eval_time  mean     n std_err
                  <dbl> <chr>                          <dbl> <dbl> <int>   <dbl>
      1        7.94e-11 brier_survival_integrated         NA 0.285    30 0.00426
      2        8.41e-11 brier_survival_integrated         NA 0.285    30 0.00426
      3        8.91e-11 brier_survival_integrated         NA 0.285    30 0.00426
      4        9.44e-11 brier_survival_integrated         NA 0.285    30 0.00426
      5        1   e-10 brier_survival_integrated         NA 0.285    30 0.00426

# race tuning (W/L) - unneeded eval_time

    Code
      tune_res <- linear_reg(penalty = tune(), engine = "glmnet") %>%
        tune_race_win_loss(mpg ~ ., resamples = vfold_cv(mtcars, 5), metrics = metric_set(
          rmse), eval_time = 10)
    Condition
      Warning in `tune_race_win_loss()`:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      tune_res <- proportional_hazards(penalty = tune(), engine = "glmnet") %>%
        tune_race_win_loss(surv ~ ., resamples = vfold_cv(lung_surv, 5), metrics = metric_set(
          concordance_survival), eval_time = 10)
    Condition
      Warning in `tune_race_win_loss()`:
      Evaluation times are only required when dynamic or integrated metrics are used (and will be ignored here).

