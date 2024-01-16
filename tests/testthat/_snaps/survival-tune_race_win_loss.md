# race tuning (win_loss) survival models with integrated metric

    Code
      num_final_wl <- show_best(wl_integrated_res, metric = "brier_survival_integrated",
        eval_time = 5) %>% pluck("cost_complexity") %>% unique()
    Condition
      Warning:
      An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).

# race tuning (win_loss) survival models with dynamic metrics

    Code
      set.seed(2193)
      wl_dyn_res <- mod_spec %>% tune_race_win_loss(event_time ~ X1 + X2, resamples = sim_rs,
      grid = grid, metrics = dyn_mtrc, eval_time = time_points, control = rctrl)
    Condition
      Warning:
      4 evaluation times are available; the first (10) will be used.

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
      Warning:
      4 evaluation times are available; the first (10) will be used.

---

    Code
      expect_snapshot_plot(print(autoplot(wl_mixed_res)), "mix-wl-race-0-times")
    Condition
      Warning in `filter_plot_eval_time()`:
      No evaluation time was set; a value of 5 was used.

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival")
    Condition
      Warning:
      4 evaluation times are available; the first (10) will be used.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1        7.94e-11 brier_survi~ standard           10 0.633    30  0.0110 Prepro~
      2        8.41e-11 brier_survi~ standard           10 0.633    30  0.0110 Prepro~
      3        8.91e-11 brier_survi~ standard           10 0.633    30  0.0110 Prepro~
      4        9.44e-11 brier_survi~ standard           10 0.633    30  0.0110 Prepro~
      5        1   e-10 brier_survi~ standard           10 0.633    30  0.0110 Prepro~

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1        7.94e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      2        8.41e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      3        8.91e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      4        9.44e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      5        1   e-10 brier_survi~ standard            1 0.177    30 0.00707 Prepro~

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1.1))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Warning:
      2 evaluation times are available; the first (1) will be used.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1        7.94e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      2        8.41e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      3        8.91e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      4        9.44e-11 brier_survi~ standard            1 0.177    30 0.00707 Prepro~
      5        1   e-10 brier_survi~ standard            1 0.177    30 0.00707 Prepro~

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival_integrated")
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1        7.94e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      2        8.41e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      3        8.91e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      4        9.44e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      5        1   e-10 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~

