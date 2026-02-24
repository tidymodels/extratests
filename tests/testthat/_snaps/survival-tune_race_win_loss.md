# race tuning (win_loss) survival models with static metric

    Code
      ggplot2::get_labs(stc_race_plot)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Analysis Stage"
      
      $y
      [1] "concordance_survival"
      
      $y.sec
      NULL
      
      $group
      [1] ".config"
      
      $colour
      [1] ".config"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(stc_autoplot)
    Output
      $alpha
      [1] "# resamples"
      
      $size
      [1] "# resamples"
      
      $x.sec
      NULL
      
      $x
                  cost_complexity 
      "Cost-Complexity Parameter" 
      
      $y
      [1] "concordance_survival"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

# race tuning (win_loss) survival models with integrated metric

    Code
      num_final_wl <- show_best(wl_integrated_res, metric = "brier_survival_integrated",
        eval_time = 5) %>% pluck("cost_complexity") %>% unique()
    Condition
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.

---

    Code
      ggplot2::get_labs(int_race_plot)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Analysis Stage"
      
      $y
      [1] "brier_survival_integrated"
      
      $y.sec
      NULL
      
      $group
      [1] ".config"
      
      $colour
      [1] ".config"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(int_autoplot)
    Output
      $alpha
      [1] "# resamples"
      
      $size
      [1] "# resamples"
      
      $x.sec
      NULL
      
      $x
                  cost_complexity 
      "Cost-Complexity Parameter" 
      
      $y
      [1] "brier_survival_integrated"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

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
      ggplot2::get_labs(dyn_race_plot)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Analysis Stage"
      
      $y
      [1] "brier_survival"
      
      $y.sec
      NULL
      
      $group
      [1] ".config"
      
      $colour
      [1] ".config"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_autoplot)
    Output
      $alpha
      [1] "# resamples"
      
      $size
      [1] "# resamples"
      
      $x.sec
      NULL
      
      $x
                  cost_complexity 
      "Cost-Complexity Parameter" 
      
      $y
      [1] "brier_survival @10"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

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
      ggplot2::get_labs(mix_race_plot)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Analysis Stage"
      
      $y
      [1] "brier_survival"
      
      $y.sec
      NULL
      
      $group
      [1] ".config"
      
      $colour
      [1] ".config"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_autoplot)
    Output
      $alpha
      [1] "# resamples"
      
      $size
      [1] "# resamples"
      
      $x.sec
      NULL
      
      $x
                  cost_complexity 
      "Cost-Complexity Parameter" 
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_multi_autoplot)
    Output
      $alpha
      [1] "# resamples"
      
      $size
      [1] "# resamples"
      
      $x.sec
      NULL
      
      $x
                  cost_complexity 
      "Cost-Complexity Parameter" 
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_alt_autoplot)
    Output
      $alpha
      [1] "# resamples"
      
      $size
      [1] "# resamples"
      
      $x.sec
      NULL
      
      $x
                  cost_complexity 
      "Cost-Complexity Parameter" 
      
      $y
      [1] "concordance_survival"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

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

# race tuning (win_loss) survival models mixture of metric types including linear_pred

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = 1) %>% select(
        -.estimator, -.config)
    Output
      # A tibble: 1 x 6
        penalty .metric        .eval_time   mean     n std_err
          <dbl> <chr>               <dbl>  <dbl> <int>   <dbl>
      1    0.01 brier_survival          1 0.0192    30 0.00158

---

    Code
      show_best(wl_mixed_res, metric = "royston_survival", eval_time = 1) %>% select(
        -.estimator, -.config)
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "royston_survival" has been chosen to rank the candidates. These results may not agree with the race.
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 6
        penalty .metric          .eval_time  mean     n std_err
          <dbl> <chr>                 <dbl> <dbl> <int>   <dbl>
      1    0.01 royston_survival         NA 0.433    30  0.0111

# race tuning (W/L) - unneeded eval_time

    Code
      tune_res <- linear_reg(penalty = tune(), engine = "glmnet") %>%
        tune_race_win_loss(mpg ~ ., resamples = vfold_cv(mtcars, 5), metrics = metric_set(
          rmse), eval_time = 10)
    Condition
      Warning in `tune_race_win_loss()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      tune_res <- proportional_hazards(penalty = tune(), engine = "glmnet") %>%
        tune_race_win_loss(surv ~ ., resamples = vfold_cv(lung_surv, 5), metrics = metric_set(
          concordance_survival), eval_time = 10)
    Condition
      Warning in `tune_race_win_loss()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

