# race tuning (anova) survival models with static metric

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
      

# race tuning (anova) survival models with integrated metric

    Code
      num_final_aov <- show_best(aov_integrated_res, metric = "brier_survival_integrated",
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
      

# race tuning (anova) survival models with dynamic metrics

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
      

# race tuning (anova) survival models with mixture of metric types

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
      show_best(aov_mixed_res, metric = "brier_survival") %>% select(-.estimator,
        -.config)
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 6
        cost_complexity .metric        .eval_time  mean     n std_err
                  <dbl> <chr>               <dbl> <dbl> <int>   <dbl>
      1          0.0794 brier_survival         10 0.632    30  0.0120
      2          0.0841 brier_survival         10 0.632    30  0.0120
      3          0.0891 brier_survival         10 0.632    30  0.0120
      4          0.0944 brier_survival         10 0.632    30  0.0120
      5          0.1    brier_survival         10 0.632    30  0.0120

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = 1) %>% select(
        -.estimator, -.config)
    Output
      # A tibble: 5 x 6
        cost_complexity .metric        .eval_time  mean     n std_err
                  <dbl> <chr>               <dbl> <dbl> <int>   <dbl>
      1          0.0841 brier_survival          1 0.201    30 0.00415
      2          0.0891 brier_survival          1 0.201    30 0.00415
      3          0.0944 brier_survival          1 0.201    30 0.00415
      4          0.1    brier_survival          1 0.201    30 0.00415
      5          0.0794 brier_survival          1 0.202    30 0.00418

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.1)) %>%
        select(-.estimator, -.config)
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1, 3)) %>%
        select(-.estimator, -.config)
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 5 x 6
        cost_complexity .metric        .eval_time  mean     n std_err
                  <dbl> <chr>               <dbl> <dbl> <int>   <dbl>
      1          0.0841 brier_survival          1 0.201    30 0.00415
      2          0.0891 brier_survival          1 0.201    30 0.00415
      3          0.0944 brier_survival          1 0.201    30 0.00415
      4          0.1    brier_survival          1 0.201    30 0.00415
      5          0.0794 brier_survival          1 0.202    30 0.00418

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival_integrated") %>% select(
        -.estimator, -.config)
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
    Output
      # A tibble: 5 x 6
        cost_complexity .metric                   .eval_time  mean     n std_err
                  <dbl> <chr>                          <dbl> <dbl> <int>   <dbl>
      1          0.0794 brier_survival_integrated         NA 0.338    30 0.00487
      2          0.0841 brier_survival_integrated         NA 0.338    30 0.00480
      3          0.0891 brier_survival_integrated         NA 0.338    30 0.00480
      4          0.0944 brier_survival_integrated         NA 0.338    30 0.00480
      5          0.1    brier_survival_integrated         NA 0.338    30 0.00480

# race tuning (anova) - unneeded eval_time

    Code
      tune_res <- linear_reg(penalty = tune(), engine = "glmnet") %>% tune_race_anova(
        mpg ~ ., resamples = vfold_cv(mtcars, 5), metrics = metric_set(rmse),
        eval_time = 10)
    Condition
      Warning in `tune_race_anova()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      tune_res <- proportional_hazards(penalty = tune(), engine = "glmnet") %>%
        tune_race_anova(surv ~ ., resamples = vfold_cv(lung_surv, 5), metrics = metric_set(
          concordance_survival), eval_time = 10)
    Condition
      Warning in `tune_race_anova()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

