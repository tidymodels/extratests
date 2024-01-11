# race tuning (anova) survival models with integrated metric

    An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).

# race tuning (anova) survival models with dynamic metrics

    4 evaluation times were specified during tuning; the first (10) will be used.

# race tuning (anova) survival models with mixture of metric types

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0841 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      2          0.0891 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      3          0.0944 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      4          0.1    brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      5          0.0794 brier_survi~ standard            1 0.202    30 0.00418 Prepro~

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.1))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    2 evaluation times were specified during tuning; the first (1) will be used.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival_integrated")
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0794 brier_survi~ standard           NA 0.338    30 0.00487 Prepro~
      2          0.0841 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      3          0.0891 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      4          0.0944 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      5          0.1    brier_survi~ standard           NA 0.338    30 0.00480 Prepro~

