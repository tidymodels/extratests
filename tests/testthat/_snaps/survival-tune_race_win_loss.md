# race tuning (win_loss) survival models with integrated metric

    Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric (and will be ignored).

# race tuning (win_loss) survival models with dynamic metrics

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    No evaluation time was set; a value of 5 was used.

# race tuning (win_loss) survival models with mixture of metric types

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    No evaluation time was set; a value of 5 was used.

---

    4 evaluation times were specified during tuning; the first (10) will be used.

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

    2 evaluation times were specified during tuning; the first (1) will be used.

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

