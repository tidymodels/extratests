# race tuning (win_loss) survival models with dynamic metrics

    No evaluation time was set; a value of 5 was used.

# race tuning (win_loss) survival models with mixture of metric types

    No evaluation time was set; a value of 5 was used.

---

    No evaluation time was set; a value of 5 was used.

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
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1.001))
    Condition
      Error in `choose_eval_time()`:
      ! No evaluation times matched a value of 1.001.

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Error in `choose_eval_time()`:
      ! Please pick a single evaluation time point.

---

    Code
      show_best(wl_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1        7.94e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      2        8.41e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      3        8.91e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      4        9.44e-11 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~
      5        1   e-10 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~

