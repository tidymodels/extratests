# race tuning survival models with dynamic metrics

    No evaluation time was set; a value of 5 was used.

---

    No evaluation time was set; a value of 5 was used.

# race tuning survival models with mixture of metric types

    No evaluation time was set; a value of 5 was used.

---

    No evaluation time was set; a value of 5 was used.

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
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.001))
    Condition
      Error in `choose_eval_time()`:
      ! No evaluation times matched a value of 1.001.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Error in `choose_eval_time()`:
      ! Please pick a single evaluation time point.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0794 brier_survi~ standard           NA 0.338    30 0.00487 Prepro~
      2          0.0841 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      3          0.0891 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      4          0.0944 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      5          0.1    brier_survi~ standard           NA 0.338    30 0.00480 Prepro~

