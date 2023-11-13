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
      # A tibble: 1 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1    0.0000000001 brier_survi~ standard            1 0.177    30 0.00707 Prepro~

---

    No evaluation times matched a value of 1.001.

---

    Please pick a single evaluation time point.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1    0.0000000001 brier_survi~ standard           NA 0.285    30 0.00426 Prepro~

