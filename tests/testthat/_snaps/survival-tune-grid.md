# grid tuning survival models 

    No evaluation time was set; a value of 5 was used.

---

    No evaluation time was set; a value of 5 was used.

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 3 x 8
        tree_depth .metric        .estimator .eval_time   mean     n std_err .config  
             <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>    
      1          1 brier_survival standard            1 0.0209    10 0.00501 Preproce~
      2         10 brier_survival standard            1 0.0210    10 0.00496 Preproce~
      3          2 brier_survival standard            1 0.0210    10 0.00499 Preproce~

---

    No evaluation times matched a value of 1.001.

---

    Please pick a single evaluation time point.

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 3 x 8
        tree_depth .metric           .estimator .eval_time  mean     n std_err .config
             <dbl> <chr>             <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1         10 brier_survival_i~ standard           NA 0.125    10 0.00897 Prepro~
      2          2 brier_survival_i~ standard           NA 0.137    10 0.00884 Prepro~
      3          1 brier_survival_i~ standard           NA 0.143    10 0.00865 Prepro~

---

    No evaluation time was set; a value of 5 was used.

