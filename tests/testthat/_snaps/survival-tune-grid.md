# grid tuning survival models with dynamic metric

    No evaluation time was set; a value of 5 was used.

# grid tuning survival models mixture of metric types

    No evaluation time was set; a value of 5 was used.

---

    No evaluation time was set; a value of 5 was used.

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 3 x 8
        penalty .metric        .estimator .eval_time   mean     n std_err .config     
          <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>       
      1  0.1    brier_survival standard            1 0.0208    10 0.00503 Preprocesso~
      2  0.01   brier_survival standard            1 0.0208    10 0.00498 Preprocesso~
      3  0.0001 brier_survival standard            1 0.0208    10 0.00498 Preprocesso~

---

    No evaluation times matched a value of 1.001.

---

    Please pick a single evaluation time point.

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 3 x 8
        penalty .metric              .estimator .eval_time  mean     n std_err .config
          <dbl> <chr>                <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1  0.0001 brier_survival_inte~ standard           NA 0.113    10 0.00941 Prepro~
      2  0.01   brier_survival_inte~ standard           NA 0.113    10 0.00941 Prepro~
      3  0.1    brier_survival_inte~ standard           NA 0.116    10 0.00934 Prepro~

