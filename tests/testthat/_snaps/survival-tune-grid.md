# grid tuning survival models with dynamic metric

    Code
      expect_snapshot_plot(print(autoplot(grid_dynamic_res)), "dyn-grid")

# grid tuning survival models mixture of metric types

    Code
      expect_snapshot_plot(print(autoplot(grid_mixed_res)), "mix-grid-0-times")

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival")
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 3 x 8
        penalty .metric        .estimator .eval_time  mean     n std_err .config      
          <dbl> <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>        
      1  0.0001 brier_survival standard           10 0.154    10  0.0210 pre0_mod1_po~
      2  0.01   brier_survival standard           10 0.154    10  0.0210 pre0_mod2_po~
      3  0.1    brier_survival standard           10 0.159    10  0.0209 pre0_mod3_po~

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 3 x 8
        penalty .metric        .estimator .eval_time   mean     n std_err .config     
          <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>       
      1  0.1    brier_survival standard            1 0.0208    10 0.00503 pre0_mod3_p~
      2  0.01   brier_survival standard            1 0.0208    10 0.00498 pre0_mod2_p~
      3  0.0001 brier_survival standard            1 0.0208    10 0.00498 pre0_mod1_p~

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1.001))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1 is not in the results.

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 3 x 8
        penalty .metric        .estimator .eval_time   mean     n std_err .config     
          <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>       
      1  0.1    brier_survival standard            1 0.0208    10 0.00503 pre0_mod3_p~
      2  0.01   brier_survival standard            1 0.0208    10 0.00498 pre0_mod2_p~
      3  0.0001 brier_survival standard            1 0.0208    10 0.00498 pre0_mod1_p~

---

    Code
      show_best(grid_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 3 x 8
        penalty .metric              .estimator .eval_time  mean     n std_err .config
          <dbl> <chr>                <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1  0.0001 brier_survival_inte~ standard           NA 0.113    10 0.00941 pre0_m~
      2  0.01   brier_survival_inte~ standard           NA 0.113    10 0.00941 pre0_m~
      3  0.1    brier_survival_inte~ standard           NA 0.116    10 0.00934 pre0_m~

