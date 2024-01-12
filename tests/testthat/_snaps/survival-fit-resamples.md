# resampling survival models mixture of metric types

    Code
      show_best(rs_mixed_res, metric = "brier_survival")
    Condition
      Warning:
      4 evaluation times are available; the first (10) will be used.
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time  mean     n std_err .config             
        <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>               
      1 brier_survival standard           10 0.166    10  0.0204 Preprocessor1_Model1

---

    Code
      show_best(rs_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time   mean     n std_err .config             
        <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>               
      1 brier_survival standard            1 0.0208    10 0.00501 Preprocessor1_Model1

---

    Code
      show_best(rs_mixed_res, metric = "brier_survival", eval_time = c(1.001))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1 is not in the results.

---

    Code
      show_best(rs_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Warning:
      2 evaluation times are available; the first (1) will be used.
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time   mean     n std_err .config             
        <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>               
      1 brier_survival standard            1 0.0208    10 0.00501 Preprocessor1_Model1

---

    Code
      show_best(rs_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 7
        .metric                   .estimator .eval_time  mean     n std_err .config   
        <chr>                     <chr>           <dbl> <dbl> <int>   <dbl> <chr>     
      1 brier_survival_integrated standard           NA 0.123    10 0.00962 Preproces~

