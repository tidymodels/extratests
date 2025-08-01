# resampling survival models with static metric

    Code
      set.seed(2193)
      rs_static_res <- mod_spec %>% fit_resamples(event_time ~ X1 + X2, resamples = sim_rs,
      metrics = stc_mtrc, control = rsctrl, eval_time = time_points)
    Condition
      Warning in `fit_resamples()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

# resampling survival models mixture of metric types

    Code
      show_best(rs_mixed_res, metric = "brier_survival")
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time  mean     n std_err .config        
        <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>          
      1 brier_survival standard           10 0.161    10  0.0203 pre0_mod0_post0

---

    Code
      show_best(rs_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time   mean     n std_err .config        
        <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>          
      1 brier_survival standard            1 0.0210    10 0.00497 pre0_mod0_post0

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
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time   mean     n std_err .config        
        <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>          
      1 brier_survival standard            1 0.0210    10 0.00497 pre0_mod0_post0

---

    Code
      show_best(rs_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 7
        .metric                   .estimator .eval_time  mean     n std_err .config   
        <chr>                     <chr>           <dbl> <dbl> <int>   <dbl> <chr>     
      1 brier_survival_integrated standard           NA 0.123    10 0.00913 pre0_mod0~

