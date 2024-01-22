# select_*() with static metric

    Code
      select_best(grid_static_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "concordance_survival" will be used.
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1  0.0129 Preprocessor1_Model02

---

    Code
      select_best(grid_static_res, metric = "concordance_survival")
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1  0.0129 Preprocessor1_Model02

---

    Code
      select_best(grid_static_res, metric = "brier_survival_integrated")
    Condition
      Error in `select_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "concordance_survival".

---

    Code
      select_best(grid_static_res, metric = "concordance_survival", eval_time = 0)
    Condition
      Warning in `show_best()`:
      An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1  0.0129 Preprocessor1_Model02

---

    Code
      select_by_one_std_err(grid_static_res, metric = "concordance_survival", penalty)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_by_pct_loss(grid_static_res, metric = "concordance_survival", penalty)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

# grid tuning survival models with integrated metric

    Code
      select_best(grid_integrated_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "brier_survival_integrated" will be used.
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_integrated_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_integrated_res, metric = "brier_survival_integrated",
        eval_time = 0)
    Condition
      Warning in `show_best()`:
      An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_by_one_std_err(grid_integrated_res, metric = "brier_survival_integrated",
        penalty)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_by_pct_loss(grid_integrated_res, metric = "brier_survival_integrated",
        penalty)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

# grid tuning survival models with dynamic metric

    Code
      select_best(grid_dynamic_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_dynamic_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_dynamic_res, metric = "brier_survival", eval_time = c(5, 10))
    Condition
      Warning:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 5`).
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_dynamic_res, metric = "brier_survival_integrated")
    Condition
      Error in `select_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "brier_survival".

---

    Code
      select_best(grid_dynamic_res, metric = "brier_survival", eval_time = 0)
    Condition
      Error in `show_best()`:
      ! Evaluation time 0 is not in the results.

---

    Code
      select_by_one_std_err(grid_dynamic_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_by_pct_loss(grid_dynamic_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

# grid tuning survival models mixture of metric types

    Code
      select_best(grid_mixed_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_mixed_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_mixed_res, metric = "brier_survival_integrated", eval_time = 0)
    Condition
      Warning in `show_best()`:
      An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_best(grid_mixed_res, metric = "brier_survival", eval_time = 0)
    Condition
      Error in `show_best()`:
      ! Evaluation time 0 is not in the results.

---

    Code
      select_by_one_std_err(grid_mixed_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

---

    Code
      select_by_pct_loss(grid_mixed_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config              
          <dbl> <chr>                
      1    0.01 Preprocessor1_Model01

