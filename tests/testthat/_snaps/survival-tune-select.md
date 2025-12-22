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
      1  0.0129 pre0_mod02_post0

---

    Code
      select_best(grid_static_res, metric = "concordance_survival")
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1  0.0129 pre0_mod02_post0

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
      Warning in `select_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1  0.0129 pre0_mod02_post0

---

    Code
      select_by_one_std_err(grid_static_res, metric = "concordance_survival", penalty)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_by_pct_loss(grid_static_res, metric = "concordance_survival", penalty)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

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
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_integrated_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_integrated_res, metric = "brier_survival_integrated",
        eval_time = 0)
    Condition
      Warning in `select_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_by_one_std_err(grid_integrated_res, metric = "brier_survival_integrated",
        penalty)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_by_pct_loss(grid_integrated_res, metric = "brier_survival_integrated",
        penalty)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

# grid tuning survival models with dynamic metric

    Code
      select_best(grid_dynamic_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `select_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_dynamic_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_dynamic_res, metric = "brier_survival", eval_time = c(5, 10))
    Condition
      Warning in `select_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 5`).
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

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
      Error in `select_best()`:
      ! Evaluation time 0 is not in the results.

---

    Code
      select_by_one_std_err(grid_dynamic_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_by_pct_loss(grid_dynamic_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

# select_*() with linear pred metric

    Code
      select_best(grid_static_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "royston_survival" will be used.
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1  0.0129 pre0_mod02_post0

---

    Code
      select_best(grid_static_res, metric = "royston_survival")
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1  0.0129 pre0_mod02_post0

---

    Code
      select_best(grid_static_res, metric = "royston_survival", eval_time = 0)
    Condition
      Warning in `select_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1  0.0129 pre0_mod02_post0

---

    Code
      select_by_one_std_err(grid_static_res, metric = "royston_survival", penalty)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_by_pct_loss(grid_static_res, metric = "royston_survival", penalty)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

# grid tuning survival models mixture of metric types

    Code
      select_best(grid_mixed_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `select_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_mixed_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_mixed_res, metric = "brier_survival_integrated", eval_time = 0)
    Condition
      Warning in `select_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_best(grid_mixed_res, metric = "brier_survival", eval_time = 0)
    Condition
      Error in `select_best()`:
      ! Evaluation time 0 is not in the results.

---

    Code
      select_by_one_std_err(grid_mixed_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

---

    Code
      select_by_pct_loss(grid_mixed_res, metric = "brier_survival", penalty,
        eval_time = 10)
    Output
      # A tibble: 1 x 2
        penalty .config         
          <dbl> <chr>           
      1    0.01 pre0_mod01_post0

