# Bayesian tuning survival models with dynamic metric

    Code
      set.seed(2193)
      bayes_dynamic_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, metrics = dyn_mtrc, eval_time = time_points, control = bctrl,
      initial = init_grid_dynamic_res)
    Condition
      Warning in `tune_bayes()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      expect_snapshot_plot(print(autoplot(bayes_dynamic_res)), "dyn-bayes")

# Bayesian tuning survival models with mixture of metric types

    Code
      set.seed(2193)
      bayes_mixed_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, metrics = mix_mtrc, eval_time = time_points, initial = init_grid_mixed_res,
      control = bctrl)
    Condition
      Warning in `tune_bayes()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      expect_snapshot_plot(print(autoplot(bayes_mixed_res)), "mix-bayes-0-times")

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival")
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 9
        tree_depth .metric     .estimator .eval_time  mean     n std_err .config .iter
             <dbl> <chr>       <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1         10 brier_surv~ standard           10 0.164    10  0.0198 pre0_m~     0
      2          7 brier_surv~ standard           10 0.164    10  0.0198 iter1       1
      3         13 brier_surv~ standard           10 0.164    10  0.0198 iter2       2
      4          2 brier_surv~ standard           10 0.179    10  0.0209 pre0_m~     0
      5          1 brier_surv~ standard           10 0.193    10  0.0201 pre0_m~     0

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 9
        tree_depth .metric    .estimator .eval_time   mean     n std_err .config .iter
             <dbl> <chr>      <chr>           <dbl>  <dbl> <int>   <dbl> <chr>   <int>
      1          1 brier_sur~ standard            1 0.0209    10 0.00501 pre0_m~     0
      2         10 brier_sur~ standard            1 0.0210    10 0.00496 pre0_m~     0
      3          7 brier_sur~ standard            1 0.0210    10 0.00496 iter1       1
      4         13 brier_sur~ standard            1 0.0210    10 0.00496 iter2       2
      5          2 brier_sur~ standard            1 0.0210    10 0.00499 pre0_m~     0

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1.1))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 5 x 9
        tree_depth .metric    .estimator .eval_time   mean     n std_err .config .iter
             <dbl> <chr>      <chr>           <dbl>  <dbl> <int>   <dbl> <chr>   <int>
      1          1 brier_sur~ standard            1 0.0209    10 0.00501 pre0_m~     0
      2         10 brier_sur~ standard            1 0.0210    10 0.00496 pre0_m~     0
      3          7 brier_sur~ standard            1 0.0210    10 0.00496 iter1       1
      4         13 brier_sur~ standard            1 0.0210    10 0.00496 iter2       2
      5          2 brier_sur~ standard            1 0.0210    10 0.00499 pre0_m~     0

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 9
        tree_depth .metric     .estimator .eval_time  mean     n std_err .config .iter
             <dbl> <chr>       <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1         10 brier_surv~ standard           NA 0.125    10 0.00897 pre0_m~     0
      2          7 brier_surv~ standard           NA 0.125    10 0.00897 iter1       1
      3         13 brier_surv~ standard           NA 0.125    10 0.00897 iter2       2
      4          2 brier_surv~ standard           NA 0.137    10 0.00884 pre0_m~     0
      5          1 brier_surv~ standard           NA 0.143    10 0.00865 pre0_m~     0

