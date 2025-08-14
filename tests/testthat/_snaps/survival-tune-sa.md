# sim annealing tuning survival models with dynamic metric

    Code
      set.seed(2193)
      sa_dynamic_res <- mod_spec %>% tune_sim_anneal(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, param_info = mod_param, metrics = dyn_mtrc, eval_time = time_points,
      control = sctrl, initial = init_grid_dynamic_res)
    Condition
      Warning in `tune_sim_anneal()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      expect_snapshot_plot(print(autoplot(sa_dynamic_res)), "dyn-sa")

# sim annealing tuning survival models with mixture of metric types

    Code
      set.seed(2193)
      sa_mixed_res <- mod_spec %>% tune_sim_anneal(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, param_info = mod_param, metrics = mix_mtrc, eval_time = time_points,
      initial = init_grid_mixed_res, control = sctrl)
    Condition
      Warning in `tune_sim_anneal()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      expect_snapshot_plot(print(autoplot(sa_mixed_res)), "mix-sa-0-times")

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival")
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 9
        trees .metric        .estimator .eval_time  mean     n std_err .config   .iter
        <dbl> <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>     <int>
      1    20 brier_survival standard           10 0.174    10  0.0195 initial_~     0
      2    17 brier_survival standard           10 0.178    10  0.0195 Iter2         2
      3    15 brier_survival standard           10 0.180    10  0.0194 Iter1         1
      4     5 brier_survival standard           10 0.203    10  0.0197 initial_~     0
      5     1 brier_survival standard           10 0.222    10  0.0204 initial_~     0

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 9
        trees .metric        .estimator .eval_time   mean     n std_err .config  .iter
        <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>    <int>
      1    20 brier_survival standard            1 0.0207    10 0.00505 initial~     0
      2    17 brier_survival standard            1 0.0208    10 0.00507 Iter2        2
      3    15 brier_survival standard            1 0.0208    10 0.00508 Iter1        1
      4     5 brier_survival standard            1 0.0210    10 0.00515 initial~     0
      5     1 brier_survival standard            1 0.0211    10 0.00518 initial~     0

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1.1))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 5 x 9
        trees .metric        .estimator .eval_time   mean     n std_err .config  .iter
        <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>    <int>
      1    20 brier_survival standard            1 0.0207    10 0.00505 initial~     0
      2    17 brier_survival standard            1 0.0208    10 0.00507 Iter2        2
      3    15 brier_survival standard            1 0.0208    10 0.00508 Iter1        1
      4     5 brier_survival standard            1 0.0210    10 0.00515 initial~     0
      5     1 brier_survival standard            1 0.0211    10 0.00518 initial~     0

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 9
        trees .metric          .estimator .eval_time  mean     n std_err .config .iter
        <dbl> <chr>            <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1    20 brier_survival_~ standard           NA 0.129    10 0.00812 initia~     0
      2    17 brier_survival_~ standard           NA 0.132    10 0.00807 Iter2       2
      3    15 brier_survival_~ standard           NA 0.134    10 0.00807 Iter1       1
      4     5 brier_survival_~ standard           NA 0.150    10 0.00839 initia~     0
      5     1 brier_survival_~ standard           NA 0.163    10 0.00886 initia~     0

