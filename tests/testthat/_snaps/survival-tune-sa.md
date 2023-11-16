# sim annealing tuning survival models with dynamic metric

    No evaluation time was set; a value of 10 was used.

# sim annealing tuning survival models with mixture of metric types

    No evaluation time was set; a value of 10 was used.

---

    No evaluation time was set; a value of 10 was used.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 3 x 9
        trees .metric        .estimator .eval_time   mean     n std_err .config  .iter
        <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>    <int>
      1    20 brier_survival standard            1 0.0207    10 0.00505 initial~     0
      2     5 brier_survival standard            1 0.0210    10 0.00515 initial~     0
      3     1 brier_survival standard            1 0.0211    10 0.00518 initial~     0

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1.001))
    Condition
      Error in `choose_eval_time()`:
      ! No evaluation times matched a value of 1.001.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Error in `choose_eval_time()`:
      ! Please pick a single evaluation time point.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 9
        trees .metric          .estimator .eval_time  mean     n std_err .config .iter
        <dbl> <chr>            <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1    15 brier_survival_~ standard           NA 0        10 0       Iter1       1
      2    13 brier_survival_~ standard           NA 0        10 0       Iter2       2
      3    20 brier_survival_~ standard           NA 0.129    10 0.00812 initia~     0
      4     5 brier_survival_~ standard           NA 0.150    10 0.00839 initia~     0
      5     1 brier_survival_~ standard           NA 0.163    10 0.00886 initia~     0

