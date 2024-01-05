# sim annealing tuning survival models with dynamic metric

    No evaluation time was set; a value of 5 was used.

# sim annealing tuning survival models with mixture of metric types

    No evaluation time was set; a value of 5 was used.

---

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 9
        trees .metric        .estimator .eval_time   mean     n std_err .config  .iter
        <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>    <int>
      1    20 brier_survival standard            1 0.0207    10 0.00505 initial~     0
      2    15 brier_survival standard            1 0.0208    10 0.00508 Iter1        1
      3    13 brier_survival standard            1 0.0209    10 0.00509 Iter2        2
      4     5 brier_survival standard            1 0.0210    10 0.00515 initial~     0
      5     1 brier_survival standard            1 0.0211    10 0.00518 initial~     0

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1.001))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1 is not in the results.

---

    2 evaluation times were specified during tuning; the first (1) will be used.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 9
        trees .metric          .estimator .eval_time  mean     n std_err .config .iter
        <dbl> <chr>            <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1    20 brier_survival_~ standard           NA 0.129    10 0.00812 initia~     0
      2    15 brier_survival_~ standard           NA 0.134    10 0.00807 Iter1       1
      3    13 brier_survival_~ standard           NA 0.136    10 0.00804 Iter2       2
      4     5 brier_survival_~ standard           NA 0.150    10 0.00839 initia~     0
      5     1 brier_survival_~ standard           NA 0.163    10 0.00886 initia~     0

