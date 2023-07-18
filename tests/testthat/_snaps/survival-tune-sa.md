# sim annealing tuning survival models with dynamic metrics

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
        tree_depth .metric    .estimator .eval_time   mean     n std_err .config .iter
             <dbl> <chr>      <chr>           <dbl>  <dbl> <int>   <dbl> <chr>   <int>
      1          1 brier_sur~ standard            1 0.0209    10 0.00501 initia~     0
      2         10 brier_sur~ standard            1 0.0210    10 0.00496 initia~     0
      3          2 brier_sur~ standard            1 0.0210    10 0.00499 initia~     0

---

    No evaluation times matched a value of 1.001.

---

    Please pick a single evaluation time point.

---

    Code
      show_best(sa_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 9
        tree_depth .metric     .estimator .eval_time  mean     n std_err .config .iter
             <dbl> <chr>       <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1          8 brier_surv~ standard           NA 0        10 0       Iter1       1
      2          7 brier_surv~ standard           NA 0        10 0       Iter2       2
      3         10 brier_surv~ standard           NA 0.125    10 0.00897 initia~     0
      4          2 brier_surv~ standard           NA 0.137    10 0.00884 initia~     0
      5          1 brier_surv~ standard           NA 0.143    10 0.00865 initia~     0

