# Bayesian tuning survival models with dynamic metric

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    No evaluation time was set; a value of 5 was used.

# Bayesian tuning survival models with mixture of metric types

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    No evaluation time was set; a value of 5 was used.

---

    4 evaluation times were specified during tuning; the first (10) will be used.

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 9
        tree_depth .metric    .estimator .eval_time   mean     n std_err .config .iter
             <dbl> <chr>      <chr>           <dbl>  <dbl> <int>   <dbl> <chr>   <int>
      1          1 brier_sur~ standard            1 0.0209    10 0.00501 Prepro~     0
      2         10 brier_sur~ standard            1 0.0210    10 0.00496 Prepro~     0
      3         15 brier_sur~ standard            1 0.0210    10 0.00496 Iter1       1
      4          6 brier_sur~ standard            1 0.0210    10 0.00496 Iter2       2
      5          2 brier_sur~ standard            1 0.0210    10 0.00499 Prepro~     0

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1.1))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    2 evaluation times were specified during tuning; the first (1) will be used.

---

    Code
      show_best(bayes_mixed_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 9
        tree_depth .metric     .estimator .eval_time  mean     n std_err .config .iter
             <dbl> <chr>       <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1         10 brier_surv~ standard           NA 0.125    10 0.00897 Prepro~     0
      2         15 brier_surv~ standard           NA 0.125    10 0.00897 Iter1       1
      3          6 brier_surv~ standard           NA 0.125    10 0.00897 Iter2       2
      4          2 brier_surv~ standard           NA 0.137    10 0.00884 Prepro~     0
      5          1 brier_surv~ standard           NA 0.143    10 0.00865 Prepro~     0

