# show_best with censored data - integrated metric - grid

    Code
      show_best(grid_int_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "brier_survival_integrated" will be used.
    Output
      # A tibble: 5 x 7
        cost_complexity .metric               .estimator    mean     n std_err .config
                  <dbl> <chr>                 <chr>        <dbl> <int>   <dbl> <chr>  
      1        0.000126 brier_survival_integ~ standard   0.00791    10 0.00137 pre0_m~
      2        0.000251 brier_survival_integ~ standard   0.00793    10 0.00137 pre0_m~
      3        0.000200 brier_survival_integ~ standard   0.00794    10 0.00136 pre0_m~
      4        0.000158 brier_survival_integ~ standard   0.00796    10 0.00134 pre0_m~
      5        0.000316 brier_survival_integ~ standard   0.00797    10 0.00152 pre0_m~

# show_best with censored data - dynamic metric - bayes

    Code
      show_best(bayes_dyn_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "brier_survival" will be used.
    Output
      # A tibble: 5 x 9
        cost_complexity .metric     .estimator .eval_time   mean     n std_err .config
                  <dbl> <chr>       <chr>           <dbl>  <dbl> <int>   <dbl> <chr>  
      1        7.81e- 5 brier_surv~ standard          100 0.0110    10 0.00316 pre0_m~
      2        1.03e-10 brier_surv~ standard          100 0.0114    10 0.00312 pre0_m~
      3        9.12e- 8 brier_surv~ standard          100 0.0114    10 0.00312 pre0_m~
      4        3.85e- 6 brier_surv~ standard          100 0.0114    10 0.00312 iter2  
      5        2.47e- 9 brier_surv~ standard          100 0.0114    10 0.00312 iter3  
      # i 1 more variable: .iter <int>

---

    Code
      show_best(bayes_dyn_res, metric = "brier_survival", eval_time = 1)
    Condition
      Error in `show_best()`:
      ! Evaluation time 1 is not in the results.

---

    Code
      show_best(bayes_dyn_res, metric = "brier_survival_integrated")
    Condition
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "brier_survival".

# show_best with censored data - static metric - anova racing

    Code
      show_best(race_stc_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "concordance_survival" will be used.
    Output
      # A tibble: 1 x 7
        cost_complexity .metric              .estimator  mean     n std_err .config   
                  <dbl> <chr>                <chr>      <dbl> <int>   <dbl> <chr>     
      1         0.00001 concordance_survival standard   0.278    10  0.0147 pre0_mod1~

---

    Code
      show_best(race_stc_res, metric = "concordance_survival", eval_time = 1)
    Condition
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 7
        cost_complexity .metric              .estimator  mean     n std_err .config   
                  <dbl> <chr>                <chr>      <dbl> <int>   <dbl> <chr>     
      1         0.00001 concordance_survival standard   0.278    10  0.0147 pre0_mod1~

---

    Code
      show_best(race_stc_res, metric = "brier_survival_integrated")
    Condition
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "concordance_survival".

# show_best with censored data - static metric (+dyn) - W/L racing

    Code
      show_best(race_stc_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "concordance_survival" will be used.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1       0.0867    concordance~ standard           NA 0.297    10  0.0118 pre0_m~
      2       0.0000192 concordance~ standard           NA 0.279    10  0.0147 pre0_m~
      3       0.0000107 concordance~ standard           NA 0.278    10  0.0147 pre0_m~
      4       0.0000384 concordance~ standard           NA 0.277    10  0.0156 pre0_m~
      5       0.000118  concordance~ standard           NA 0.270    10  0.0141 pre0_m~

---

    Code
      show_best(race_stc_res, metric = "concordance_survival", eval_time = 1)
    Condition
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1       0.0867    concordance~ standard           NA 0.297    10  0.0118 pre0_m~
      2       0.0000192 concordance~ standard           NA 0.279    10  0.0147 pre0_m~
      3       0.0000107 concordance~ standard           NA 0.278    10  0.0147 pre0_m~
      4       0.0000384 concordance~ standard           NA 0.277    10  0.0156 pre0_m~
      5       0.000118  concordance~ standard           NA 0.270    10  0.0141 pre0_m~

---

    Code
      show_best(race_stc_res, metric = "brier_survival_integrated")
    Condition
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "concordance_survival" and "brier_survival".

# show_best with censored data - dyn metric (+stc) - W/L racing

    Code
      show_best(race_dyn_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "brier_survival" will be used.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric     .estimator .eval_time   mean     n std_err .config
                  <dbl> <chr>       <chr>           <dbl>  <dbl> <int>   <dbl> <chr>  
      1       0.000327  brier_surv~ standard          100 0.0108    10 0.00329 pre0_m~
      2       0.000118  brier_surv~ standard          100 0.0110    10 0.00317 pre0_m~
      3       0.0000107 brier_surv~ standard          100 0.0114    10 0.00312 pre0_m~
      4       0.0000192 brier_surv~ standard          100 0.0114    10 0.00312 pre0_m~
      5       0.0000384 brier_surv~ standard          100 0.0114    10 0.00312 pre0_m~

---

    Code
      mutate(show_best(race_dyn_res, metric = "concordance_survival"), mean = round(
        mean, 2), std_err = round(std_err, 2))
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "concordance_survival" has been chosen to rank the candidates. These results may not agree with the race.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1       0.0000192 concordance~ standard           NA  0.28    10    0.01 pre0_m~
      2       0.0000107 concordance~ standard           NA  0.28    10    0.01 pre0_m~
      3       0.0000384 concordance~ standard           NA  0.28    10    0.02 pre0_m~
      4       0.000118  concordance~ standard           NA  0.27    10    0.01 pre0_m~
      5       0.000327  concordance~ standard           NA  0.26    10    0.01 pre0_m~

---

    Code
      show_best(race_dyn_res, metric = "brier_survival", eval_time = 1)
    Condition
      Error in `show_best()`:
      ! Evaluation time 1 is not in the results.

---

    Code
      show_best(race_dyn_res, metric = "brier_survival_integrated")
    Condition
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "brier_survival" and "concordance_survival".

# show_best with censored data - linpred metric (+stc) - SA

    Code
      show_best(sa_linpred_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "royston_survival" will be used.
    Output
      # A tibble: 3 x 8
           penalty .metric          .estimator  mean     n   std_err .config     .iter
             <dbl> <chr>            <chr>      <dbl> <int>     <dbl> <chr>       <int>
      1 0.00000168 royston_survival standard   1.000    10 0.0000534 initial_pr~     0
      2 0.0000470  royston_survival standard   1.000    10 0.0000534 Iter1           1
      3 0.000153   royston_survival standard   1.000    10 0.0000534 Iter2           2

---

    Code
      show_best(sa_linpred_res, metric = "concordance_survival")
    Output
      # A tibble: 3 x 8
           penalty .metric              .estimator  mean     n std_err .config   .iter
             <dbl> <chr>                <chr>      <dbl> <int>   <dbl> <chr>     <int>
      1 0.00000168 concordance_survival standard   0.938    10 0.00369 initial_~     0
      2 0.0000470  concordance_survival standard   0.938    10 0.00369 Iter1         1
      3 0.000153   concordance_survival standard   0.938    10 0.00369 Iter2         2

---

    Code
      show_best(sa_linpred_res, metric = "royston_survival", eval_time = 1)
    Condition
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 3 x 8
           penalty .metric          .estimator  mean     n   std_err .config     .iter
             <dbl> <chr>            <chr>      <dbl> <int>     <dbl> <chr>       <int>
      1 0.00000168 royston_survival standard   1.000    10 0.0000534 initial_pr~     0
      2 0.0000470  royston_survival standard   1.000    10 0.0000534 Iter1           1
      3 0.000153   royston_survival standard   1.000    10 0.0000534 Iter2           2

---

    Code
      show_best(sa_linpred_res, metric = "brier_survival")
    Condition
      Error in `show_best()`:
      ! "brier_survival" was not in the metric set. Please choose from: "royston_survival" and "concordance_survival".

