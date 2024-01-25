# race tuning (anova) survival models with integrated metric

    Code
      num_final_aov <- show_best(aov_integrated_res, metric = "brier_survival_integrated",
        eval_time = 5) %>% pluck("cost_complexity") %>% unique()
    Condition
      Warning in `show_best()`:
      An evaluation time is only required when a dynamic metric is selected (and `eval_time` will thus be ignored).

# race tuning (anova) survival models with dynamic metrics

    4 evaluation times are available; the first (10) will be used.

# race tuning (anova) survival models with mixture of metric types

    4 evaluation times are available; the first (10) will be used.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival")
    Condition
      Warning:
      4 evaluation times are available; the first (10) will be used.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0794 brier_survi~ standard           10 0.632    30  0.0120 Prepro~
      2          0.0841 brier_survi~ standard           10 0.632    30  0.0120 Prepro~
      3          0.0891 brier_survi~ standard           10 0.632    30  0.0120 Prepro~
      4          0.0944 brier_survi~ standard           10 0.632    30  0.0120 Prepro~
      5          0.1    brier_survi~ standard           10 0.632    30  0.0120 Prepro~

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0841 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      2          0.0891 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      3          0.0944 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      4          0.1    brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      5          0.0794 brier_survi~ standard            1 0.202    30 0.00418 Prepro~

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.1))
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
    Condition
      Warning:
      2 evaluation times are available; the first (1) will be used.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0841 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      2          0.0891 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      3          0.0944 brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      4          0.1    brier_survi~ standard            1 0.201    30 0.00415 Prepro~
      5          0.0794 brier_survi~ standard            1 0.202    30 0.00418 Prepro~

---

    Code
      show_best(aov_mixed_res, metric = "brier_survival_integrated")
    Condition
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
    Output
      # A tibble: 5 x 8
        cost_complexity .metric      .estimator .eval_time  mean     n std_err .config
                  <dbl> <chr>        <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1          0.0794 brier_survi~ standard           NA 0.338    30 0.00487 Prepro~
      2          0.0841 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      3          0.0891 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      4          0.0944 brier_survi~ standard           NA 0.338    30 0.00480 Prepro~
      5          0.1    brier_survi~ standard           NA 0.338    30 0.00480 Prepro~

# race tuning (anova) - unneeded eval_time

    Code
      tune_res <- linear_reg(penalty = tune(), engine = "glmnet") %>% tune_race_anova(
        mpg ~ ., resamples = vfold_cv(mtcars, 5), metrics = metric_set(rmse),
        eval_time = 10)
    Condition
      Warning in `tune_race_anova()`:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      tune_res <- proportional_hazards(penalty = tune(), engine = "glmnet") %>%
        tune_race_anova(surv ~ ., resamples = vfold_cv(lung_surv, 5), metrics = metric_set(
          concordance_survival), eval_time = 10)
    Condition
      Warning in `tune_race_anova()`:
      Evaluation times are only required when dynamic or integrated metrics are used (and will be ignored here).

