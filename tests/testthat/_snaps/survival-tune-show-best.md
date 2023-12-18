# show_best with censored data - integrated metric - grid

    No value of `metric` was given; "brier_survival_integrated" will be used.

# show_best with censored data - dynamic metric - bayes

    No value of `metric` was given; "brier_survival" will be used.

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

    No value of `metric` was given; "concordance_survival" will be used.

---

    Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric (and will be ignored).

---

    Code
      show_best(race_stc_res, metric = "brier_survival_integrated")
    Condition
      Warning:
      Metric "concordance_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "concordance_survival".

# show_best with censored data - static metric (+dyn) - W/L racing

    Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric (and will be ignored).

---

    No value of `metric` was given; "concordance_survival" will be used.

---

    Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric (and will be ignored).

---

    Code
      show_best(race_stc_res, metric = "brier_survival_integrated")
    Condition
      Warning:
      Metric "concordance_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "concordance_survival" and "brier_survival".

# show_best with censored data - dyn metric (+stc) - W/L racing

    No value of `metric` was given; "brier_survival" will be used.

---

    Metric "brier_survival" was used to evaluate model candidates in the race but "concordance_survival" has been chosen to rank the candidates. These results may not agree with the race.

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
      Warning:
      Metric "brier_survival" was used to evaluate model candidates in the race but "brier_survival_integrated" has been chosen to rank the candidates. These results may not agree with the race.
      Error in `show_best()`:
      ! "brier_survival_integrated" was not in the metric set. Please choose from: "brier_survival" and "concordance_survival".

