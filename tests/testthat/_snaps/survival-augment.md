# augment() works for tune_results

    Code
      aug_res <- augment(grid_mixed_res)
    Condition
      Warning in `augment()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `select_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      aug_res <- augment(grid_mixed_res, eval_time = 10)
    Condition
      Warning in `augment()`:
      No value of `metric` was given; "brier_survival" will be used.

