# autoplot-ting survival models with dynamic metric

    Code
      set.seed(2193)
      bayes_dynamic_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, metrics = dyn_mtrc, eval_time = time_points, control = bctrl,
      initial = grid_dynamic_res)
    Condition
      Warning in `tune_bayes()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      dyn_mult_param <- autoplot(bayes_dynamic_res, type = "parameters", eval_time = c(
        10, 15))
    Condition
      Warning:
      Evaluation times are not required with `autoplot(..., type = 'parameters')`.

# autoplot-ting survival models with different metric types

    Code
      set.seed(2193)
      bayes_mixed_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, metrics = mix_mtrc, eval_time = time_points, initial = grid_mixed_res,
      control = bctrl)
    Condition
      Warning in `tune_bayes()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      mix_mult_param <- autoplot(bayes_mixed_res, type = "parameters", eval_time = c(
        10, 15))
    Condition
      Warning:
      Evaluation times are not required with `autoplot(..., type = 'parameters')`.

# autoplot() warning for unneeded evaluation times

    Code
      p1 <- autoplot(tune_res, eval_time = 10, metric = "concordance_survival")
    Condition
      Warning in `autoplot()`:
      Evaluation times are only required when the results of a dynamic survival metric are being visualized (and will be ignored).

---

    Code
      p2 <- autoplot(tune_res, eval_time = 10, metric = "brier_survival_integrated")
    Condition
      Warning in `autoplot()`:
      Evaluation times are only required when the results of a dynamic survival metric are being visualized (and will be ignored).

