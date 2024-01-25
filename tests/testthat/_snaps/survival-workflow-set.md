# resampling survival models with static metric

    Code
      wflow_set_fit_stc_eval <- workflow_map(wflow_set, "fit_resamples", seed = 2193,
        resamples = sim_rs, metrics = stc_mtrc, eval_time = time_points, control = ctrl)
    Condition
      Warning in `tune::fit_resamples()`:
      Evaluation times are only required when dynamic or integrated metrics are used (and will be ignored here).
      Warning in `tune::fit_resamples()`:
      Evaluation times are only required when dynamic or integrated metrics are used (and will be ignored here).

