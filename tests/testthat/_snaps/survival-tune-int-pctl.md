# percentile internals for survival models with static metric

    Code
      static_int <- int_pctl(rs_static_res, times = 10)
    Condition
      Warning in `rsample::int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `concordance_survival`.

---

    Code
      static_int_45 <- int_pctl(rs_static_res, times = 10, alpha = 0.45)
    Condition
      Warning in `rsample::int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `concordance_survival`.

# percentile internals for survival models with integrated metric

    Code
      integrated_int <- int_pctl(grid_integrated_res, times = 10)
    Condition
      Warning in `rsample::int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `brier_survival_integrated`.

# percentile internals for survival models with dynamic metrics

    Code
      dyn_int <- int_pctl(aov_dyn_res, times = 10)
    Condition
      Warning in `rsample::int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `brier_survival`.

# percentile internals for survival models mixture of metric types

    Code
      mixed_int <- int_pctl(rs_mixed_res, times = 10)
    Condition
      Warning in `rsample::int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for terms `brier_survival`, `brier_survival_integrated`, `concordance_survival`, and `royston_survival`.

# percentile internals for subset of eval times

    Code
      mixed_int <- int_pctl(rs_mixed_res, times = 10, eval_time = c(10, 5))
    Condition
      Warning in `rsample::int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for terms `brier_survival`, `brier_survival_integrated`, and `concordance_survival`.

