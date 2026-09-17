# metric inputs are checked for censored regression models

    Code
      check_metrics_arg(NULL, wflow)
    Output
      A metric set, consisting of:
      - `brier_survival()`, a dynamic survival metric | direction: minimize

---

    Code
      check_metrics_arg(met_reg, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      check_metrics_arg(met_cls, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      check_metrics_arg(met_srv, wflow)
    Output
      A metric set, consisting of:
      - `concordance_survival()`, a static survival metric | direction: maximize

---

    Code
      fit_resamples(wflow, rs, metrics = met_cls)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      fit_resamples(wflow, rs, metrics = met_reg)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_cls)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_reg)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_cls)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_reg)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_cls)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_reg)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

