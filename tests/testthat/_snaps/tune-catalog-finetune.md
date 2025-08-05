# interactive logger works (finetune integration, error)

    Code
      res_anova <- tune_race_anova(parsnip::nearest_neighbor("regression", "kknn",
        neighbors = tune()), Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72,
        40:45)], 5), control = control_race(allow_par = FALSE, extract = function(x) {
        raise_warning()
        raise_error()
      }))
    Message
      > A | warning: ope! yikes.
      > B | error:   AHHhH

---

    Code
      catalog_summary_test
    Output
      A: x5   B: x5

---

    Code
      res_sa <- tune_sim_anneal(parsnip::nearest_neighbor("regression", "kknn",
        neighbors = tune()), Sale_Price ~ ., rsample::vfold_cv(modeldata::ames[, c(72,
        40:45)], 5), initial = res_anova, iter = 15, control = control_sim_anneal(
        allow_par = FALSE, verbose_iter = FALSE, extract = function(x) {
          raise_warning()
          raise_error()
        }))
    Message
      > A | warning: ope! yikes.
      > B | error:   AHHhH

---

    Code
      catalog_summary_test
    Output
      A: x75   B: x75

