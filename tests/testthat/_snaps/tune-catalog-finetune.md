# interactive logger works (finetune integration, error)

    Code
      res_anova <- tune_race_anova(parsnip::decision_tree(cost_complexity = tune(),
      min_n = tune(), mode = "regression"), Sale_Price ~ ., rsample::vfold_cv(
        modeldata::ames[, c(72, 40:45)], 5), control = control_race(extract = function(
        x) {
        raise_warning()
        raise_error()
      }))
    Message
      > A | warning: ope! yikes.
      > B | error:   AHHhH
      There were issues with some computations   A: x39   B: x39

---

    Code
      res_sa <- tune_sim_anneal(parsnip::decision_tree(cost_complexity = tune(),
      min_n = tune(), mode = "regression"), Sale_Price ~ ., rsample::vfold_cv(
        modeldata::ames[, c(72, 40:45)], 5), initial = res_anova, iter = 15, control = control_sim_anneal(
        verbose_iter = FALSE, extract = function(x) {
          raise_warning()
          raise_error()
        }))
    Message
      > A | warning: ope! yikes.
      > B | error:   AHHhH
      There were issues with some computations   A: x75   B: x75

