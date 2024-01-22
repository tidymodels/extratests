# tune*_() saves eval_time

    Code
      set.seed(2193)
      bayes_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, sim_rs, initial = grid_res,
      iter = 2, metrics = srv_mtrc, eval_time = time_points)
    Condition
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      set.seed(2193)
      sa_res <- mod_spec %>% tune_sim_anneal(event_time ~ X1 + X2, sim_rs, initial = grid_res,
      iter = 2, metrics = srv_mtrc, eval_time = time_points, control = control_sim_anneal(
        verbose_iter = FALSE))
    Condition
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      set.seed(2193)
      anova_res <- mod_spec %>% tune_race_anova(event_time ~ X1 + X2, sim_rs, grid = grid,
      metrics = srv_mtrc, eval_time = time_points)
    Condition
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      set.seed(2193)
      wl_res <- mod_spec %>% tune_race_win_loss(event_time ~ X1 + X2, sim_rs, grid = grid,
      metrics = srv_mtrc, eval_time = time_points)
    Condition
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

