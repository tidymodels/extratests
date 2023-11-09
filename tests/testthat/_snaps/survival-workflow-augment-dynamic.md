# augment survival workflows with eval_time

    Code
      augment(wflow_fit, new_data = sim_dat)
    Condition
      Error in `augment()`:
      ! The `eval_time` argument is missing, with no default.

---

    Code
      workflow() %>% add_model(proportional_hazards(penalty = 0.001) %>% set_engine(
        "glmnet")) %>% add_formula(event_time ~ .) %>% fit(data = sim_dat) %>%
        augment(new_data = sim_dat)
    Condition
      Error in `h()`:
      ! error in evaluating the argument 'x' in selecting a method for function 'as.matrix': Cholmod error 'X and/or Y have wrong dimensions' at file ../MatrixOps/cholmod_sdmult.c, line 88

