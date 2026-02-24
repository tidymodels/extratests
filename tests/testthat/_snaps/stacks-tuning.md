# stacking with Bayesian tuning works

    Code
      data_st_bayes <- stacks() %>% add_candidates(wf_set_bayes) %>% suppressMessages()
    Condition
      Warning:
      ! Some elements of the supplied workflow set failed to evaluate with resamples.
      i The workflow with ID `rec_bt` will be excluded from the data stack.

