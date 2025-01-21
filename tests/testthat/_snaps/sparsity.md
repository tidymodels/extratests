# id: 1, recipe sparsity: yes, sparsity: high, model support: yes, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! correct: x is sparse matrix

# id: 2, recipe sparsity: yes, sparsity: high, model support: yes, arg: no

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! correct: x is dense matrix

# id: 3, recipe sparsity: yes, sparsity: high, model support: yes, arg: yes

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! correct: x is sparse matrix

# id: 4, recipe sparsity: yes, sparsity: high, model support: no, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `stats::lm()`:
      ! correct: x is dense matrix

# id: 5, recipe sparsity: yes, sparsity: high, model support: no, arg: no

    Code
      fit(wf_spec, ames)
    Condition
      Error in `stats::lm()`:
      ! correct: x is dense matrix

# id: 6, recipe sparsity: yes, sparsity: high, model support: no, arg: yes

    Code
      fit(wf_spec, ames)
    Condition
      Warning:
      `x` is a sparse tibble, but `linear_reg()` with engine "lm" doesn't accept that. Converting to non-sparse.
      Error in `stats::lm()`:
      ! correct: x is dense matrix

