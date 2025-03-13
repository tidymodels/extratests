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

# id: 7, recipe sparsity: yes, sparsity: low, model support: yes, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! correct: x is dense matrix

# id: 8, recipe sparsity: yes, sparsity: low, model support: yes, arg: no

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! correct: x is dense matrix

# id: 9, recipe sparsity: yes, sparsity: low, model support: yes, arg: yes

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! wrong: x is sparse matrix

# id: 10, recipe sparsity: yes, sparsity: low, model support: no, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `stats::lm()`:
      ! correct: x is dense matrix

# id: 11, recipe sparsity: yes, sparsity: low, model support: no, arg: no

    Code
      fit(wf_spec, ames)
    Condition
      Error in `stats::lm()`:
      ! correct: x is dense matrix

# id: 12, recipe sparsity: yes, sparsity: low, model support: no, arg: yes

    Code
      fit(wf_spec, ames)
    Condition
      Warning:
      `x` is a sparse tibble, but `linear_reg()` with engine "lm" doesn't accept that. Converting to non-sparse.
      Error in `stats::lm()`:
      ! correct: x is dense matrix

# id: 13, recipe sparsity: no, sparsity: high, model support: yes, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! wrong: x is dense matrix

# id: 16, recipe sparsity: no, sparsity: high, model support: no, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `stats::lm()`:
      ! correct: x is dense matrix

# id: 19, recipe sparsity: no, sparsity: low, model support: yes, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `parsnip::xgb_train()`:
      ! correct: x is dense matrix

# id: 22, recipe sparsity: no, sparsity: low, model support: no, arg: auto

    Code
      fit(wf_spec, ames)
    Condition
      Error in `stats::lm()`:
      ! correct: x is dense matrix

