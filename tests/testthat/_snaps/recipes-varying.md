# recipe steps with non-varying args error if specified as varying()

    Code
      varying_args(rec_bad_varying)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `map2()`:
      i In index: 5.
      i With name: skip.
      Caused by error in `.f()`:
      ! The argument skip for a recipe step of type "step_type" is not allowed to vary.

