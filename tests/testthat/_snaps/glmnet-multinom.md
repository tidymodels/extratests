# multi_predict() with default or single penalty value

    Code
      multi_predict(xy_fit, newdata = hpc[rows, 1:4], penalty = c(0.1, 0.5))
    Error <rlang_error>
      Please use `new_data` instead of `newdata`.

# error traps

    Code
      multinom_reg(penalty = 0.01) %>% set_engine("glmnet") %>% fit(class ~ ., data = hpc_data) %>%
        predict(hpc_data, penalty = 0:1)
    Error <rlang_error>
      `penalty` should be a single numeric value. `multi_predict()` can be used to get multiple predictions per row of data.

---

    Code
      multinom_reg() %>% set_engine("glmnet") %>% fit(class ~ ., data = hpc_data)
    Error <rlang_error>
      For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

