# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 columns were requested but there were 6 predictors in the data.
      i 6 predictors will be used.
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 samples will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      ! 1000 columns were requested but there were 6 predictors in the data.
      i 6 predictors will be used.
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 samples will be used.

