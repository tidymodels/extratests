# autoplot-ting survival models with static metric

    Code
      ggplot2::get_labs(stc_marginal)
    Output
      $x.sec
      NULL
      
      $x
          trees 
      "# Trees" 
      
      $y
      [1] "concordance_survival"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(stc_perf)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "concordance_survival"
      
      $y.sec
      NULL
      
      $ymin
      [1] "mean - const * std_err"
      
      $ymax
      [1] "mean + const * std_err"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(stc_param)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "# Trees"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

# autoplot-ting survival models with integrated metric

    Code
      ggplot2::get_labs(int_grid)
    Output
      $colour
      `Proportion of Lasso Penalty`
      
      $x.sec
      NULL
      
      $x
      [1] "Amount of Regularization"
      
      $y
      [1] "brier_survival_integrated"
      
      $y.sec
      NULL
      
      $group
      [1] "Proportion of Lasso Penalty"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(int_marginal)
    Output
      $x.sec
      NULL
      
      $x
      [1] ""
      
      $y
      [1] "brier_survival_integrated"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(int_perf)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "brier_survival_integrated"
      
      $y.sec
      NULL
      
      $ymin
      [1] "mean - const * std_err"
      
      $ymax
      [1] "mean + const * std_err"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(int_param)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

# autoplot-ting survival models with dynamic metric

    Code
      set.seed(2193)
      bayes_dynamic_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, metrics = dyn_mtrc, eval_time = time_points, control = bctrl,
      initial = grid_dynamic_res)
    Condition
      Warning in `tune_bayes()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      ggplot2::get_labs(dyn_grid)
    Output
      $x.sec
      NULL
      
      $x
      [1] ""
      
      $y
      [1] "brier_survival @10"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_mult_grid)
    Output
      $x.sec
      NULL
      
      $x
      [1] ""
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_marginal)
    Output
      $x.sec
      NULL
      
      $x
      [1] ""
      
      $y
      [1] "brier_survival @10"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_mult_marginal)
    Output
      $x.sec
      NULL
      
      $x
      [1] ""
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_perf)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "brier_survival @10"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_marginal)
    Output
      $x.sec
      NULL
      
      $x
      [1] ""
      
      $y
      [1] "brier_survival @10"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(dyn_param)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      dyn_mult_param <- autoplot(bayes_dynamic_res, type = "parameters", eval_time = c(
        10, 15))
    Condition
      Warning:
      `eval_time` is not used with `autoplot(..., type = 'parameters')`.

---

    Code
      ggplot2::get_labs(dyn_mult_param)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

# autoplot-ting survival models with different metric types

    Code
      set.seed(2193)
      bayes_mixed_res <- mod_spec %>% tune_bayes(event_time ~ X1 + X2, resamples = sim_rs,
      iter = 2, metrics = mix_mtrc, eval_time = time_points, initial = grid_mixed_res,
      control = bctrl)
    Condition
      Warning in `tune_bayes()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).

---

    Code
      ggplot2::get_labs(mix_grid)
    Output
      $x.sec
      NULL
      
      $x
        tree_depth 
      "Tree Depth" 
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_mult_grid)
    Output
      $x.sec
      NULL
      
      $x
        tree_depth 
      "Tree Depth" 
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_marginal)
    Output
      $x.sec
      NULL
      
      $x
        tree_depth 
      "Tree Depth" 
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_mult_marginal)
    Output
      $x.sec
      NULL
      
      $x
        tree_depth 
      "Tree Depth" 
      
      $y
      [1] ""
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_perf)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "mean"
      
      $y.sec
      NULL
      
      $ymin
      [1] "mean - const * std_err"
      
      $ymax
      [1] "mean + const * std_err"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_mult_perf)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "mean"
      
      $y.sec
      NULL
      
      $ymin
      [1] "mean - const * std_err"
      
      $ymax
      [1] "mean + const * std_err"
      
      $alt
      [1] ""
      

---

    Code
      ggplot2::get_labs(mix_param)
    Output
      $x.sec
      NULL
      
      $x
      [1] "Iteration"
      
      $y
      [1] "Tree Depth"
      
      $y.sec
      NULL
      
      $alt
      [1] ""
      

---

    Code
      mix_mult_param <- autoplot(bayes_mixed_res, type = "parameters", eval_time = c(
        10, 15))
    Condition
      Warning:
      `eval_time` is not used with `autoplot(..., type = 'parameters')`.

# autoplot() warning for unneeded evaluation times

    Code
      p1 <- autoplot(tune_res, eval_time = 10, metric = "concordance_survival")
    Condition
      Warning in `autoplot()`:
      `eval_time` is only used for dynamic survival metrics.

---

    Code
      p2 <- autoplot(tune_res, eval_time = 10, metric = "brier_survival_integrated")
    Condition
      Warning in `autoplot()`:
      `eval_time` is only used for dynamic survival metrics.

