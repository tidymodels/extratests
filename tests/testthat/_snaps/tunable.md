# bad model inputs

    Code
      (expect_error(tunable(bad_class)))
    Output
      <simpleError: The `parsnip` model database doesn't know about the arguments for model `potato`. Was it registered?>

# test tunable parameter values

    Code
      boost_tree(trees = tune(), min_n = tune(), sample_size = tune()) %>% set_engine(
        "C5.0") %>% print_parameters()
    Output
      $trees
      $trees$pkg
      [1] "dials"
      
      $trees$fun
      [1] "trees"
      
      $trees$range
      [1]   1 100
      
      
      $min_n
      $min_n$pkg
      [1] "dials"
      
      $min_n$fun
      [1] "min_n"
      
      
      $sample_size
      $sample_size$pkg
      [1] "dials"
      
      $sample_size$fun
      [1] "sample_prop"
      
      

---

    Code
      C5_rules(trees = tune(), min_n = tune()) %>% set_engine("C5.0") %>%
        print_parameters()
    Output
      $trees
      $trees$pkg
      [1] "dials"
      
      $trees$fun
      [1] "trees"
      
      $trees$range
      [1]   1 100
      
      
      $min_n
      $min_n$pkg
      [1] "dials"
      
      $min_n$fun
      [1] "min_n"
      
      $min_n$range
      [1]  2 40
      
      

---

    Code
      decision_tree(min_n = tune()) %>% set_engine("C5.0") %>% print_parameters()
    Output
      $min_n
      $min_n$pkg
      [1] "dials"
      
      $min_n$fun
      [1] "min_n"
      
      

---

    Code
      logistic_reg(penalty = tune()) %>% set_engine("brulee") %>% print_parameters()
    Output
      $penalty
      $penalty$pkg
      [1] "dials"
      
      $penalty$fun
      [1] "penalty"
      
      
      $mixture
      $mixture$pkg
      [1] "dials"
      
      $mixture$fun
      [1] "mixture"
      
      

---

    Code
      mars(prod_degree = tune()) %>% set_engine("earth") %>% set_mode(
        "classification") %>% print_parameters()
    Output
      $num_terms
      $num_terms$pkg
      [1] "dials"
      
      $num_terms$fun
      [1] "num_terms"
      
      $num_terms$range
      [1] 2 5
      
      
      $prod_degree
      $prod_degree$pkg
      [1] "dials"
      
      $prod_degree$fun
      [1] "prod_degree"
      
      
      $prune_method
      $prune_method$pkg
      [1] "dials"
      
      $prune_method$fun
      [1] "prune_method"
      
      

---

    Code
      multinom_reg(penalty = tune()) %>% set_engine("brulee") %>% print_parameters()
    Output
      $penalty
      $penalty$pkg
      [1] "dials"
      
      $penalty$fun
      [1] "penalty"
      
      
      $mixture
      $mixture$pkg
      [1] "dials"
      
      $mixture$fun
      [1] "mixture"
      
      

---

    Code
      rand_forest(mtry = tune(), min_n = tune()) %>% set_engine("randomForest") %>%
        set_mode("classification") %>% print_parameters()
    Output
      $mtry
      $mtry$pkg
      [1] "dials"
      
      $mtry$fun
      [1] "mtry"
      
      
      $trees
      $trees$pkg
      [1] "dials"
      
      $trees$fun
      [1] "trees"
      
      
      $min_n
      $min_n$pkg
      [1] "dials"
      
      $min_n$fun
      [1] "min_n"
      
      

---

    Code
      rand_forest(mtry = tune(), min_n = tune()) %>% set_engine("ranger") %>%
        set_mode("classification") %>% print_parameters()
    Output
      $mtry
      $mtry$pkg
      [1] "dials"
      
      $mtry$fun
      [1] "mtry"
      
      
      $trees
      $trees$pkg
      [1] "dials"
      
      $trees$fun
      [1] "trees"
      
      
      $min_n
      $min_n$pkg
      [1] "dials"
      
      $min_n$fun
      [1] "min_n"
      
      

---

    Code
      linear_reg(penalty = tune()) %>% set_engine("brulee") %>% print_parameters()
    Output
      $penalty
      $penalty$pkg
      [1] "dials"
      
      $penalty$fun
      [1] "penalty"
      
      
      $mixture
      $mixture$pkg
      [1] "dials"
      
      $mixture$fun
      [1] "mixture"
      
      

---

    Code
      boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(),
      loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
        set_engine("xgboost") %>% set_mode("classification") %>% print_parameters()
    Output
      $tree_depth
      $tree_depth$pkg
      [1] "dials"
      
      $tree_depth$fun
      [1] "tree_depth"
      
      
      $trees
      $trees$pkg
      [1] "dials"
      
      $trees$fun
      [1] "trees"
      
      
      $learn_rate
      $learn_rate$pkg
      [1] "dials"
      
      $learn_rate$fun
      [1] "learn_rate"
      
      $learn_rate$range
      [1] -3.0 -0.5
      
      
      $mtry
      $mtry$pkg
      [1] "dials"
      
      $mtry$fun
      [1] "mtry"
      
      
      $min_n
      $min_n$pkg
      [1] "dials"
      
      $min_n$fun
      [1] "min_n"
      
      
      $loss_reduction
      $loss_reduction$pkg
      [1] "dials"
      
      $loss_reduction$fun
      [1] "loss_reduction"
      
      
      $sample_size
      $sample_size$pkg
      [1] "dials"
      
      $sample_size$fun
      [1] "sample_prop"
      
      
      $stop_iter
      $stop_iter$pkg
      [1] "dials"
      
      $stop_iter$fun
      [1] "stop_iter"
      
      

---

    Code
      mlp(hidden_units = tune(), penalty = tune(), dropout = tune(), epochs = tune(),
      activation = tune()) %>% set_engine("brulee") %>% set_mode("classification") %>%
        print_parameters()
    Output
      $hidden_units
      $hidden_units$pkg
      [1] "dials"
      
      $hidden_units$fun
      [1] "hidden_units"
      
      
      $penalty
      $penalty$pkg
      [1] "dials"
      
      $penalty$fun
      [1] "penalty"
      
      
      $epochs
      $epochs$pkg
      [1] "dials"
      
      $epochs$fun
      [1] "epochs"
      
      $epochs$range
      [1]   5 500
      
      
      $dropout
      $dropout$pkg
      [1] "dials"
      
      $dropout$fun
      [1] "dropout"
      
      
      $learn_rate
      $learn_rate$pkg
      [1] "dials"
      
      $learn_rate$fun
      [1] "learn_rate"
      
      $learn_rate$range
      [1] -3.0 -0.5
      
      
      $activation
      $activation$pkg
      [1] "dials"
      
      $activation$fun
      [1] "activation"
      
      $activation$values
      [1] "relu" "elu"  "tanh"
      
      

