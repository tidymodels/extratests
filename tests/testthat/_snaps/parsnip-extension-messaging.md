# messaging with unknown implementation (bag tree, tidymodels/parsnip#793)

    Code
      bag_tree() %>% set_engine("rpart") %>% set_mode("regression")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications using the `rpart` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_mode("censored regression")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `bag_tree` censored regression model specifications.
      i The parsnip extension package censored implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (censored regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree()
    Message <rlang_message>
      ! parsnip could not locate an implementation for `bag_tree` model specifications.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_engine("rpart")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `bag_tree` model specifications using the `rpart` engine.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_mode("censored regression") %>% set_engine("rpart")
    Output
      Bagged Decision Tree Model Specification (censored regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_engine("rpart")
    Output
      Bagged Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_mode("regression") %>% set_engine("rpart")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications using the `rpart` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_mode("classification") %>% set_engine("C5.0")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `bag_tree` classification model specifications using the `C5.0` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (classification)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: C5.0 
      

---

    Code
      bag_tree() %>% set_engine("C5.0")
    Output
      Bagged Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: C5.0 
      

# messaging with unknown implementation (decision tree, tidymodels/parsnip#793)

    Code
      decision_tree()
    Output
      Decision Tree Model Specification (unknown)
      
      Computational engine: rpart 
      

---

    Code
      decision_tree() %>% set_mode("censored regression")
    Output
      Decision Tree Model Specification (censored regression)
      
      Computational engine: rpart 
      

---

    Code
      decision_tree() %>% set_engine("partykit")
    Output
      Decision Tree Model Specification (unknown)
      
      Computational engine: partykit 
      

---

    Code
      decision_tree() %>% set_engine("partykit") %>% set_mode("regression")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `decision_tree` regression model specifications using the `partykit` engine.
      i The parsnip extension package bonsai implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Decision Tree Model Specification (regression)
      
      Computational engine: partykit 
      

---

    Code
      decision_tree() %>% set_mode("censored regression") %>% set_engine("rpart")
    Output
      Decision Tree Model Specification (censored regression)
      
      Computational engine: rpart 
      

---

    Code
      decision_tree() %>% set_engine("partykit")
    Output
      Decision Tree Model Specification (unknown)
      
      Computational engine: partykit 
      

---

    Code
      decision_tree() %>% set_mode("regression") %>% set_engine("partykit")
    Message <rlang_message>
      ! parsnip could not locate an implementation for `decision_tree` regression model specifications using the `partykit` engine.
      i The parsnip extension package bonsai implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Decision Tree Model Specification (regression)
      
      Computational engine: partykit 
      

---

    Code
      decision_tree() %>% set_mode("regression") %>% set_engine("partykit")
    Output
      Decision Tree Model Specification (regression)
      
      Computational engine: partykit 
      

