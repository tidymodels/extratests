# mlp execution, quantile regression

    Code
      spec_1
    Output
      Single Layer Neural Network Model Specification (quantile regression)
      
      Computational engine: qrnn 
      
    Message
      Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.

---

    Code
      print(body(qnt_fit_1$fit$Th))
    Output
      {
          tanh(0.5 * x)
      }

---

    Code
      spec_2
    Output
      Single Layer Neural Network Model Specification (quantile regression)
      
      Main Arguments:
        activation = relu
      
      Computational engine: qrnn 
      
    Message
      Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.

---

    Code
      print(body(qnt_fit_2$fit$Th))
    Output
      {
          ifelse(x >= 0, x, 0)
      }

---

    Code
      spec_3
    Output
      Single Layer Neural Network Model Specification (quantile regression)
      
      Main Arguments:
        hidden_units = 4
        penalty = 0.1
        epochs = 20
        activation = relu
      
      Engine-Specific Arguments:
        method = adam
      
      Computational engine: qrnn 
      
    Message
      Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.

---

    Code
      fit(set_mode(set_engine(mlp(activation = "POTAAATO"), "qrnn"),
      "quantile regression", quantile_levels = (1:9) / 10), outcome ~ ., data = dat)
    Condition
      Error in `parsnip::mcqrnn_wrap()`:
      ! Could not find an activation function called `POTAAATO()` in the qrnn package.

