# Correct values

    Code
      rec <- prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step nnmf sparse [training] 
      The retained training set is ~ 0 Mb  in memory.
      

# No NNF

    Code
      print(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * No non-negative matrix factorization was extracted from: Sepal.Length,
        Sepal.Width, Petal.Length, Petal.Width | Trained

