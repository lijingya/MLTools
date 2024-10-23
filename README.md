# Examples
```
out <- train(super_label ~ ., 
            data = train.df, 
            method = glm_h2o, 
            preProc = c("center", "scale"), 
            # trControl = trainControl(method = "none",classProbs = TRUE, savePredictions = "final"),
            trControl = trainControl(method = "cv", number = 4, classProbs = TRUE, summaryFunction = twoClassSummary, seeds = NA, savePredictions ="final"),
            metric = 'ROC',
            tuneGrid = data.frame(
              solver = "COORDINATE_DESCENT", 
              max_iterations = 19,
              objective_epsilon = 1e-04,
              gradient_epsilon = 1e-06,
              link = "logit",
              lambda_min_ratio = 1e-04,
              max_active_predictors = 5000,
              obj_reg = 0.025, 
              alpha = 0, lambda = 2.1098, lambda_search = FALSE
            )
     )
```
