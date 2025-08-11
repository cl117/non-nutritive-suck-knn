# non-nutritive suck knn model
## Use the model:
Given a new data frame test with the same 11 features:

R
```
model <- readRDS("knn_final_k29.rds")
test_pred <- predict(knnFit, test[,-c(11,12)], type = "prob")[,2]
tau <- 0.5 # or any other values greater than 0.5
pred  <- ifelse(test_pred >= tau, 1, 0)

out <- data.frame(readiness_score = test_pred, prediction = pred, label = test$category2)

out$prediction <- factor(as.character(out$prediction))
levels(out$prediction) <- make.names(levels(out$prediction))
cm <- confusionMatrix(out$prediction, out$label, positive="X1")
cm
```
Interpretation

readiness_score ∈ [0,1] = calibrated probability of Ready (X1)

prediction: "X1" = Ready, "X0" = Not Ready (at threshold τ)

Evaluate (confusion matrix)
If your out data frame has prediction and true label:

R
```
library(caret)

out$prediction <- factor(out$prediction, levels = c("X0","X1"))
out$label      <- factor(out$label,      levels = c("X0","X1"))

cm <- confusionMatrix(out$prediction, out$label, positive = "X1")
cm
```

Console output:
```
> cm
Confusion Matrix and Statistics

          Reference
Prediction X0 X1
        X0 55 14
        X1 22 42
                                          
               Accuracy : 0.7293          
                 95% CI : (0.6455, 0.8027)
    No Information Rate : 0.5789          
    P-Value [Acc > NIR] : 0.00023         
                                          
                  Kappa : 0.4554          
                                          
 Mcnemar's Test P-Value : 0.24335         
                                          
            Sensitivity : 0.7500          
            Specificity : 0.7143          
         Pos Pred Value : 0.6562          
         Neg Pred Value : 0.7971          
             Prevalence : 0.4211          
         Detection Rate : 0.3158          
   Detection Prevalence : 0.4812          
      Balanced Accuracy : 0.7321          
                                          
       'Positive' Class : X1
```

## How to cite
NNS Feeding Readiness via KNN (k=29), version 1.0. Model and code available in this repository. Please cite the accompanying manuscript when available.