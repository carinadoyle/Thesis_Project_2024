library(rpart)
library(rpart.plot)

set.seed(0)

# creating a decision tree for 18

dt_model_18 <- rpart(currdepress~., method="class", data = train_18)
rpart.plot(dt_model_18)

pred_exp_train <- predict(dt_model_18, train_18, type = "class")
mean(pred_exp_train != train_18$currdepress)

pred_exp_test <- predict(dt_model_18, test_18, type = "class")
pred_test_18 <- list(pred_exp_test)
mean(pred_exp_test != test_18$currdepress)

accuracy_18 <- append(accuracy_18, sum(pred_exp_test == test_18$currdepress) / length(pred_exp_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_exp_test), test_18$currdepress)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_exp_test_prob_18 <- predict(dt_model_18, test_18, type = "prob")
dt_ROCurve_18 <- roc(test_18$currdepress, as.numeric(pred_exp_test_prob_18[,1]))
auc_18 <- append(auc_18, auc(dt_ROCurve_18))

# creating a decision tree for 19

dt_model_19 <- rpart(nspd~., method="class", data = train_19)
rpart.plot(dt_model_19)

pred_exp_train <- predict(dt_model_19, train_19, type = "class")
mean(pred_exp_train != train_19$nspd)

pred_exp_test <- predict(dt_model_19, test_19, type = "class")
mean(pred_exp_test != test_19$nspd)

accuracy_19 <- append(accuracy_19, sum(pred_exp_test == test_19$nspd) / length(pred_exp_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_exp_test), test_19$nspd)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_exp_test_prob_19 <- predict(dt_model_19, test_19, type = "prob")
dt_ROCurve_19 <- roc(test_19$nspd, as.numeric(pred_exp_test_prob_19[,1]))
auc_19 <- append(auc_19, auc(dt_ROCurve_19))

# creating a decision tree for 20

dt_model_20 <- rpart(nspd~., method="class", data = train_20)
rpart.plot(dt_model_20)

pred_exp_train <- predict(dt_model_20, train_20, type = "class")
mean(pred_exp_train != train_20$nspd)

pred_exp_test <- predict(dt_model_20, test_20, type = "class")
mean(pred_exp_test != test_20$nspd)

accuracy_20 <- append(accuracy_20, sum(pred_exp_test == test_20$nspd) / length(pred_exp_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_exp_test), test_20$nspd)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_exp_test_prob_20 <- predict(dt_model_20, test_20, type = "prob")
dt_ROCurve_20 <- roc(test_20$nspd, as.numeric(pred_exp_test_prob_20[,2]))
auc_20 <- append(auc_20, auc(dt_ROCurve_20))
