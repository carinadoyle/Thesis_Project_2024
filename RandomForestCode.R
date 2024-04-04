library(randomForest)

set.seed(0)

# random forest model for 18

rf_model_18 <- randomForest(currdepress~., train_18, importance=TRUE, 
                            ntree=1000, proximity=TRUE, nodesize=10, mtry=10)
rf_model_18

rf_pred_18 <- predict(rf_model_18, test_18, type="class")
confusionMatrix(rf_pred_18, test_18$currdepress)
te_error_18 <- mean(rf_pred_18!=test_18$currdepress)

accuracy_18 <- append(accuracy_18, sum(rf_pred_18 == test_18$currdepress) / length(rf_pred_18))

rf_pred_prob_18 <- predict(rf_model_18, test_18, type="prob")
rf_ROCurve_18 <- roc(test_18$currdepress, as.numeric(rf_pred_prob_18[,1]))
auc_18 <- append(auc_18, auc(rf_ROCurve_18))

# random forest model for 19

rf_model_19 <- randomForest(nspd~., train_19, proximity=TRUE, importance=TRUE, 
                            ntree=1000, nodesize=10, mtry=10)
rf_model_19

rf_pred_19 <- predict(rf_model_19, test_19, type="class")
confusionMatrix(rf_pred_19, test_19$nspd)
te_error_19 <- mean(rf_pred_19!=test_19$nspd)

accuracy_19 <- append(accuracy_19, sum(rf_pred_19 == test_19$nspd) / length(rf_pred_19))

rf_pred_prob_19 <- predict(rf_model_19, test_19, type="prob")
rf_ROCurve_19 <- roc(test_19$nspd, as.numeric(rf_pred_prob_19[,1]))
auc_19 <- append(auc_19, auc(rf_ROCurve_19))

# random forest model for 20

rf_model_20 <- randomForest(nspd~., train_20, proximity=TRUE, importance=TRUE, 
                            ntree=1000, nodesize=10, mtry=10)
rf_model_20

rf_pred_20 <- predict(rf_model_20, test_20, type="class")
confusionMatrix(rf_pred_20, test_20$nspd)
te_error_20 <- mean(rf_pred_20!=test_20$nspd)

accuracy_20 <- append(accuracy_20, sum(rf_pred_20 == test_20$nspd) / length(rf_pred_20))

rf_pred_prob_20 <- predict(rf_model_20, test_20, type="prob")
rf_ROCurve_20 <- roc(test_20$nspd, as.numeric(rf_pred_prob_20[,1]))
auc_20 <- append(auc_20, auc(rf_ROCurve_20))

