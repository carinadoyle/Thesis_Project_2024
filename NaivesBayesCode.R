library(naivebayes)

set.seed(0)

# naive bayes classifier for 18

nb_model_18 <- naive_bayes(currdepress ~ ., train_18, usekernel = T, laplace=1) 
summary(nb_model_18)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model <- train(currdepress ~ .,data = train_18,trControl = train_control,
               method = "naive_bayes",family=binomial())
model

pred_train_18 <- predict(nb_model_18, newdata=train_18, type="class")
tr_error_18 <- mean(pred_train_18!=train_18$currdepress)

pred_test_18 <- predict(nb_model_18, newdata=test_18, type="class")
te_error_18 <- mean(pred_test_18!=test_18$currdepress)

tr_error_18
te_error_18

accuracy_18 <- append(accuracy_18, sum(pred_test_18 == test_18$currdepress) / length(pred_test_18))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_test_18), test_18$currdepress)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test_18 <- predict(nb_model_18, newdata=test_18, type="prob")
nb_ROCurve_18<-roc(test_18$currdepress,as.numeric(pred_prob_test_18[,1]))
auc_18 <- append(auc_18, auc(nb_ROCurve_18))

# naive bayes classifier for 19

nb_model_19 <- naive_bayes(nspd ~ ., train_19, usekernel = T, laplace=1) 
summary(nb_model_19)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model <- train(nspd ~ .,data = train_19,trControl = train_control,
               method = "naive_bayes",family=binomial())
model

pred_train_19 <- predict(nb_model_19, newdata=train_19, type="class")
tr_error_19 <- mean(pred_train_19!=train_19$nspd)

pred_test_19 <- predict(nb_model_19, newdata=test_19, type="class")
te_error_19 <- mean(pred_test_19!=test_19$nspd)

tr_error_19
te_error_19

accuracy_19 <- append(accuracy_19, sum(pred_test_19 == test_19$nspd) / length(pred_test_19))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_test_19), test_19$nspd)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test_19 <- predict(nb_model_19, newdata=test_19, type="prob")
nb_ROCurve_19 <- roc(test_19$nspd, as.numeric(pred_prob_test_19[,1]))
auc_19 <- append(auc_19, auc(nb_ROCurve_19))

# naive bayes classifier for 20

nb_model_20 <- naive_bayes(nspd ~ ., train_20, usekernel = T, laplace=1) 
summary(nb_model_20)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model <- train(nspd ~ .,data = train_20,trControl = train_control,
               method = "naive_bayes",family=binomial())
model

pred_train_20 <- predict(nb_model_20, newdata=train_20, type="class")
tr_error_20 <- mean(pred_train_20!=train_20$nspd)

pred_test_20 <- predict(nb_model_20, newdata=test_20, type="class")
te_error_20 <- mean(pred_test_20!=test_20$nspd)

tr_error_20
te_error_20

accuracy_20 <- append(accuracy_20, sum(pred_test_20 == test_20$nspd) / length(pred_test_20))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_test_20), test_20$nspd)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test_20 <- predict(nb_model_20, newdata=test_20, type="prob")
nb_ROCurve_20 <- roc(test_20$nspd, as.numeric(pred_prob_test_20[,1]))
auc_20 <- append(auc_20, auc(nb_ROCurve_20))
