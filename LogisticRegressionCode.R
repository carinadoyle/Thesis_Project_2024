
set.seed(0)

# modeling logistic regression 18

log_model_18 <- glm(currdepress~., data=train_18, family="binomial")
summary(log_model_18)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model_18 <- train(currdepress ~ .,data = train_18,trControl = train_control,
               method = "glm",family=binomial())
model_18

pred_train_18 <- predict(log_model_18, newdata=train_18, type="response")
train_label_18 <- ifelse(pred_train_18 > 0.5, 1, 0)
tr_error_18 <- mean(train_label_18!=train_18$currdepress)

pred_test_18 <- predict(log_model_18, newdata=test_18, type="response")
test_label_18 <- ifelse(pred_test_18 > 0.5, 1, 0)
te_error_18 <- mean(test_label_18!=test_18$currdepress)

tr_error_18
te_error_18

accuracy_18 <- c()
accuracy_18 <- append(accuracy_18, sum(test_label_18 == test_18$currdepress) / length(test_label_18))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(test_label_18), factor(test_18$currdepress))$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_test_18 <- predict(log_model_18, newdata=test_18, type="response")
log_roc_18 <- roc(as.numeric(test_18$currdepress),as.numeric(pred_test_18))
auc_18 <- c()
auc_18 <- append(auc_18, auc(log_roc_18))

imp_df <- data.frame(importance(model_18)$data)
sliced <- imp_df[1:10,]
ggplot(data=sliced) + geom_col(mapping=aes(x=Overall, y=names), fill="red") + 
  xlab("Importance Score") + ylab("Variable") + ggtitle("Important Variables For Logisitic Regression in 2018")


# modeling logistic regression 19

log_model_19 <- glm(nspd~., data=train_19, family="binomial")
summary(log_model_19)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model_19 <- train(nspd ~ .,data = train_19,trControl = train_control,
               method = "glm",family=binomial())
model_19

pred_train_19 <- predict(log_model_19, newdata=train_19, type="response")
train_label_19 <- ifelse(pred_train_19 > 0.5, 1, 0)
tr_error_19 <- mean(train_label_19!=train_19$nspd)

pred_test_19 <- predict(log_model_19, newdata=test_19, type="response")
test_label_19 <- ifelse(pred_test_19 > 0.5, 1, 0)
te_error_19 <- mean(test_label_19!=test_19$nspd)

tr_error_19
te_error_19

accuracy_19 <- c()
accuracy_19 <- append(accuracy_19, sum(test_label_19 == test_19$nspd) / length(test_label_19))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(test_label_19), factor(test_19$nspd))$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_test_19 <- predict(log_model_19, newdata=test_19, type="response")
log_roc_19 <- roc(as.numeric(test_19$nspd), as.numeric(pred_test_19))
auc_19 <- c()
auc_19 <- append(auc_19, auc(log_roc_19))

imp_df <- data.frame(importance(model_19)$data)
sliced <- imp_df[1:10,]
ggplot(data=sliced) + geom_col(mapping=aes(x=Overall, y=names), fill="purple") + 
  xlab("Importance Score") + ylab("Variable") + ggtitle("Important Variables For Logisitic Regression in 2019")

# modeling logistic regression 20

log_model_20 <- glm(nspd~., data=train_20, family="binomial")
summary(log_model_20)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model_20 <- train(nspd ~ .,data = train_20,trControl = train_control,
               method = "glm",family=binomial())
model_20

pred_train_20 <- predict(log_model_20, newdata=train_20, type="response")
train_label_20 <- ifelse(pred_train_20 > 0.5, 1, 0)
tr_error_20 <- mean(train_label_20!=train_20$nspd)

pred_test_20 <- predict(log_model_20, newdata=test_20, type="response")
test_label_20 <- ifelse(pred_test_20 > 0.5, 1, 0)
te_error_20 <- mean(test_label_20!=test_20$nspd)

tr_error_20
te_error_20

accuracy_20 <- c()
accuracy_20 <- append(accuracy_20, sum(test_label_20 == test_20$nspd) / length(test_label_20))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(test_label_20), factor(test_20$nspd))$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_test_20 <- predict(log_model_20, newdata=test_20, type="response")
log_roc_20 <- roc(as.numeric(test_20$nspd), as.numeric(pred_test_20))
auc_20 <- c()
auc_20 <- append(auc_20, auc(log_roc_20))

imp_df <- data.frame(importance(model_20)$data)
sliced <- imp_df[1:10,]
ggplot(data=sliced) + geom_col(mapping=aes(x=Overall, y=names), fill="blue") + 
  xlab("Importance Score") + ylab("Variable") + ggtitle("Important Variables For Logisitic Regression in 2020")

