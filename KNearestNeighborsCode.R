
# creating K-Nearest Neighbors model for 18

set.seed(0)

k_seq <- c(5, seq(1,40, by=1))

te_error<- c()

for(i in seq_along(k_seq)){
  knn_fit <-caret::knn3(currdepress~., train_18, k=k_seq[i])
  knn_pred <- predict(knn_fit, test_18, type="class")
  te_error[i] <- mean(knn_pred != test_18$currdepress)
}

df <- data.frame(k_seq, te_error)

ggplot(data = df, mapping = aes(x=k_seq, y=te_error))+geom_line(col="red") +
  geom_line()+xlab("k value")+ylab("Testing error")

trControl <- trainControl(method  = "cv", number  = 10)
fit <- train(currdepress ~ ., method = "knn", tuneGrid= expand.grid(k = 1:20),
             trControl= trControl, metric = "Accuracy", data=train_18)
fit

# best K - 15 **** whats going on here???

knn_best_18 <- knn3(currdepress~.,train_18, k=15)
best_pred_18 <- predict(knn_best_18, test_18, type="class")
best_te_error_18 <- mean(best_pred_18 != test_18$currdepress)

accuracy_18 <- append(accuracy_18, sum(best_pred_18 == test_18$currdepress) / length(best_pred_18))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(best_pred_18), test_18$currdepress)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test_18 <- predict(knn_best_18, test_18, type="prob")
knn_ROCurve_18 <- roc(test_18$currdepress, as.numeric(pred_prob_test_18[,1]))
auc_18 <- append(auc_18, auc(knn_ROCurve_18))

# creating K-Nearest Neighbors model for 19

k_seq <- c(5, seq(1,40, by=1))

te_error<- c()

for(i in seq_along(k_seq)){
  knn_fit <-caret::knn3(nspd~., train_19, k=k_seq[i])
  knn_pred <- predict(knn_fit, test_19, type="class")
  te_error[i] <- mean(knn_pred != test_19$nspd)
}

df <- data.frame(k_seq, te_error)

ggplot(data = df, mapping = aes(x=k_seq, y=te_error))+geom_line(col="red") +
  geom_line()+xlab("k value")+ylab("Testing error")

trControl <- trainControl(method  = "cv", number  = 10)
fit <- train(nspd ~ ., method = "knn", tuneGrid= expand.grid(k = 1:20),
             trControl= trControl, metric = "Accuracy", data=train_19)
fit

# best K - 5

knn_best_19 <- knn3(nspd~.,train_19, k=10)
best_pred_19 <- predict(knn_best_19, test_19, type="class")
best_te_error_19 <- mean(best_pred_19 != test_19$nspd)

accuracy_19 <- append(accuracy_19, sum(best_pred_19 == test_19$nspd) / length(best_pred_19))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(best_pred_19), test_19$nspd)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test_19 <- predict(knn_best_19, test_19, type="prob")
knn_ROCurve_19 <- roc(test_19$nspd, as.numeric(pred_prob_test_19[,1]))
auc_19 <- append(auc_19, auc(knn_ROCurve_19))

# creating K-Nearest Neighbors model for 20

k_seq <- c(5, seq(1,20, by=1))

te_error<- c()

for(i in seq_along(k_seq)){
  knn_fit <-caret::knn3(nspd~., train_20, k=k_seq[i])
  knn_pred <- predict(knn_fit, test_20, type="class")
  te_error[i] <- mean(knn_pred != test_20$nspd)
}

df <- data.frame(k_seq, te_error)

ggplot(data = df, mapping = aes(x=k_seq, y=te_error))+geom_line(col="red") +
  geom_line()+xlab("k value")+ylab("Testing error") 

trControl <- trainControl(method  = "cv", number  = 10)
fit <- train(nspd ~ ., method = "knn", tuneGrid= expand.grid(k = 1:20),
             trControl= trControl, metric = "Accuracy", data=train_20)
fit

# best K - 10

knn_best_20 <- knn3(nspd~.,train_20, k=50)
best_pred_20 <- predict(knn_best_20, test_20, type="class")
best_te_error_20 <- mean(best_pred_20 != test_20$nspd)

accuracy_20 <- append(accuracy_20, sum(best_pred_20 == test_20$nspd) / length(best_pred_20))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(best_pred_20), test_20$nspd)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test_20 <- predict(knn_best_20, test_20, type="prob")
knn_ROCurve_20 <- roc(test_20$nspd, as.numeric(pred_prob_test_20[,1]))
auc_20 <- append(auc_20, auc(knn_ROCurve_20))

