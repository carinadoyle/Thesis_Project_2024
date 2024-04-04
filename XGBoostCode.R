library(xgboost)
library(varhandle)
library(cvms)

set.seed(0)

# XGBoost model for 18

X_train_18 <- unfactor(X_train_18)
X_test_18 <- unfactor(X_test_18)

xgb_train_18 <- xgb.DMatrix(data = as.matrix(X_train_18), label = y_train_18)
xgb_test_18 <- xgb.DMatrix(data = as.matrix(X_test_18), label = y_test_18)
xgb_params <- list(booster = "gbtree",eta = 0.1, max_depth = 4,gamma = 4,subsample=1,
  colsample_bytree = 0.7,objective = "binary:logistic", eval_metric = "error")

xgbcv <- xgb.cv(params = xgb_params, data = xgb_train_18, nrounds = 500, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train(params = xgb_params, data = xgb_train_18, nrounds = 310, watchlist = list(val=xgb_test_18,train=xgb_train_18), 
                   print_every_n = 10, early_stopping_rounds = 10, maximize = F)

xgbpred <- predict(xgb1,as.matrix(X_test_18))
xgbpred <- ifelse(xgbpred > 0.5,1,0)

confusionMatrix (factor(xgbpred), factor(test_18$currdepress))

testing_error <- mean(xgbpred != test_18$currdepress)
testing_error

accuracy_18 <- append(accuracy_18, sum(xgbpred == test_18$currdepress) / length(xgbpred))

mat <- xgb.importance(model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:10], col="red", main="Important Variables of XGBoost in 2018") 

xgbpred_prob <- predict(xgb1,as.matrix(X_test_18), type="response")
xgb_ROCurve_18 <- roc(test_18$currdepress, as.numeric(xgbpred_prob))
auc_18 <- append(auc_18, auc(xgb_ROCurve_18))

# XGBoost model for 19

X_train_19 <- unfactor(X_train_19)
X_test_19 <- unfactor(X_test_19)

xgb_train_19 <- xgb.DMatrix(data = as.matrix(X_train_19), label = y_train_19)
xgb_test_19 <- xgb.DMatrix(data = as.matrix(X_test_19), label = y_test_19)
xgb_params <- list(booster = "gbtree",eta = 0.1, max_depth = 4,gamma = 4,subsample=1,
                   colsample_bytree = 0.7,objective = "binary:logistic", eval_metric = "error")

xgbcv <- xgb.cv(params = xgb_params, data = xgb_train_19, nrounds = 500, nfold = 5, showsd = T, 
                stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train(params = xgb_params, data = xgb_train_19, nrounds = 310, 
                  watchlist = list(val=xgb_test_19,train=xgb_train_19), 
                  print_every_n = 10, early_stopping_rounds = 10, maximize = F)

xgbpred <- predict(xgb1,as.matrix(X_test_19))
xgbpred <- ifelse(xgbpred > 0.5,1,0)

confusionMatrix (factor(xgbpred), factor(test_19$nspd))

testing_error <- mean(xgbpred != test_19$nspd)
testing_error

accuracy_19 <- append(accuracy_19, sum(xgbpred == test_19$nspd) / length(xgbpred))

mat <- xgb.importance(model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:10], col="purple", main="Important Variables of XGBoost in 2019") 

xgbpred_prob <- predict(xgb1,as.matrix(X_test_19), type="response")
xgb_ROCurve_19 <- roc(test_19$nspd, as.numeric(xgbpred_prob))
auc_19 <- append(auc_19, auc(xgb_ROCurve_19))


# XGBoost model for 20

X_train_20 <- unfactor(X_train_20)
X_test_20 <- unfactor(X_test_20)

xgb_train_20 <- xgb.DMatrix(data = as.matrix(X_train_20), label = y_train_20)
xgb_test_20 <- xgb.DMatrix(data = as.matrix(X_test_20), label = y_test_20)
xgb_params <- list(booster = "gbtree",eta = 0.1, max_depth = 4,gamma = 4,subsample=1,
                   colsample_bytree = 0.7,objective = "binary:logistic", eval_metric = "error")

xgbcv <- xgb.cv(params = xgb_params, data = xgb_train_20, nrounds = 700, nfold = 5, showsd = T, 
                stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train(params = xgb_params, data = xgb_train_20, nrounds = 487, 
                  watchlist = list(val=xgb_test_20,train=xgb_train_20), 
                  print_every_n = 10, early_stopping_rounds = 10, maximize = F)

xgbpred <- predict(xgb1,as.matrix(X_test_20))
xgbpred <- ifelse(xgbpred > 0.5,1,0)

confusionMatrix (factor(xgbpred), factor(test_20$nspd))

testing_error <- mean(xgbpred != test_20$nspd)
testing_error

accuracy_20 <- append(accuracy_20, sum(xgbpred == test_20$nspd) / length(xgbpred))

mat <- xgb.importance(model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:10], col="blue", main="Important Variables of XGBoost in 2020") 

xgbpred_prob <- predict(xgb1,as.matrix(X_test_20), type="response")
xgb_ROCurve_20 <- roc(test_20$nspd, as.numeric(xgbpred_prob))
auc_20 <- append(auc_20, auc(xgb_ROCurve_20))


