
# listing algorithms used
models <- c("Logistic Regression", "Naive Bayes", "K-Nearest Neighbors", 
            "Decision Trees", "Random Forest", "XGBoost")

# table of accuracy values
cbind("Model"=models, "2018"=round(accuracy_18,3), "2019"=round(accuracy_19,3), "2020"=round(accuracy_20,3))

# table of AUC values
cbind("Model"=models, "2018"=round(auc_18,3), "2019"=round(auc_19,3), "2020"=round(auc_20,3))

# plotting auc curves for each model in 2018
ggroc(list("Logistic Regression"=log_roc_18,"Naive Bayes"=nb_ROCurve_18, 
           "K-Nearest Neighbors"=knn_ROCurve_18, "Random Forest"=rf_ROCurve_18,
           "Decision Tree"=dt_ROCurve_18, "XGBoost"=xgb_ROCurve_18), legacy.axes = TRUE) + 
  labs(color="Algorithm") + ggtitle("ROC Curves By Model, 2018")

# plotting auc curves for each model in 2019
ggroc(list("Logistic Regression"=log_roc_19,"Naive Bayes"=nb_ROCurve_19,
           "K-Nearest Neighbors"=knn_ROCurve_19, "Random Forest"=rf_ROCurve_19,
           "Decision Tree"=dt_ROCurve_19, "XGBoost"=xgb_ROCurve_19), legacy.axes = TRUE) + 
  labs(color="Algorithm") + ggtitle("ROC Curves By Model, 2019")

# plotting auc curves for each model in 2020
ggroc(list("Logistic Regression"=log_roc_20,"Naive Bayes"=nb_ROCurve_20,
           "K-Nearest Neighbors"=knn_ROCurve_20, "Random Forest"=rf_ROCurve_20, 
           "Decision Tree"=dt_ROCurve_20, "XGBoost"=xgb_ROCurve_20), legacy.axes = TRUE) + 
  labs(color="Algorithm") + ggtitle("ROC Curves By Model, 2020")


