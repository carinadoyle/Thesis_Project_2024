
set.seed(0)

# feature selection steps for 2018
n <- nrow(data_18)
set.seed(1)
sample <- sample(n/2, replace=F) 

grep("currdepress", colnames(data_18))

tr_dat <- data_18[sample, ]
te_dat <- data_18[-sample, ]

lasso_x_tr <- as.matrix(tr_dat[, -32])
lasso_x_tr
lasso_y_tr <- tr_dat[, 32, drop = T]
lasso_x_te <- as.matrix(te_dat[, -32])
lasso_y_te <- te_dat[, 32, drop = T]

cv_fit_lasso <- cv.glmnet(data.matrix(tr_dat[, -32]), factor(lasso_y_tr), family="binomial", alpha=1)
lasso_lambda <- cv_fit_lasso$lambda.min
lasso_coef <- coef(cv_fit_lasso,lasso_lambda)
lasso.model <- glmnet(data.matrix(tr_dat[, -32]), factor(lasso_y_tr),  alpha=1, family="binomial",lambda = cv_fit_lasso$lambda.min)

lasso_tr_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(tr_dat[, -32]), type="class")
lasso_te_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(te_dat[, -32]), type="class")

lasso_tr_error <- mean(lasso_tr_pred != lasso_y_tr)
lasso_te_error <- mean(lasso_te_pred != lasso_y_te)
lasso_tr_error
lasso_te_error

cbind(lasso_coef)

data_18 <- data_18 %>% select(currdepress, newrace, agegroup5, generalhealth, insuredgateway18, pcp18, didntgetcare18, 
                              medcost18, checkedbp18, everasthma, currentasthma18, delaypayrent, smoker, 
                              agesmkever, smellcigsmoke, understandsexualid, employment18, twoplussoda, 
                              enoughfood, exercise18,
                              sleepquality_q1, everusedprep18, drinker, ipvphy, insultipv)
# , avgsugarperday18 smokecat, cpd18a nsugardrinkperday18, nsodasugarperday18,numberperdaya,
# feature selection steps for 2019
n <- nrow(data_19)
set.seed(1)
sample <- sample(n/2, replace=F) 

tr_dat <- data_19[sample, ]
te_dat <- data_19[-sample, ]

grep("nspd", colnames(data_19))

lasso_x_tr <- as.matrix(tr_dat[, -32])
lasso_y_tr <- tr_dat[, 32, drop = T]
lasso_x_te <- as.matrix(te_dat[, -32])
lasso_y_te <- te_dat[, 32, drop = T]

cv_fit_lasso <- cv.glmnet(data.matrix(tr_dat[, -32]), factor(lasso_y_tr), family="binomial", alpha=1)
lasso_lambda <- cv_fit_lasso$lambda.min
lasso_coef <- coef(cv_fit_lasso,lasso_lambda)
lasso.model <- glmnet(data.matrix(tr_dat[, -32]), factor(lasso_y_tr),  alpha=1, family="binomial",lambda = cv_fit_lasso$lambda.min)

lasso_tr_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(tr_dat[, -32]), type="class")
lasso_te_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(te_dat[, -32]), type="class")

lasso_tr_error <- mean(lasso_tr_pred !=lasso_y_tr)
lasso_te_error <- mean(lasso_te_pred !=lasso_y_te)
lasso_tr_error
lasso_te_error

cbind(lasso_coef)

data_19 <- data_19 %>% select(nspd, nutrition47, agegroup5, birthsex, generalhealth, 
                              pcp19, visitnonpcp12m19, didntgetcare19, visitdds19, toldprescription19, 
                              checkedbp19, currentasthma19, delaypayrent, wallmold, cpd19a, heavysmoker19a, 
                              smokecat, mentholcigs19, sourcelastcig, cost20cigarettes, 
                              smokeecig30days, usborn, maritalstatus19, education, employment19, 
                              exercise19,
                              difficultdailyact, assistdevice, cogdecline, discusscogdec, unpaidcare, fluvaccineshot, 
                              hiv12months19, wsw, sexuallyactive19, drinker, heavydrink19, sexasltrape,
                              insure19r)


# agegroup6, age18_64, everydaycpda, avgsugarperday19, nsodasugarperday19, avgpresweetenedperday19_q1, analstdtest

# feature selection steps for 2020
n <- nrow(data_20)
set.seed(1)
sample <- sample(n/2, replace=F) 

tr_dat <- data_20[sample, ]
te_dat <- data_20[-sample, ]

grep("nspd", colnames(data_20))

lasso_x_tr <- as.matrix(tr_dat[, -35])
lasso_y_tr <- tr_dat[, 35, drop = T]
lasso_x_te <- as.matrix(te_dat[, -35])
lasso_y_te <- te_dat[, 35, drop = T]

cv_fit_lasso <- cv.glmnet(data.matrix(tr_dat[, -35]), factor(lasso_y_tr), family="binomial", alpha=1)
lasso_lambda <- cv_fit_lasso$lambda.min
lasso_coef <- coef(cv_fit_lasso,lasso_lambda)
lasso.model <- glmnet(data.matrix(tr_dat[, -35]), factor(lasso_y_tr),  alpha=1, family="binomial",lambda = cv_fit_lasso$lambda.min)

lasso_tr_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(tr_dat[, -35]), type="class")
lasso_te_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(te_dat[, -35]), type="class")

lasso_tr_error <- mean(lasso_tr_pred !=lasso_y_tr)
lasso_te_error <- mean(lasso_te_pred !=lasso_y_te)
lasso_tr_error
lasso_te_error

cbind(lasso_coef)

data_20 <- data_20 %>% select(nspd, newrace6, agegroup5, generalhealth, pcp20, didntgetcare20, 
                              skiprxcost, toldprescription20, takingmeds20 , checkedbp20_q1, diabcntrlmeds, 
                              toohighblsugar, stillasthmaall, delaypayrent, rodentsstreet, proudneigh, 
                              cpd20a, mentholcigs20, cost20cigarettes, cigarillo20_q1, smokeecig30days20_q1, 
                              smokeecig30days20_q1, smellcigsmoke20_q1, maritalstatus20, education, 
                              employment20, whereflu20 , twoplussoda, avgsodasugarperday20, cyclingfreq, 
                              swim, difficultdailyact, colonoscopy10yr_45, analsex, wswexclusive, sexuallyactive20,
                              everheardofprep, everusedprep20, heavydrink20, ipvphy, insultipv)

# ssb, daysalc30, nutrition1

ncol(data_18)
ncol(data_19)
ncol(data_20)





