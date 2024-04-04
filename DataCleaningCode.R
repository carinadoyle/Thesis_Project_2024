library(haven) #import data
library(tidyverse)
library(dplyr) #data mining/ data cleaning
library(naniar) #replace with NA
library(leaps) #best subset
library(glmnet) # lasso
library(caret) # KNN 
library(ggplot2) # plotting results
library(randomForest) # Random forest classifier
library(pROC) # AU-ROC curves
library(ROCR) # AU-ROC curves
library(DescTools) # mode imputation

set.seed(0)

# read in datasets
data_18 <- read_sas("chs2018_public.sas7bdat")
data_19 <- read_sas("chs2019_public.sas7bdat")
data_20 <- read_sas("chs2020_public.sas7bdat")

#nspd in 2020, 2019
#currdepress in 2018

# creating factors for categorical variables
index <- 1:ncol(data_18)
data_18[ , index] <- lapply(data_18[ , index], as.factor)
str(data_18)
summary(data_18)

index <- 1:ncol(data_19)
data_19[ , index] <- lapply(data_19[ , index], as.factor)
str(data_19)
summary(data_19)

index <- 1:ncol(data_20)
data_20[ , index] <- lapply(data_20[ , index], as.factor)
str(data_20)
summary(data_20)

# data cleaning and recoding steps for 18
# removing imputed variables or variables that correspond with mental health
data_18 <- data_18 %>% select(-mood63, -mood64, -mood55, -mood56, -mood57, -mood58, -mood59, -mood61, -mood62,
                              -mood11, -imputed_PA08_3R_q1, -imputed_sexuallyactive18, -imputed_sexpartner, 
                              -wt19_dual_q1, -strata_q1, -mood54, -wt19_dual, -cid, -strata, -survey, -qxvers,
                              -imputed_callpoison,-imputed_neighpovgroup4_1317, -imputed_povertygroup,
                              -imputed_povgroup3, -imputed_pov200, -imputed_helpneighbors18_cat, -wt_compare, 
                              -phq8score, -mhtreat18, -tolddepression18)

# string of column names with NAs to replace
missing_names_18 <- colnames(data_18)[colSums(is.na(data_18)) > 0]

# loop to replace NAs with the mode for each question
for (i in missing_names_18){
  mode_18 <- Mode(na.omit(data_18[[i]]))[1]
  data_18[[i]][which(is.na(data_18[[i]]))] <- as.numeric(mode_18)
}

# for variables with many levels, recoding to numeric from factors and replacing NA with mean/mode
data_18$bmi <- as.numeric(data_18$bmi)
data_18$bmi[which(is.na(data_18$bmi))] <- mean(data_18$bmi, na.rm=T)

data_18$lastfobt18 <- as.numeric(data_18$lastfobt18)
data_18$lastfobt18[which(is.na(data_18$lastfobt18))] <- Mode(na.omit(data_18$lastfobt18))[1]

data_18$lastfobt18_45 <- as.numeric(data_18$lastfobt18_45)
data_18$lastfobt18_45[which(is.na(data_18$lastfobt18_45))] <- Mode(na.omit(data_18$lastfobt18_45))[1]

data_18$numberperdaya <- as.numeric(data_18$numberperdaya)
data_18$numberperdaya[which(is.na(data_18$numberperdaya))] <- Mode(na.omit(data_18$numberperdaya))[1]

data_18$smoke5a <- as.numeric(data_18$smoke5a)
data_18$smoke5a[which(is.na(data_18$smoke5a))] <- Mode(na.omit(data_18$smoke5a))[1]

data_18$cost20cigarettes <- as.numeric(data_18$cost20cigarettes)
data_18$cost20cigarettes[which(is.na(data_18$cost20cigarettes))] <- Mode(na.omit(data_18$cost20cigarettes))[1]

# ensuring that no NAs remain in the data
colnames(data_18)[colSums(is.na(data_18)) > 0]

# recoding our outcomes to 1s and 0s
data_18$currdepress <- ifelse(data_18$currdepress==2, 0, 1)
data_18$currdepress <- as.factor(data_18$currdepress)
table(data_18$currdepress)

# data cleaning and recoding steps for 19
# removing imputed variables or variables that correspond with mental health
data_19 <- data_19 %>% select(-mood9, -mood8, -mood11, -wt20_dual_q1, -strata_q1, -cid, -survey, -wt20_dual, 
                              -qxvers, -mood1, -mood2, -mood3, -mood4, -mood5, -mood6, -imputed_neighpovgroup4_1418, 
                              -imputed_povertygroup, -imputed_povgroup3, -imputed_pov200, -imputed_helpneighbors19,
                              -strata, -wt_compare, -k6, -nspdinterfere19, -mhtreat19_all)

# string of column names with NAs to replace
missing_names_19 <- colnames(data_19)[colSums(is.na(data_19)) > 0]

# loop to replace NAs with the mode for each question
for (i in missing_names_19){
  mode_19 <- Mode(na.omit(data_19[[i]]))[1]
  data_19[[i]][which(is.na(data_19[[i]]))] <- as.numeric(mode_19)
}

# for variables with many levels, recoding to numeric from factors and replacing NA with mean/mode
data_19$bmi <- as.numeric(data_19$bmi)
data_19$bmi[which(is.na(data_19$bmi))] <- mean(data_19$bmi, na.rm=T)

data_19$numberperdaya <- as.numeric(data_19$numberperdaya)
data_19$numberperdaya[which(is.na(data_19$numberperdaya))] <- Mode(na.omit(data_19$numberperdaya))[1]

data_19$cost20cigarettes <- as.numeric(data_19$cost20cigarettes)
data_19$cost20cigarettes[which(is.na(data_19$cost20cigarettes))] <- Mode(na.omit(data_19$cost20cigarettes))[1]

data_19$timemammogram19 <- as.numeric(data_19$timemammogram19)
data_19$timemammogram19[which(is.na(data_19$timemammogram19))] <- Mode(na.omit(data_19$timemammogram19))[1]

# ensuring that no NAs remain in the data
colnames(data_19)[colSums(is.na(data_19)) > 0]

# recoding our outcomes to 1s and 0s
data_19$nspd <- ifelse(data_19$nspd==2, 0, 1)
data_19$nspd <- as.factor(data_19$nspd)
table(data_19$nspd)

# data cleaning and recoding steps for 20
# removing imputed variables or variables that correspond with mental health
data_20 <- data_20 %>% select(-mood9, -mood8, -mood11, -wt21_dual_q1, -strata_q1, -cid, -survey, -wt21_dual, 
                              -qxvers, -mood1, -mood2, -mood3, -mood4, -mood5, -mood6, -imputed_neighpovgroup4_1519, 
                              -imputed_povertygroup, -imputed_povgroup3, -imputed_pov200,
                              -strata, -wt_compare, -k6, -mhtreat20_all)

# string of column names with NAs to replace
missing_names_20 <- colnames(data_20)[colSums(is.na(data_20)) > 0]

# loop to replace NAs with the mode for each question
for (i in missing_names_20){
  mode_20 <- Mode(na.omit(data_20[[i]]))[1]
  data_20[[i]][which(is.na(data_20[[i]]))] <- as.numeric(mode_20)
}

# for variables with many levels, recoding to numeric from factors and replacing NA with mean/mode
data_20$bmi <- as.numeric(data_20$bmi)
data_20$bmi[which(is.na(data_20$bmi))] <- mean(data_20$bmi, na.rm=T)

data_20$numberperdaya <- as.numeric(data_20$numberperdaya)
data_20$numberperdaya[which(is.na(data_20$numberperdaya))] <- Mode(na.omit(data_20$numberperdaya))[1]

data_20$cost20cigarettes <- as.numeric(data_20$cost20cigarettes)
data_20$cost20cigarettes[which(is.na(data_20$cost20cigarettes))] <- Mode(na.omit(data_20$cost20cigarettes))[1]

data_20$toohighblsugar <- as.numeric(data_20$toohighblsugar)
data_20$toohighblsugar[which(is.na(data_20$toohighblsugar))] <- Mode(na.omit(data_20$toohighblsugar))[1]

# ensuring that no NAs remain in the data
colnames(data_20)[colSums(is.na(data_20)) > 0]

# recoding our outcomes to 1s and 0s
data_20$nspd <- ifelse(data_20$nspd==2, 0, 1)
data_20$nspd <- as.factor(data_20$nspd)
table(data_20$nspd)


