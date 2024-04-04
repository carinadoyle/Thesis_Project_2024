
library(ROSE)

set.seed(0)

# because our outcome is relatively uncommmon, 
# we will balance the dataset by oversampling those with depression

barplot(prop.table(table(data_18$currdepress)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

barplot(prop.table(table(data_19$nspd)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

barplot(prop.table(table(data_20$nspd)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

# creating training and testing sets for 18
n <- nrow(data_18)
set.seed(1)
sample <- sample(n/1.20, replace=F) 

train_18 <- data_18[sample, ]
test_18 <- data_18[-sample, ]

train_18 <- ovun.sample(currdepress~., data=train_18, method = "both",
                            seed = 0, N = nrow(train_18))$data
table(data_18$currdepress)

y_train_18 <- as.integer(train_18$currdepress) -1
y_test_18 <- as.integer(test_18$currdepress) - 1
X_train_18 <- train_18 %>% select(-currdepress)
X_test_18 <- test_18 %>% select(-currdepress)

barplot(prop.table(table(train_18$currdepress)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

# creating training and testing sets for 19
n <- nrow(data_19)
set.seed(1)
sample <- sample(n/1.20, replace=F) 

train_19 <- data_19[sample, ]
test_19 <- data_19[-sample, ]

train_19 <- ovun.sample(nspd~., data=train_19, method = "both",
                        p = 0.5, seed = 0, N = nrow(train_19))$data
table(train_19$nspd)

y_train_19 <- as.integer(train_19$nspd) - 1
y_test_19 <- as.integer(test_19$nspd) - 1
X_train_19 <- train_19 %>% select(-nspd)
X_test_19 <- test_19 %>% select(-nspd)

barplot(prop.table(table(train_19$nspd)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

# creating training and testing sets for 20
n <- nrow(data_20)
set.seed(1)
sample <- sample(n/1.20, replace=F) 

train_20 <- data_20[sample, ]
test_20 <- data_20[-sample, ]

train_20 <- ovun.sample(nspd~., data=train_20, method = "both",
                        p = 0.5, seed = 0, N = nrow(train_20))$data
table(train_20$nspd)

y_train_20 <- as.integer(train_20$nspd) - 1
y_test_20 <- as.integer(test_20$nspd) - 1
X_train_20 <- train_20 %>% select(-nspd)
X_test_20 <- test_20 %>% select(-nspd)

barplot(prop.table(table(train_20$nspd)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")


