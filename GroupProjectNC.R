#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
require(data.table)
require(dplyr)
require(ggplot2)
require(stargazer)
require(PerformanceAnalytics)
require(MASS)
require(corrr)
require(rpart)
require(rpart.plot)
require(pastecs)
require(PerformanceAnalytics)
set.seed(42) # Set a seed to ensure repeatable random samples

##### Manipulating the Data #####

dt <- fread("C:/Users/Nathan/Desktop/BUS 462/Group Project/Diabetes/diabetes_data_upload.csv")
head(dt)
tail(dt)

stat.desc(dt)

dtage <- dt$Age
dtage <- data.frame(dtage)

dt1 <- dt[,c(-1)]
dt1 <- lapply(dt1, factor)
dt1 <- lapply(dt1, as.integer)
dt1 <- data.frame(dt1)
dt1 <- dt1 - 1

dt2 <- cbind(dtage, dt1)
colnames(dt2)[1] <- "Age"

dt2 <- na.omit(dt2)

##### Creating Age Buckets for Control ####

#Check the range of ages 
range(dt$Age) #16-90

#Visualize the Histogram
hist(dt2$Age,breaks=5) #5 age ranges

#Separate age into groups

testage <- split(dt2, cut(dt2$Age, c(0,20,40,60,80,100), include.lowest = TRUE))

age0_20 <- testage[1]

age21_40 <- testage[2]
dftest <- data.frame(matrix(sapply(age21_40, c)))

age41_60 <- testage[3]
age61_80 <- testage[4]
age81_100 <- testage[5]

kstestage <- glm(class ~ ., data=age21_40, family = "binomial")

##### Summary Stats #####
stargazer(dt2, type="text")
stat.desc(dt2)
chart.Correlation(dt2)

#Shuffling data for training groups
shuffle_index <- sample(1:nrow(dt2))
dt2 <- dt2[shuffle_index, ]
head(dt2)

#Create train/test data
#80% train, bottom 20% test
n_cut <- round(nrow(dt2)*0.8,0)
data_train <- dt2[1:n_cut, ]
data_test <- dt2[(n_cut+1):nrow(dt2), ]

#check dim
dim(data_train)
dim(data_test)

# test distribution of people who got diabetes
prop.table(table(data_train$class))
prop.table(table(data_test$class))

#####BUILDING THE MODEL #####

#LOGIT Model

m.LOGIT.KS <- glm(class ~ ., data=data_train, family="binomial")
stargazer(m.LOGIT.KS, type = "text")
AIC(m.LOGIT.KS)

# Make predictions
probabilities <- m.LOGIT.KS %>% predict(data_test, type = "response")
hist(probabilities)

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes <- as.factor(predicted.classes)
predicted.classes <- as.integer(predicted.classes)
predicted.classes <- predicted.classes - 1

# Model accuracy
mean(predicted.classes == data_test$class)

table_logit <- table(data_test$class, predicted.classes)
table_logit

TP_l = table_logit[1,1]
FN_l = table_logit[2,1]
FP_l = table_logit[1,2]
TN_l = table_logit[2,2]


accuracy_test_logit <- (TP_l+TN_l)/sum(table_logit)
print(paste('Accuracy for test', accuracy_test_logit))


#LASSO LOGIT Model


#CART Model
fitd <- rpart(class ~ ., data=data_train, method = "class") 
rpart.plot(fitd, extra = 100)

#Now start looking into k-fold cross validation


##### Prediction #####

predict_unseen <- predict(fitd, data_test, type = "class")

#Confusion Matrix
table_mat <- table(data_test$class, predict_unseen)
table_mat

##### Perfromance Measures #####
TP = table_mat[1,1]
FN = table_mat[2,1]
FP = table_mat[1,2]
TN = table_mat[2,2]


accuracy_test <- (TP+TN)/sum(table_mat)
print(paste('Accuracy for test', accuracy_test))

recall_test <- TP/(TP+FN)
print(paste('Recall for test', recall_test))

precision_test <- TP/(TP+FP) #performance
print(paste('Precision for test', recall_test))

f1_test <- precision_test*recall_test/(precision_test+recall_test)
print(paste('F1 score is', recall_test))
