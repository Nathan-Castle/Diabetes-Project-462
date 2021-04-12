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
require(caret)
require(glmnet)
set.seed(42) # Set a seed to ensure repeatable random samples

##### Manipulating the Data #####

dt <- fread("C:/Users/Nathan/Desktop/BUS 462/Group Project/Diabetes/diabetes_data_upload.csv")
head(dt)
tail(dt)

stat.desc(dt)
str(dt)

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

##### Summary Stats #####
stargazer(dt2, type="text")
stat.desc(dt2)
chart.Correlation(dt2)

##### Creating Age Buckets for Control ####

#Check the range of ages 
range(dt$Age) #16-90

#Visualize the Histogram
hist(dt2$Age,breaks=5) #5 age ranges

#Separate age into groups

dt2$Agebucket <- ifelse(dt2$Age <= 40, "U40", ifelse(dt2$Age <= 60, "40to60", "A60")) #convert to factor 
dt2$Agebucket <- as.factor(dt2$Agebucket)
table(dt2$Agebucket)

#####Shuffling data for training groups 80/20 ####
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

#####BUILDING THE MODELS #####

#LOGIT Model ####

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


#LASSO LOGIT Model ####



x <- model.matrix(class ~., data_train)[,-1]
#y <- ifelse(dt2$class == "pos", 1,0)
y <- ifelse(data_train$class == "1", 1,0)

glmnet(x, y, family = "binomial", aplha = 1, lambda = NULL)

cv.lasso <- cv.glmnet(x,y, alpha = 1, family = "binomial",
                      lamda = cv.lasso$lamda.min)

model_lasso <- glmnet(x,y, alpha = 1, family = "binomial",
                lamda = cv.lasso$lambda.min)

coef(model_lasso)
plot(model_lasso)
plot(cv.lasso)

#Prediction
x.test.lasso <- model.matrix(class~., data_test)[,-1]
probabilities_lasso <- model_lasso %>% predict(newx = x.test.lasso)
predicted.classes.lasso <- ifelse(probabilities_lasso > 0.5, "pos", "neg")
predicted.classes.lasso <- as.factor(predicted.classes.lasso)
predicted.classes.lasso <- as.integer(predicted.classes.lasso)
predicted.classes.lasso <- predicted.classes.lasso - 1
#Accuracy
observed.classes.lasso <- data_test$class
mean(predicted.classes.lasso == observed.classes.lasso)

#CART Model ####
fitd <- rpart(class ~ ., data=data_train, method = "class") 
rpart.plot(fitd, extra = 101)

# Prediction

predict_unseen <- predict(fitd, data_test, type = "class")

#Confusion Matrix
table_mat <- table(data_test$class, predict_unseen)
table_mat

# Perfromance Measures
TP = table_mat[1,1]
FN = table_mat[2,1]
FP = table_mat[1,2]
TN = table_mat[2,2]

accuracy_test <- (TP+TN)/sum(table_mat)
print(paste('Accuracy for test', accuracy_test))

#k-Fold Cross Validation ####
require(e1071)

dt2$class <- as.factor(dt2$class)
train_control <- trainControl(method="cv", number=3, savePredictions = TRUE)
modelk <- train(class ~ ., data = dt2, method = "rpart", trControl=train_control)

print(modelk)
plot(modelk)
rpart.best <- modelk$finalModel
rpart.best

rpart.plot(rpart.best, extra = 101)
