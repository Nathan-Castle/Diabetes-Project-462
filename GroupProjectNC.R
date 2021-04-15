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

dt_main <- cbind(dtage, dt1)
colnames(dt_main)[1] <- "Age"

dt_main <- na.omit(dt_main)

##### Summary Stats #####
stargazer(dt_main, type="text")
stat.desc(dt_main)
chart.Correlation(dt_main)

##### Creating Age Buckets for Control ####

#Check the range of ages 
range(dt_main$Age) #16-90

#Visualize the Histogram
hist(dt_main$Age,
     breaks=5,
     main = "Histogram of Age Ranges",
     xlab = "Age",
     col = "grey",
     border = "black") #5 age ranges

#Separate age into groups

dt_main$Agebucket <- ifelse(dt_main$Age <= 40, "U40", ifelse(dt_main$Age <= 60, "40to60", "A60")) #convert to factor 
dt_main$Agebucket <- as.factor(dt_main$Agebucket)
table(dt_main$Agebucket)

#####Shuffling data for training groups 80/20 ####
shuffle_index <- sample(1:nrow(dt_main))
dt_main <- dt_main[shuffle_index, ]
head(dt_main)

#Create train/test data
#80% train, bottom 20% test
n_cut <- round(nrow(dt_main)*0.8,0)
data_train <- dt_main[1:n_cut, ]
data_test <- dt_main[(n_cut+1):nrow(dt_main), ]

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
accuracy_logit <- mean(predicted.classes == data_test$class)

print(paste('Accuracy for test', accuracy_logit))

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

#data_train[2:18] <- lapply(data_train[2:18], as.factor)

cart.KS <- rpart(class ~ ., data=data_train, method = "class") 
rpart.plot(cart.KS, extra = 101)

# Prediction

predict_unseen <- predict(cart.KS, data_test, type = "class")

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

dt_main$class <- as.factor(dt_main$class)
train_control <- trainControl(method="cv", number=3, savePredictions = TRUE)
modelk <- train(class ~ ., data = dt_main, method = "rpart", trControl=train_control)

print(modelk)
plot(modelk)
rpart.best <- modelk$finalModel
rpart.best

rpart.plot(rpart.best, extra = 101)

