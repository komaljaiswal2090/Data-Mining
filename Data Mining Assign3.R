
library(caret)
library(plyr)
library(dplyr)
library(C50)
library(kernlab)

#Part 1
testData4<- read.csv("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/57c93be5643d6/6402499?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27bank-test.csv&response-content-type=text%2Fcsv&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20181210T063846Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21599&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20181210%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=e5570b5752bb4e2e4c4a585e7ae6545539e67b30d214587e301c1d5d9f1dc895", header = TRUE)
trainingData4<-read.csv("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/57c93be5643d6/6402500?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27bank-training.csv&response-content-type=text%2Fcsv&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20181210T063920Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20181210%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=a516e1b9fa2bc188e41e436407fe743c7aac2ff31ed39b88d599a89c7482f1b0", header = TRUE)

#View Data
str(trainingData4)
class(testData4)

#drop first column as it is serial number
bank_test2 <- testData4[,-1]
bank_train2 <- trainingData4[,-1]
str(bank_train2)

# Change data types from character factors to numeric then factor
# nominal attributes are 2:10,14,20
nomvars <- c(2:10, 14,20)

# First convert everything to numeric
bank_train2[, 1:20] <-lapply(bank_train2[,1:20], as.numeric)
bank_test2[, 1:20] <-lapply(bank_test2[,1:20], as.numeric)
# Convert nomvar columns to factors
bank_train2[,nomvars] <-lapply(bank_train2[,nomvars], as.factor)
bank_test2[,nomvars] <-lapply(bank_test2[,nomvars], as.factor)
#check if converted
str(bank_train2)
str(bank_test2)
# how many missing values.
sum(is.na(bank_train2))
sum(is.na(bank_test2))

# Factor levels
faclen <- bank_train2 %>% Filter(f = is.factor) %>% sapply(levels) %>% sapply(length) %>% (function(x) x[x<2])
facnames <-names(bank_train2) %in% names(faclen)
bank_train3<- bank_train2[!facnames]

faclen <- bank_test2 %>% Filter(f = is.factor) %>% sapply(levels) %>% sapply(length) %>% (function(x) x[x<2])
facnames <-names(bank_test2) %in% names(faclen)
bank_test3<- bank_test2[!facnames]
str(bank_test3)
# Create a Training Control Object that stores information about how we want to develop(train) the models
# We will use 10 fold cross validation to train and evaluate model

TrainingParameters <- trainControl(method = "cv", number = 10)
# train model
DecTreeModel <- train(bank_train3[,-20], bank_train3$y, 
                      method = "C5.0",
                      trControl= TrainingParameters,
                      preProcess = c("corr", "nzv", "pca"),
                      na.action = na.omit
)

# check tree
DecTreeModel
#Predict
DTPredictions <-predict(DecTreeModel, bank_test3[,-20], na.action = na.pass)
# See predictions
DTPredictions
# Create confusion matrix
cm4<-confusionMatrix(DTPredictions, bank_test3$y,positive = "2")
cm4$overall
cm5 <- confusionMatrix(DTPredictions, bank_test3$y, mode="sens_spec", positive = "2")
cm5$table
cm5$byClass
cm5

#naive bayes
nbModel <- train(bank_train3[,-20],bank_train3$y,  
                      method = "nb",
                      trControl= TrainingParameters,
                 preProcess = c("corr", "nzv", "pca"),
                 na.action = na.omit
)

nbModel
#Predict
nbPredictions <-predict(nbModel, bank_test3[,-20])
# See predictions
nbPredictions
# Create confusion matrix
cm9<-confusionMatrix(nbPredictions, bank_test3$y, positive = "2")
cm9$overall
cm2 <- confusionMatrix(nbPredictions, bank_test3$y, mode="sens_spec", positive = "2")
cm2$table
cm2$byClass
cm2

# train model with neural network
NNModel4 <- train(bank_train3[,-20],bank_train3$y, 
                 method = "nnet",
                 trControl= TrainingParameters,
                 preProcess = c("corr", "nzv", "pca"),
                 tuneGrid = data.frame(size = 5,
                                       decay = 0
                 )
                 
)
NNPredictions3 <-predict(NNModel4, bank_test3[,-20])
# See predictions
NNPredictions3
# Create confusion matrix
cmNN3 <-confusionMatrix(NNPredictions3, bank_test3$y, positive = "2")
cmNN3$overall
cmNN3$table
cmNN3$byClass
cmNN3

# train model with SVM
SVModel <- train(y ~ ., data = bank_train3,
                 method = "svmPoly",
                 trControl= TrainingParameters,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1
                 )
                 
)

SVMPredictions1 <-predict(SVModel, bank_test3[,-20])
# See predictions
SVMPredictions1
# Create confusion matrix
cmSVM1<-confusionMatrix(SVMPredictions1, bank_test3$y, positive = "2")
cmSVM1$overall
cmSVM2<- confusionMatrix(SVMPredictions1, bank_test3$y, mode="sens_spec", positive = "2")
cmSVM2$table
cmSVM2$byClass
cmSVM2


#Part 2 - Models evaluation;Weighted f2 measure
model = c("dec","nb","nn","svm")
recall = c(0.1964,0.4196,0.2232,0.1250)
precision = c(0.5945,0.4563,0.5102,0.5600)
f2measure <- (1+(2**2)) * precision * recall / ((2**2)*precision + recall)
eval_table = data.frame(model,recall,precision,f2measure) 
eval_table


# Part 3-Cost sensitive 
levels(bank_train3$y) <- c("first_class", "second_class")
levels(bank_test3$y) <- c("first_class", "second_class")

cmatrix <- cbind(c(0,1), c(10,0))
cmatrix

CostDTModel2 <- C5.0(y ~., data=bank_train3, cost=cmatrix)
CostDTModel2
summary(CostDTModel2)
plot(CostDTModel2)
#Predict
DTPredictions5<-predict(CostDTModel2, bank_test3[,-20], na.action = na.pass)
# See predictions
DTPredictions5
# Create confusion matrix
cm7<-confusionMatrix(DTPredictions5, bank_test3$y, positive = "second_class")
cm7$overall
cm8 <- confusionMatrix(DTPredictions5, bank_test3$y, mode="sens_spec", positive = "second_class")
cm8$table
cm8$byClass
cm8

#Part 4a- Correlation Matrix
# create training control
econtrol <- trainControl(method="cv", number=5, summaryFunction = twoClassSummary, savePredictions=TRUE, classProbs=TRUE)

# Create models
models1 <- caretList(y ~., data=bank_train3,
                    methodList=c("C5.0", "nb", "nnet", "svmPoly"),
                    trControl = econtrol
)
# Lets see results. note the best performing model
results1 <- resamples(models1)
results1$values
results1$metrics
summary(results1)
dotplot(results1)

# What is model correlation?
mcr1 <-modelCor(results1)
mcr1
# Plot model correlation
splom(results1)
results1

# Part4b and 4c
ensmodel1 <- caretEnsemble(models1,
                          metric = "Accuracy",
                          trControl = trainControl(method="cv", number = 10, classProbs = TRUE)
)

summary(ensmodel1)
enstackpredictions1 <-predict(ensmodel1, bank_test3, na.action = na.omit)
cmdata2<-confusionMatrix(enstackpredictions1, bank_test3$y, mode="everything",positive="second_class")
cmdata2$table


# Create models with sensitivity as optimization metric instead of accuracy
ensmodel2 <- caretEnsemble(models1,
                           metric = "Sens",
                           trControl = trainControl(number = 2, summaryFunction = twoClassSummary,classProbs = TRUE)
)

summary(ensmodel2)
enstackpredictions2 <-predict(ensmodel2, bank_test3, na.action = na.omit)
cmdata1 <-confusionMatrix(enstackpredictions2, bank_test3$y, mode="everything", positive = "second_class")
cmdata1

#model with least correlations are Decision Tree and SVM model.(negative correlation)
models3 <- caretList(y ~., data=bank_train3,
                     methodList=c("C5.0", "svmPoly"),
                     trControl = econtrol
)
ensmodel3 <- caretEnsemble(models3,
                           metric = "Accuracy",
                           trControl = trainControl(method="cv", number = 10, classProbs = TRUE)
)

summary(ensmodel3)
enstackpredictions3 <-predict(ensmodel3, bank_test3, na.action = na.omit)
cmdata3<-confusionMatrix(enstackpredictions3, bank_test3$y, mode="everything",positive="second_class")
cmdata3

