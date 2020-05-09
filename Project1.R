library(tidyverse)
library(data.table)
library(purrr)
library(ggplot2)
library(hrbrthemes)
library(caret) 
library(caretEnsemble)
library(dplyr)
library(plyr)
library(base)
library(C50)
library(kernlab)
library(corrplot)
library(gridExtra)
library(partykit)
library(party)




# reading in the data
df<-read.csv("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/57c93be5643d6/6656427?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27WA_Fn-UseC_-Telco-Customer-Churn.csv&response-content-type=text%2Fcsv&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20181204T220011Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20181204%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=ac794c73cee9f19f092641ca3cff140c16a33980b80d842cd919399e98330c58", header = TRUE)

#View Data
str(df)
summary(df)

#Identiying number of missing values

missing<- df %>% 
  summarize_all(funs(sum(is.na(.))))
#There are 11 missing values in the dataset, and all of them are from one variable - "Total Charges".
#Out of 7043 records, 11 records are missing and for the purpose of this EDA, those NA's will be removed.

#data$TotalCharges

row_index<- which(is.na(df$TotalCharges))
row_index

df1<- df[-row_index,]

#Checking how many rows were deleted
nrow(df) - nrow(df1)

# By looking at the variables, there is some wrangling to do.
cols_recode1 <- c(10:15)
for(i in 1:ncol(df1[,cols_recode1])) {
  df1[,cols_recode1][,i] <- as.factor(mapvalues
                                        (df1[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#We will change “No phone service” to “No” for column “MultipleLines”
df1$MultipleLines <- as.factor(mapvalues(df1$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

#Since the minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups: “0–12 Month”, “12–24 Month”, “24–48 Months”, “48–60 Month”, “> 60 Month”
min(df1$tenure); max(df1$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
df1$tenure_group <- sapply(df1$tenure,group_tenure)
df1$tenure_group <- as.factor(df1$tenure_group)

#Change the values in column “SeniorCitizen” from 0 or 1 to “No” or “Yes”.
df1$SeniorCitizen <- as.factor(mapvalues(df1$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

#Remove the columns we do not need for the analysis.
df1$customerID <- NULL
df1$tenure <- NULL
str(df1)

#Exploratory data analysis and feature selection
#Correlation between numeric variables
numeric.var <- sapply(df1, is.numeric)
corr.matrix <- cor(df1[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#The Monthly Charges and Total Charges are correlated. So one of them can be removed from the model. I will remove Total Charges.
df1$TotalCharges <- NULL

#Bar plots of categorical variables
p1 <- ggplot(df1, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(df1, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(df1, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(df1, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(df1, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(df1, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(df1, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(df1, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(df1, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(df1, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(df1, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(df1, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(df1, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(df1, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(df1, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(df1, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(df1, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)

# Lets do stratified sampling. Select rows to based on Class variable as strata
train<- createDataPartition(df1$Churn,p=0.7,list=FALSE)
set.seed(2017)
# Create Training Data as subset 
training<- df1[train,]
# Everything else not in training is test data. Note the - (minus)sign
testing<- df1[-train,]
dim(training); dim(testing)
str(training)

#Methods:
#1. Decision tree

# We will use 10 fold cross validation to train and evaluate model
TrainingParameters <- trainControl(method = "cv", number = 10)
# train model
DecTreeModel <- train(training[,-18], training$Churn, 
                      method = "C5.0",
                      trControl= TrainingParameters,
                      na.action = na.omit
)

# check tree
DecTreeModel
#Predict
DTPredictions <-predict(DecTreeModel, testing[,-18], na.action = na.pass)
# See predictions
DTPredictions
# Create confusion matrix
cm4<-confusionMatrix(DTPredictions, testing$Churn, positive = 'No')
cm4$overall
cm5 <- confusionMatrix(DTPredictions, testing$Churn, mode="sens_spec", positive = 'No')
cm5$table
cm5$byClass
cm5

#Decision tree visualization
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)

#2. Neural network
NNModel4 <- train(training[,-18],training$Churn, 
                  method = "nnet",
                  trControl= TrainingParameters,
                  tuneGrid = data.frame(size = 5,
                                        decay = 0
                  )
                  
)
NNPredictions3 <-predict(NNModel4, testing[,-18])
# See predictions
NNPredictions3
# Create confusion matrix
cmNN3 <-confusionMatrix(NNPredictions3, testing$Churn, positive = 'No')
cmNN3$overall
cmNN3$table
cmNN3$byClass
cmNN3

# 3. SVM
SVModel <- train(Churn ~ ., data = training,
                 method = "svmPoly",
                 trControl= TrainingParameters,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1
                 )
                 
)

SVMPredictions1 <-predict(SVModel, testing[,-18])
# See predictions
SVMPredictions1
# Create confusion matrix
cmSVM1<-confusionMatrix(SVMPredictions1, testing$Churn, positive = "No")
cmSVM1$overall
cmSVM2<- confusionMatrix(SVMPredictions1, testing$Churn, mode="sens_spec", positive = "No")
cmSVM2$table
cmSVM2$byClass
cmSVM2


#Models evaluation;Weighted f0.5 measure
model = c("dec","nn","svm")
recall = c(0.8986,0.8895,0.8973)
precision = c(0.8250,0.83,0.8128)
f0.5measure <- (1+(0.5**2)) * precision * recall / ((0.5**2)*precision + recall)
eval_table = data.frame(model,recall,precision,f0.5measure) 
eval_table

#Correlation Matrix
# create training control
econtrol <- trainControl(method="cv", number=5, summaryFunction = twoClassSummary, savePredictions=TRUE, classProbs=TRUE)

# Create models
models1 <- caretList(Churn ~., data=training,
                     methodList=c("C5.0", "nnet", "svmPoly"),
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

