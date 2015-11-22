#Loading the library
library(caret)
library(rpart)

#grab necesary data
training=read.csv('C:/Users/a0592zz/Documents/Practical Machine Learning/Course Project/pml-training.csv', header = TRUE)
testing=read.csv('C:/Users/a0592zz/Documents/Practical Machine Learning/Course Project/pml-testing.csv', header = TRUE)

#Getting familiar with the data
summary(training)
head(training)
names(training)

#time series data but the testing data is not time series for a given subjects dumbbell lift
set.seed(32323)

##PRE-PROCESSING
#There are no zero variance data but several near zero variance in whic the summary stats available.
#Test dataset has no summary stats...imputing all the missing cells will not help
nzv<-nearZeroVar(training,saveMetrics=TRUE)
nzv

#Subsetting out the time or data sequence tracking variables as response classe variable in the 
#training dataset is in chronologically order (a simple regression tree will erroneously give almost perfect fit)
#All variables I am calling "time or data sequence tracking" variables are the first seven (7) variables.

#Subsetting out also the summary stats variables as this will not help if test or future prediction dataset is 
#a totally random sample without time series data 
#Pre-processing some more with PCA did not improve the accuracy for the random forest method.

train_subset<-training[,c(8,9,10,37,40,41,42,43,44,45,46,47,48,60,61,62,63,64,65,66,67,68,113,114,115,116,117,118,119,120,121,122,123,124,151,152,153,154,155,156,157,158,159,160)]
test_subset<-testing[,c(8,9,10,37,40,41,42,43,44,45,46,47,48,60,61,62,63,64,65,66,67,68,113,114,115,116,117,118,119,120,121,122,123,124,151,152,153,154,155,156,157,158,159,160)]
names(train_subset)
dim(train_subset)


#regression tree
modFit1<-train(classe~., method="rpart", data=train_subset)
modFit1
modFit1$finalModel

#Variables in regression tree model were roll_belt, pitch_forearm, magnet_bumbbell, roll_foreamr, accel_dumbbell 
#Accuracy low as 51.1%

#Boosting with trees
modFit2<-train(classe~.,method="gbm",data=train_subset, verbose=FALSE)
modFit2$finalModel
modFit2
#accuracy between 67.3% (50 n.trees) to 71.5% (150 n.trees)

#using three repeats of 10-fold cross-validation 
cvCtrl <- trainControl(method = "repeatedcv", repeats = 3)

#random forest without cross validation
modFit3<-train(classe~., method="rf", data=train_subset, prox=TRUE)
modFit3
modFit3$finalModel
#Good accuracy: 71.3% without cross-validation

#random forest with cross validation
modFit4<-train(classe~., method="rf", data=train_subset, prox=TRUE, trControl=cvCtrl)
modFit4
modFit4$finalModel
#Good accuracy: 71.3% without cross-validation

prediction<-predict(modFit3,newdata=test_subset)
 
