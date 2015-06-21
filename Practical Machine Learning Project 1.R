#The goal of your project is to predict the manner in which they did the exercise. This is the 
#"classe" variable in the training set. You may use any of the other variables to predict with. 
#You should create a report describing:
#1. How you built your model, how you used cross validation, 
#2. What you think the expected out of sample error is, and why you made the choices you did. 
#3. You will also use your prediction model to predict 20 different test cases. 

library(AppliedPredictiveModeling)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)


pmltesting<-data.frame(read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!","")))
pmltesting<-subset(pmltesting, na.strings=c("NA","#DIV/0!",""))
pmltesting<-pmltesting[, colSums(is.na(pmltesting))==0]
pmltesting <- pmltesting[,8:length(colnames(pmltesting))]

pmltrain<-data.frame(read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!","")))
pmltrain<-subset(pmltrain, na.strings=c("NA","#DIV/0!",""))
pmltrain<-pmltrain[, colSums(is.na(pmltrain))==0]
pmltrain <- pmltrain[,8:length(colnames(pmltrain))]
nearZeroVar(pmltrain, saveMetrics=TRUE)



set.seed(2)
pmltrain2 <- createDataPartition(pmltrain$classe, p=0.6)[[1]]
pmltrain_training <- pmltrain[pmltrain2,]
pmltrain_testing <- pmltrain[-pmltrain2,]

#classification trees (method = rpart)

mf <- train(pmltrain_training$classe ~ ., data = pmltrain_training, method="rpart")

predmf <- predict(mf, newdata=pmltrain_testing)
confusionMatrix(predmf, pmltrain_testing$classe)

print(mf$finalModel, digits=3)

fancyRpartPlot(mf$finalModel)



#random forests (method = rf)
set.seed(2)

mf2<-randomForest(classe ~. , data=pmltrain_training)
print(mf2)
varImp(mf2)

predmf2<-predict(mf2,pmltrain_testing)

print(confusionMatrix(predmf2, pmltrain_testing$classe), digits=4)


#Final Testing

testset=predict(mf2, newdata=pmltesting)
print(testset)



