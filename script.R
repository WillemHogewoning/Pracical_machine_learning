# Course Project
library(tidyverse)
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
set.seed(1000)

#Download

dir.create("./data")

url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url_train, destfile = "./data/training.csv")
download.file(url_test, destfile = "./data/test.csv")


#Import
trainset <- read.csv("./data/training.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
test <- read.csv("./data/test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

inTrain = createDataPartition(trainset$classe, p = 3/4)[[1]]
training = trainset[ inTrain,]
testing = trainset[-inTrain,]

#remove NA's colums
training <- training[ , colSums(is.na(training)) < 1000]

#remove empty columns
a <- sapply(training, function(x) sum(x == "")) >1000
training <- training[ , a == FALSE ]
rm(a)
dataset <- training
dataset <- dataset[,8:length(dataset)]
dataset$classe <- as.factor(dataset$classe)


testset <- testing
testset$classe <- as.factor(testset$classe)


corMatrix <- cor(dataset[, -53])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

#test and traindata

library(randomForest)
set.seed(1000)
modFit <- randomForest(classe~., data = dataset)
print(modFit)

predict1 <- predict(modFit, testset, type = "class")
confusionMatrix(testset$classe, predict1)


predict_FINAL <- predict(modFit, test)
print(predict_FINAL)

#end checks

accuracy <- postResample(predict1, testset$classe)
accuracy

#Out of Sample Error rate

outsamperr <- 1 - as.numeric(confusionMatrix(testset$classe, predict1)$overall[1])
outsamperr

qplot(classe, predict1, data=testset,  colour= classe, geom = c("boxplot", "jitter"),
      main = "predicted vs. observed in validation data", xlab = "Observed Classe", ylab = "Predicted Classe")


plot(modFit)
