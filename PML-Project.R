## R script for the Practical Machine Learning Course Project

## Load the necessary libraries

library(caret)
library(ggplot2)
library(dplyr)
library(randomForest)
library(e1071)
library(rattle)

## Read the data

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")

validation <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

## The training data set has 19622 observations and 160 variables
## The testing data set has 20 observations and 160 variables

## Let's clean the data before fitting the models

## Remove the first seven columns since are the ones that don't work for the model
training <- training[,8:160]

## Remove all columns that have at least one empty row
training <- training[,colSums(is.na(training))==0]

## Remove all variables which its variance is small

ZeroVar <- nearZeroVar(training)
training <- training[,-ZeroVar]

## Split the data set into training & testing

inTrain <- createDataPartition(y=training$classe,p=0.7,list=FALSE)
trainData <- training[inTrain,]
testData <- training[-inTrain,]

## Match the remaining variables to the validation set
## Function defined below

RemCols <- colnames(training)
finalValidation <- RemainingDF(RemCols, validation)

## Due to the naturality of the problem I'll start with a random forest model

fit1 <- randomForest(classe~.,data=trainData)

confusionMatrix(predict(fit1,testData),testData$classe)$overall

## Overall accuracy for the train set was 0.99. There doesn´t seem to be necessary
## to make another model





RemainingDF <- function(namesCol, newDF){
    
    x <- c()
    for(i in 1:159){
        for(j in 1:length(namesCol)){
            LogTest <- colnames(newDF)[i] == namesCol[j]
            if(LogTest == TRUE){
                x <- c(x,i)
                break
                
            }
        }    
    }
    return(newDF[,c(x,160)])
}

