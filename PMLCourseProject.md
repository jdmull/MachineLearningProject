# PML Weight Training Analysis
Jessica Mullison  
Sunday, January 25, 2015  

## Synopsis
This machine learning analysis uses a random forest prediction model to determine if a machine monitored weight training exercise was done correctly. The random forest model was potentially sutable because of the large number of sample point in the training data set size & and large number of non-linear variables within the training set that affected the outcome. 

## Setting up environmnet & loading the data 
Originally the data for this project came from: http://groupware.les.inf.puc-rio.br/har but the training
& test data were downloaded from the following locations to the project working directory.
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: survival
## Loading required package: splines
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:Hmisc':
## 
##     src, summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:Hmisc':
## 
##     combine
```

```r
## Visual ispection of training table revealed both "NA" and "#DIV/0!" strings in NA positions
tbl <- read.csv("pml-training.csv",header=TRUE,na.strings=c("NA","#DIV/0!"),stringsAsFactors=F)
test <- read.csv("pml-testing.csv",header=TRUE,na.strings=c("NA","#DIV/0!"),stringsAsFactors=F)
test["classe"]<-factor(test$classe)  ## converting classe variable to a factor for building the model

tbl2<-tbl[sapply(tbl, is.numeric)]
## Removed variables with very large numbers of NA's to improve performance & reduce
sparseData<-c()  
for (idx in 1:ncol(tbl2))
  sparseData[idx] <- sum(is.na(tbl2[idx])) > 1000
nzv<-nearZeroVar(tbl2,saveMetrics=TRUE)
tbl2<-tbl2[!nzv$nzv & !sparseData]
tbl2["classe"]<-factor(tbl$classe)
nCols<-ncol(tbl2)
tbl2<-tbl2[6:nCols]  ## stripping off index, timestamp, window variables

## Reserving a portion of the dataset to estimate out of bounds error
set.seed(8883336)
splitIndexes<- createDataPartition(tbl$classe,p=.9,list=FALSE)
validationTbl<-tbl2[-splitIndexes,]
tbl2<-tbl2[splitIndexes,]
tbl<-NULL ## clearing memory
nzv<-NULL ## clearing memory
```

This gives a training dataset with 17662 rows and 51 predictor variables.

## Creating machine learning algorithms and predict activity quality from activity monitors


```r
modFitLDA<-train(classe ~ .,method="lda",data=tbl2,preProcess="pca",times=5)
```

```
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
modelRandomForest <- randomForest(classe~.,data=tbl2, mtry=20,ntree=5,importance=TRUE,do.trace = 1,na.action=na.keep,test=Test)
```

```
## ntree      OOB      1      2      3      4      5
##     1:   5.99%  3.42%  6.73%  7.47%  7.37%  6.63%
##     2:   6.45%  3.58%  7.94%  7.26%  8.65%  6.64%
##     3:   5.83%  3.23%  7.66%  6.48%  7.45%  5.94%
##     4:   5.77%  3.09%  7.45%  6.90%  7.20%  5.85%
##     5:   5.44%  3.07%  7.18%  6.52%  6.81%  5.06%
```

For the random forest model the out of bounds error was automatically calculated with cross validation in the code above.  

## Assessing accuracy by running predictions on reserved data


```r
## what is the out of bounds LDA error
validationLDA<-predict(modFitLDA,validationTbl)
estimatedError<-sum(validationLDA!=validationTbl$classe)/nrow(validationTbl)

## how much of this error is in bounds error?
set.seed(30)

valdiationLDA2<-predict(modFitLDA,validationTbl)
inBoundsError<-sum(validationLDA != valdiationLDA2)/length(validationLDA)
inBoundsError
```

```
## [1] 0
```

```r
outBoundsEstimatedError=estimatedError-inBoundsError
outBoundsEstimatedError
```

```
## [1] 0.4959184
```

```r
## what is the LDA model's accuracy
confLDA <- confusionMatrix(validationLDA,validationTbl$classe)
confLDA$overall
```

```
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.040816e-01   3.713351e-01   4.816997e-01   5.264514e-01   2.846939e-01 
## AccuracyPValue  McnemarPValue 
##   8.976208e-93   8.753848e-24
```

```r
## what is the accuracy of the random forest model?
valdiationRandomForest<-predict(modelRandomForest,validationTbl)
confRandomForest <- confusionMatrix(valdiationRandomForest,validationTbl$classe)
confRandomForest$overall
```

```
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.9841837      0.9799983      0.9776246      0.9892289      0.2846939 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN
```
confLDA


## Predicting activity quality using the Random forest model


```r
ans<-predict(modelRandomForest,test)
```

Predicted exercise quality is: B, A, B, A, A, E, D, B, A, A, B, C, B, A, E, E, A, B, B, B
