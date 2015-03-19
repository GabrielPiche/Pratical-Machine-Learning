#Getting Data
###Importing data files, NAs and blank cells are treated as missing values
pml.testing <- read.csv("C:/Users/gabriel.piche.IPERCEPTIONS/Desktop/Coursera/Machine Learning/Course Project/pml-testing.csv", header=TRUE, na.strings= c('NA',''))
pml.training <- read.csv("C:/Users/gabriel.piche.IPERCEPTIONS/Desktop/Coursera/Machine Learning/Course Project/pml-training.csv", header=TRUE, na.strings= c('NA',''))
library(caret)
library(dplyr)

##Cleaning Data
###Commented str and summary functions since the outcome was too long.
####str(pml.training)
####summary(pml.training)
###Looking at variability of the variables. We do not want to keep variables with almost no variability

nsv <- nearZeroVar(pml.training, saveMetrics=TRUE)

###Removing variables with almost no variability based on the nearZeroVar function
pml.trainingCleaned <- pml.training[,nsv$nzv=='FALSE']
pml.testingCleaned <- pml.testing[,nsv$nzv=='FALSE']

###A lot of variables contain a lot of missing values
###Removing all variables that have a more than 1000 NAs

NAVar <- sapply(pml.trainingCleaned, function(x) sum(is.na(x)))
pml.trainingCleaned <- pml.trainingCleaned[,colSums(is.na(pml.trainingCleaned))<1000]
pml.testingCleaned <- pml.testingCleaned[,colSums(is.na(pml.testingCleaned))<1000]

###Removing variables that are descriptive
grep('raw_timestamp_part', colnames(pml.trainingCleaned))
grep('cvtd_timestamp', colnames(pml.trainingCleaned))
grep('name', colnames(pml.trainingCleaned))
pml.trainingCleaned <- pml.trainingCleaned[,-c(1,2,3,4,5)]
pml.testingCleaned <- pml.testingCleaned[,-c(1,2,3,4,5)]


##Preparation for the prediction
####Creating a training set to build the model
inTrain <- createDataPartition(y=pml.trainingCleaned$classe, p=0.60, list=FALSE)
training <- pml.trainingCleaned[inTrain,]
testing <- pml.trainingCleaned[-inTrain,]
dim(training)
dim(testing)


####Looking at classe distribution
qplot(classe, data=training, fill=classe, main='Distribution of classe variable')
print(q)

####Creating Folds for Cross-Validation using trainControl function
####Using the trainControl function to create 5-fold cross validation to use in the random forest model
trControl <- trainControl(method = "cv", number = 5)

##Predicting with a Random Forest algorithm

####Fit the random forest algorithm in the training set, using cross-validation
modFit <-train(classe ~., data=training, method='rf', prox=TRUE, trControl=trControl)
modFit

#Looking at the accuracy of the model, using cross-validation.
####The model has an accuracy of over 99%.
q2 <- plot(modFit, ylim = c(.99, 1), main='Accuracy of the Random Forest model')
print(q2)

####Looking at the final model
modFit$finalModel

####Predicting on the test set.
predictions <- as.character(predict(modFit, pml.testing))

####Write the predictions and save as a separate file for all 20 respondents
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id", i, ".txt")
                write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
}

pml_write_files(predictions)


