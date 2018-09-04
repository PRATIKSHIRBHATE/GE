# Pneumoconiosis

# Random Forest Classification

#setworking directoy
setwd("~/Documents/Data Science/Projects/GE/GE_DataSCience")
inputFile = "CollatedPneumoconiosisData-GE Internal.xlsx"
inputSheet = "LeftUpper"
# Importing the dataset
library("readxl")

dataset = read_excel(inputFile, sheet = inputSheet)

# Encoding the target feature as factor
dataset$Label = factor(dataset$Label)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Label, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[,c(2:40)] = scale(training_set[,c(2:40)])
test_set[,c(2:40)] = scale(test_set[,c(2:40)])

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[,c(2:40)],
                                           y = training_set$Label,
                                           ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[,c(2:40)])

# Making the Confusion Matrix
cm = table(test_set$Label, y_pred)

# Calculating accuracy,sensitivity and specificity
library(caret)
confusionMatrix(cm)

# Tabulating actual and predicted results for each patient id and writing it to csv

outputFile = cbind(test_set[,c(1,41)],y_pred)

write.csv(outputFile, file = "LeftUpper.csv",row.names=FALSE)
