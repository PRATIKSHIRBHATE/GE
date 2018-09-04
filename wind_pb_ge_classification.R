# Random Forest Classification

#setworking directoy
setwd("~/Documents/Data Science/Projects/GE/GE_DataSCience")

# Importing the dataset
dataset = read.csv('WindFarm_2min_AnalyticsEngineer_wWS.csv')

dataset = dataset[7:21]

dataset$ShearTypeClass = factor(dataset$ShearTypeClass)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$ShearTypeClass, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[1:14] = scale(training_set[1:14])
test_set[1:14] = scale(test_set[1:14])

# Applying PCA
# install.packages('caret')
#library(caret)
# install.packages('e1071')
#library(e1071)
#pca = preProcess(x = training_set[-20], method = 'pca', pcaComp = 2)
#training_set = predict(pca, training_set)
#training_set = training_set[c(2, 3, 1)]
#test_set = predict(pca, test_set)
#test_set = test_set[c(2, 3, 1)]

# Fitting Random Forest Classification to the Training set
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[1:14],
                          y = training_set$ShearTypeClass,
                          ntree = 100)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[1:14])

# Making the Confusion Matrix
cm = table(test_set[, 15], y_pred)

# Calculating accuracy,sensitivity and specificity
library(caret)
confusionMatrix(cm)