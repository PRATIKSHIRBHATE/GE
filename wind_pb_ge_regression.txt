# Random Forest Classification

library(dplyr)
#setworking directoy
setwd("~/Documents/Data Science/Projects/GE/GE_DataSCience")

# Importing the dataset
dataset = read.csv('WindFarm_2min_AnalyticsEngineer_wWS.csv')

dataset = dataset %>% mutate(alpha = log(m38/m78)/log(38/78.7))

dataset = dataset[c(7:20,22)]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$alpha, SplitRatio = 0.8)
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
regressor = randomForest(x = training_set[1:14],
                          y = training_set$alpha,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set[1:14])

y_pred = as.data.frame(y_pred)
# Error calculation
RMSE = function(predict, actual){
  sqrt(mean((predict - actual)^2))
}

RMSE(y_pred,test_set$alpha)


# Fitting linear model
regressor_lm = lm(formula = alpha ~ .,
               data = training_set)

# Predicting the Test set results
y_pred_lm = predict(regressor_lm, newdata = test_set)
y_pred_lm = as.data.frame(y_pred_lm)
# Error calculation
summary(regressor_lm)