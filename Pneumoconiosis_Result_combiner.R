# Pneumoconiosis Combined results for Right and Left (Upper,Middle,Lower) X Ray

# Imprting required libraries
library(dplyr)
# Reading the outputs for each sections
setwd("~/Documents/Data Science/Projects/GE/GE_DataSCience")

RightUpper = read.csv("RightUpper.csv")
RightMiddle = read.csv("RightMiddle.csv")
RightLower = read.csv("RightLower.csv")
LeftUpper = read.csv("LeftUpper.csv")
LeftMiddle = read.csv("LeftMiddle.csv")
LeftLower = read.csv("LeftLower.csv")

# Comparing the actual and predicted results based on all 6 files

output_dataset = merge(RightUpper, RightMiddle, by='PatientNumMasked', all = TRUE) %>%
  merge(., RightLower, by='PatientNumMasked', all = TRUE) %>%
  merge(., LeftUpper, by='PatientNumMasked', all = TRUE) %>%
  merge(., LeftMiddle, by='PatientNumMasked', all = TRUE) %>%
  merge(., LeftLower, by='PatientNumMasked', all = TRUE)

output_dataset[is.na(output_dataset)] <- 0
output_dataset$Actual =  apply(output_dataset[c(2,4,6,8,10,12)], 1, FUN=max)
output_dataset$Predict =  apply(output_dataset[c(3,5,7,9,11,13)], 1, FUN=max)

# Making the Confusion Matrix

(cm = table(output_dataset$Actual,output_dataset$Predict))

# Calculating accuracy,sensitivity and specificity
library(caret)
confusionMatrix(cm)

