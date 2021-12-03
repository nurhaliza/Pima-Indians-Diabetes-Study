# libraries needed
library(tidyverse)
library(class)
pima <- read_csv("/Users/nurhalizahassan/Desktop/Pima-Indians-Diabetes-Study/pima-indians-diabetes-resampled.csv")
# remove 0 values (NA values) in Glucose column
pima[pima$Glucose != 0, ] 
# normalize Preg, Pedigree, Glucose variables
# === Preg ===
minPreg <- min(pima$Preg)
maxPreg <- max(pima$Preg)
# normpima <- pima %>% mutate(Preg.norm = (Preg - minPreg) / (maxPreg - minPreg)) 
# === Pedigree ===
minPedigree <- min(pima$Pedigree)
maxPedigree <- max(pima$Pedigree)
# normpima <- pima %>% mutate(Pedigree.norm = (Pedigree - minPedigree) / (maxPedigree - minPedigree)) 
# === Glucose === 
minGlucose <- min(pima$Glucose)
maxGlucose <- max(pima$Glucose)
# normpima <- pima %>% mutate(Glucose.norm = (Glucose - minGlucose) / (maxGlucose - minGlucose))
# overall command 
normpima <- pima %>% mutate(Preg.norm = (Preg - minPreg) / (maxPreg - minPreg)) %>% mutate(Pedigree.norm = (Pedigree - minPedigree) / (maxPedigree - minPedigree)) %>% mutate(Glucose.norm = (Glucose - minGlucose) / (maxGlucose - minGlucose)) %>% View()
# split dataset into train (first 500 rows) and test datasets (remaining rows)
trainingset <- pima[1:500,] 
testset <- pima[501:nrow(pima),] 
# consider only Preg and Pedigree variables
# test k-nearest neighbor (knn) classifier for dataset
# knn = 1
# === train dataset ===
trainfeatures <- trainingset %>% select(Preg, Pedigree)
trainlabels <- trainingset %>% select(HasDiabetes)
trainlabels <- trainlabels[["HasDiabetes"]]
# === test dataset === 
testfeatures <- testset %>% select(Preg, Pedigree)
testlabels <- testset %>% select(HasDiabetes)
testlabels <- testlabels[["HasDiabetes"]]
# knn manipulation saved to predicted 
predicted <- knn(train = trainfeatures, test = testfeatures, cl = trainlabels, k = 1) 
# prints out table of predicted 
table(testlabels, predicted)
# error rate computation
error_rate <- 109 / 268
error_rate

# consider including Glucose variable
# test k-nearest neighbor (knn) classifier for dataset
# knn = 1
# === train dataset ===
trainfeatures <- trainingset %>% select(Preg, Pedigree, Glucose)
trainlabels <- trainingset %>% select(HasDiabetes)
trainlabels <- trainlabels[["HasDiabetes"]]
# === test dataset === 
testfeatures <- testset %>% select(Preg, Pedigree, Glucose)
testlabels <- testset %>% select(HasDiabetes)
testlabels <- testlabels[["HasDiabetes"]]
# knn manipulation saved to predicted 
predicted <- knn(train = trainfeatures, test = testfeatures, cl = trainlabels, k = 1) 
# prints out table of predicted 
table(testlabels, predicted)
# error rate computation
error_rate <- 93 / 268
error_rate

# consider all three variables: Preg, Pedigree, and Glucose
# test k-nearest neighbor (knn) classifier for dataset
# knn = 9
# === train dataset ===
trainfeatures <- trainingset %>% select(Preg, Pedigree, Glucose)
trainlabels <- trainingset %>% select(HasDiabetes)
trainlabels <- trainlabels[["HasDiabetes"]]
# === test dataset === 
testfeatures <- testset %>% select(Preg, Pedigree, Glucose)
testlabels <- testset %>% select(HasDiabetes)
testlabels <- testlabels[["HasDiabetes"]]
# knn manipulation saved to predicted 
predicted <- knn(train = trainfeatures, test = testfeatures, cl = trainlabels, k = 9) 
# prints out table of predicted 
table(testlabels, predicted)
# error rate computation
error_rate <- 61 / 268
error_rate

# consider all three variables: Preg, Pedigree, and Glucose
# test k-nearest neighbor (knn) classifier for dataset
# knn = 15
# === train dataset ===
trainfeatures <- trainingset %>% select(Preg, Pedigree, Glucose)
trainlabels <- trainingset %>% select(HasDiabetes)
trainlabels <- trainlabels[["HasDiabetes"]]
# === test dataset === 
testfeatures <- testset %>% select(Preg, Pedigree, Glucose)
testlabels <- testset %>% select(HasDiabetes)
testlabels <- testlabels[["HasDiabetes"]]
# knn manipulation saved to predicted 
predicted <- knn(train = trainfeatures, test = testfeatures, cl = trainlabels, k = 15) 
# prints out table of predicted 
table(testlabels, predicted)
# error rate computation
error_rate <- 64 / 268
error_rate

