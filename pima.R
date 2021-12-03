# libraries needed
library(tidyverse)
library(class)

pima <- read_csv("/Users/nurhalizahassan/Desktop/HW/pima-indians-diabetes-resampled.csv")
# remove 0 values (NA values) in Glucose column
pima[pima$Glucose != 0, ] 
# normalize Preg, Pedigree, Glucose variables
# = Preg =
minPreg <- min(pima$Preg)
maxPreg <- max(pima$Preg)
# normpima <- pima %>% mutate(Preg.norm = (Preg - minPreg) / (maxPreg - minPreg)) 
# = Pedigree =
minPedigree <- min(pima$Pedigree)
maxPedigree <- max(pima$Pedigree)
# normpima <- pima %>% mutate(Pedigree.norm = (Pedigree - minPedigree) / (maxPedigree - minPedigree)) 
# = Glucose = 
minGlucose <- min(pima$Glucose)
maxGlucose <- max(pima$Glucose)
# normpima <- pima %>% mutate(Glucose.norm = (Glucose - minGlucose) / (maxGlucose - minGlucose))
# overall command 
normpima <- pima %>% mutate(Preg.norm = (Preg - minPreg) / (maxPreg - minPreg)) %>% mutate(Pedigree.norm = (Pedigree - minPedigree) / (maxPedigree - minPedigree)) %>% mutate(Glucose.norm = (Glucose - minGlucose) / (maxGlucose - minGlucose)) %>% View()
# split dataset into train (first 500 rows) and test datasets (remaining rows)
trainingset <- pima[1:500,] 
testset <- pima[501:nrow(pima),] 



