
  ## Required packages 
  
  

library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(Hmisc)
library(outliers)
library(InformationValue)



## Executive Summary 

#

## Data 

#The Titanic data from Kaggle.com was used for this particular project. This dataset has information about 889 passengers on the Titanic contained in 12 variables. 


#read data
titanic_train <- read.csv("/Users/Vidya/Downloads/train.csv", header = T, na.strings = c(""))
training.data.raw <- read.csv('/Users/Vidya/Downloads/train.csv',header=T,na.strings=c(""))



#dimension of the dataframe
str(titanic_train)
View(titanic_train)

# find sum of NA values in the data
sum(is.na(titanic_train))

sum(is.na(titanic_train$Cabin))
sapply(titanic_train, function(x) sum(is.na(x)))
sapply(titanic_train, function(x) length(unique(x)))


#There are 687 missing values in the cabin variable. This can be removed from the training dataset. The passenger ID column also does n ot contribute any meaningful information to the data as it only records a unique id of the passenger. The new training data is subset fom the original dataset.

titanic_subset <- subset(titanic_train, select= c(2:10, 12))
View(titanic_subset)

#The age variable has 177 missing values. This can be dealt with by imputing the age value.




titanic_subset$Age[is.na(titanic_subset$Age)] <- mean(titanic_subset$Age, na.rm =T)
titanic_subset <- titanic_subset[!is.na(titanic_subset$Embarked),]
rownames(titanic_subset) <- NULL
str(titanic_subset)


table(titanic_subset$Survived)


#train <- titanic_subset[1:800,]

#test <- titanic_subset[801:889,]

train <- titanic_subset[1:800,]
test <- titanic_subset[801:889,]

model <- glm(Survived ~ Sex + Pclass ,family=binomial(link='logit'),data=train)

#model <- glm(Survived ~., family = binomial(link ='logit'), data = train)

summary(model)

predicted <- plogis(predict(model, test))  # predicted scores


optCutOff <- optimalCutoff(test$Survived, predicted)[1] 

misClassError(test$Survived, predicted, threshold = optCutOff)


plotROC(test$Survived, predicted)

confusionMatrix(test$Survived, predicted, threshold = optCutOff)



  