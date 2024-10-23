####SUPERVISED LEARNING: CLASSIFICATION#####
##Suited to a target variable that is categorical
##Various Classification methods including Decision Trees, Random Forest, Support Vector Machines(SVM) etc

##We have a dataset called Golf_Gaming to help determine if one can play golf in particular weather conditions
##Our target variable is "Play Golf" which is categorical
##We need to determine if golf will be played on Day 17


#setting up the librarys
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("dlookr")
library(readxl)
library(dplyr)
library(tidyverse)
library(dlookr)

#Set working directory
setwd("/Users/keksmacbookair/Desktop/course works/Intro to Data science/MACHINE LEARNING AND AI-20240627")
# Load the dataset Golf_Gaming
Golf = readxl::read_xlsx("Golf_Gaming.xlsx")
#OR
Golf <- read_excel("Golf_Gaming.xlsx")
head(Golf ,5)#reading the first five rows of the data set
#exploring the data set
is.na(Golf)
colSums(is.na(Golf))#checking eacg specific column for missing data
describe(Golf)
str(Golf)#checking the data set data types for the columns 
#OR
sapply(Golf,class)


########### 1; Split the data into training and testing sets
#Data can be split to help train and test the model
#Data can be split as 70-80% training data, and 20-30% testing data
set.seed(20)
#Split the Golf dataset by 70% training data
train = sample(1:nrow(Golf), nrow(Golf)*0.7)

# View the train dataset formation
train_golf <- Golf[train, ]
str(train_golf)
#The training dataset has 11 observations which is 70% of the dataset

#Split in to testing dataset (30% of dataset)
test <- Golf[-train, ]
str(test)

# Build the decision tree model based on the target variable "Play Golf"
install.packages("rpart")
library(rpart)
golf_tree <- rpart(`Play Golf` ~ ., data = train_golf, method = "class")

#Plot the decision tree
# Plot the decision tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(golf_tree, main = "Decision Tree for Golfing Data")
#The Decision Tree shows that "Yes" golf will be played on day 17.
rpart.plot(golf_tree)



###COMPLETE EXAMPLE FOR DECISION TREES####
#Set working directory
setwd("/Users/keksmacbookair/Desktop/course works/Intro to Data science/MACHINE LEARNING AND AI-20240627")
# Load the dataset Loan_Approval
#The target variable is "Loan_Status" 
Loan = read_csv("Loan_Approval_Data.csv")
view(Loan)
# install the packages
install.packages("MASS") #For decision trees
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("Boruta") #For variable importance
install.packages("cvms")
# Load the libraries
library(caTools)
library(rpart) #builds the model
library(rpart.plot) #plots the decision tree
library(ggplot2)
library(caret)
library(Boruta)
library(cvms)
library(dplyr)
library(MASS)
#Pre-process data 
#1.Pre-process variables
Loan$Gender = as.factor(Loan$Gender)
Loan$Married = as.factor(Loan$Married)
Loan$Dependents = as.factor(Loan$Dependents)
Loan$Education = as.factor(Loan$Education)
Loan$Self_Employed = as.factor(Loan$Self_Employed)
Loan$Property_Area = as.factor(Loan$Property_Area)
Loan$Loan_Status = as.factor(Loan$Loan_Status)

str(Loan)
#2.Pre-process data by removing the unnecessary columns (Loan_ID)
Loan2 = subset(Loan, select = -Loan_ID)

#3.Pre-process data by handling missing data
sum(is.na(Loan2))
#Result: 149 missing data, which is less than 30% of total data
#Let's view the number for each variable
colSums(is.na(Loan2))
#Result: 4 categorical variables missing data (Gender, Married, Dependents, Self_Employed)
#3 numeric variables with missing data (LoanAmount, Loan_Amount_Term, Credit_History)
#Imputing for missing data using median and mode
Loan2 = Loan2 %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
#OR omit missing data
Loan3 = na.omit(Loan2)
#Check the missing data again
colSums(is.na(Loan3))

#Split data in to the target variable and predictor variables
#Split the data in to testing and training
#Data can be split to help train and test the model
#Data can be split as 70-80% training data, and 20-30% testing data
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(Loan3), 0.7 * nrow(Loan3))
train_data <- Loan3[train_indices, ]
test_data <- Loan3[-train_indices, ]

#Use the 70% of the data to train the model
#The Loan_Status is the target variable
#. represents all other variables (or predictors)
model <- rpart(Loan_Status ~ ., data = train_data, method = "class")
model

#Plot the decision tree of the model
library(rpart.plot)
rpart.plot(model)

#Test the model on making predictions using the test data (30% of total dataset)
predictions <- predict(model, newdata = test_data, type = "class")
predictions
#Evaluate the performance of the model
table(predictions, test_data$Loan_Status)
accuracy <- sum(predictions == test_data$Loan_Status) / nrow(test_data)
print(paste("Accuracy:", accuracy))
#Result: The model is 76.047% accurate at making predictions for Loan_Status

