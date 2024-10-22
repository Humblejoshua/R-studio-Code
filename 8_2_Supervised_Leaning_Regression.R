####SUPERVISED LEARNING: REGRESSION ####
#This model assumes a linear relationship between with a target continuous variable.
#Two types exist:
#1. Simple linear regression; when we have only one input variable
#2. Multiple linear regression; when there are multiple input variables

#Set working directory
setwd("/Users/keksmacbookair/Desktop/course works/Intro to Data science/8_2_Supervised_Regression")
#Install packages
install.packages("corrplot")
install.packages("nortest")
install.packages("ISLR")
install.packages("Hmisc")
install.packages("ModelMetrics")
install.packages("lmtest")
install.packages("car")
install.packages("olsrr")
install.packages("moments")
install.packages("bestNormalize")
install.packages("magrittr")
install.packages("ggcorrplot")
#Import libraries
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(rpart) #builds the model
library(rpart.plot) #plots the decision tree
library(ggplot2)
library(caret)
library(Boruta)
library(cvms)
library(dplyr)
library(MASS)
library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(car)
library(olsrr)
library(moments)
library(bestNormalize)
library(magrittr)
library(ggcorrplot)

####1. SIMPLE LINEAR REGRESSION ##
#Using bike_sales dataset
Data<- read_csv("Bike_Sales_New.csv")
describe(Data)
head(Data)
str(Data)
#Predicting Revenue a continuous variable
##A. VARIABLE/FEATURE SELECTION ##
#Since regression assumes a linear relationship between the input and target variable, we have to select variables that are most linear in relationship
#So subset data and check relationships to the target variable "Revenue"

#i)Turn categorical variables in to factors
Data$Date = as.factor(Data$Date)
Data$Month = as.factor(Data$Month)
Data$Age_Group = as.factor(Data$Age_Group)
Data$Customer_Gender = as.factor(Data$Customer_Gender)
Data$Country = as.factor(Data$Country)
Data$State = as.factor(Data$State)
Data$Sub_Category = as.factor(Data$Sub_Category)
Data$Product_Category = as.factor(Data$Product_Category)
Data$Product = as.factor(Data$Product)


#ii)Subset data to categorical and continuous
#Remove unnecessary columns if any (X in my dataset)
Data = subset(Data, select = -(X))
#Subset categorical variables
Cat = Data[c("Date", "Month", "Age_Group", "Customer_Gender", "Sub_Category", "Product", "Product_Category", "Country", "State")]
#Subset continuous variables without the target variable "Revenue"
Cont = Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Profit", "Cost")]

#iii)Check relationships between categorical variables and the target "Revenue"
#Use anova test
#Relationship between Date and Revenue-continous
#we cant use shapiro cause obs >5000 hence why we use the anova test
one.way <- aov(Revenue ~ Date, data = Data)
summary(one.way)
#Posthoc
TukeyHSD(one.way)
#Result:p-value < 0.05 but not statistically significant for the Tukey test(see adjusted p-values), therefore Date has no effect on Revenue and will not be selected for the prediction model

#Relationship between Month and Revenue
one.way <- aov(Revenue ~ Month, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result:p-value < 0.05 but not statistically significant for the Tukey test, therefore Month has no effect on Revenue and will not be selected for the prediction model

#Relationship between Age_Group and Revenue
one.way <- aov(Revenue ~ Age_Group, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result:p-value < 0.05 at 95% confidence, therefore Age_Group has an effect on Revenue and will be selected for the prediction model

#Relationship between Customer_Gender and Revenue
one.way <- aov(Revenue ~ Customer_Gender, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Customer_Gender is also selected for the prediction

#Relationship between Country and Revenue
one.way <- aov(Revenue ~ Country, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value > 0.05, Country is not selected for the prediction

#Relationship between State and Revenue
one.way <- aov(Revenue ~ State, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value > 0.05, State is not selected for the prediction

#Relationship between Product_Category and Revenue
one.way <- aov(Revenue ~ Product_Category, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Product_Category is also selected for the prediction

#Relationship between Product and Revenue
one.way <- aov(Revenue ~ Product, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Product is also selected for the prediction

#Relationship between Sub_Category and Revenue
one.way <- aov(Revenue ~ Sub_Category, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Sub_Category is also selected for the prediction

#List of categorical variables to be used in prediction of Revenue
#Age_Group, Customer_Gender, Product_Category, Product and Sub_Category


#iv)Relationships between continuous variables and the target "Revenue"
#Relationship between Day and Revenue
cor.test(Data$Revenue, Data$Day, 
         method="pearson", use="complete.obs")
#Result:p-value >0.05, therefore no correlation. Exclude Day from prediction analysis
######Pearson generates n R^2 value
#r = 0-0.3 (negative)-low correlation
#r = 0.3-0.69 (negative)-medium/moderate correlation
#r = 0.7(negative)-High correlation
#negative(one increases while another decreases-inverse relationship)
#Relationship between Year and Revenue
cor.test(Data$Revenue, Data$Year, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=-0.013. Very low negative correlation. Year is not included

#Relationship between Customer_Age and Revenue
cor.test(Data$Revenue, Data$Customer_Age, 
         method="pearson", use="complete.obs")
##Result: p<0.05, r2=0.034. Very low positive correlation. Customer_Age is not included

#Relationship between Order_Quantity and Revenue
cor.test(Data$Revenue, Data$Order_Quantity, 
         method="pearson", use="complete.obs")
##Result: p<0.05, r2=0.284. Low but significant correlation. Order_Quantity is included

#Relationship between Unit_Cost and Revenue
cor.test(Data$Revenue, Data$Unit_Cost, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.468. Moderate but significant correlation. Unit_Cost is included

#Relationship between Unit_Price and Revenue
cor.test(Data$Revenue, Data$Unit_Price, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.6042. Moderate but significant correlation. Unit_Price is included

#Relationship between Profit and Revenue
cor.test(Data$Revenue, Data$Profit, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.9726. Very high and significant correlation. Profit is included

#Relationship between Cost and Revenue
cor.test(Data$Revenue, Data$Cost, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.9353. Very high and significant correlation. Cost is included

#Alternatively generate a correlation plot and correlation matrix
#First subset continuous variables including Revenue
Cont2 = Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Profit", "Cost", "Revenue")]
library(Hmisc)
rcorr(as.matrix(Cont2))
library(corrplot)
corrplot(cor(Cont2))
library(ggcorrplot)
ggcorrplot(cor(Cont2))
#Correlation matrix
cor(Cont2)

#Continuous variables selected.
#If we place our cut-off correlation at +/-0.3, we select the following variables
#Unit_Cost, Unit_Price, Profit and Cost

#v)Subset the variables to be used in the prediction combining categorical and continuous
Data_final = Data[c("Unit_Cost", "Unit_Price", "Profit", "Cost", "Age_Group", "Customer_Gender", "Sub_Category", "Product", "Product_Category", "Revenue")]

##B. Split data in to training and testing dataset
#Data can be split as 70-80% training data, and 20-30% testing data
#But remember our target variable is Revenue which is in column 10
library(caTools)
# set seed to ensure you always have same random numbers generated
set.seed(123)
# splits the data in the ratio mentioned in SplitRatio (70%. 
sample = sample.split(Data_final,SplitRatio = 0.7)
# create a training set 
train_set =subset(Data_final,sample ==TRUE)
# create a testing set
test_set=subset(Data_final, sample==FALSE)

##C. Generate a simple linear regression model
#i)Plot relationship between the predictor variable and the target (Revenue)
plot(train_set$Unit_Cost,train_set$Revenue)
#Result: This generates the graph with the equation y = mX + c


#ii)Fit a linear model
# fit a linear model using the training data
linear_model <- lm(Revenue~Unit_Cost, data=train_set)
# see the output of the model
summary(linear_model)
#Result: p-value is significant
#Intercept (y) = 89.907 i.e. when unit cost is 0, Revenue is 89.907 and increases positively by 9.416.
#R^2 = 0.2187 which is low indicating model shows low linear relationship between Unit_Cost and Revenue

##iii)Fit the regression model on a plot
ggplot(data=train_set,aes(x=Unit_Cost,y=Revenue)) + 
  geom_point() + 
  stat_smooth(method = lm)

####2. MULTIPLE LINEAR REGRESSION ####
#Using the bike_sales dataset
# We want to predict the "Revenue" which is a continuous variable
#Importing the already transformed dataset (See previous lessons on data cleaning and EDA)
Data = read.csv("Bike_Sales_New.csv")
#The dataframe has 69407 observations and 19 variables
#Since it has 18 input variables, we use the multiple linear regression

##A. VARIABLE/FEATURE SELECTION ##
#Since regression assumes a linear relationship between the input and target variable, we have to select variables that are most linear in relationship
#So subset data and check relationships to the target variable "Revenue"

#i)Turn categorical variables in to factors
Data$Date = as.factor(Data$Date)
Data$Month = as.factor(Data$Month)
Data$Age_Group = as.factor(Data$Age_Group)
Data$Customer_Gender = as.factor(Data$Customer_Gender)
Data$Country = as.factor(Data$Country)
Data$State = as.factor(Data$State)
Data$Sub_Category = as.factor(Data$Sub_Category)
Data$Product_Category = as.factor(Data$Product_Category)
Data$Product = as.factor(Data$Product)

#ii)Subset data to categorical and continuous
#Remove unnecessary columns if any (X in my dataset)
Data = subset(Data, select = -(X) )
#Subset categorical variables
Cat = Data[c("Date", "Month", "Age_Group", "Customer_Gender", "Sub_Category", "Product", "Product_Category", "Country", "State")]
#Subset continuous variables without the target variable "Revenue"
Cont = Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Profit", "Cost")]

#iii)Check relationships between categorical variables and the target "Revenue"
#Use anova test
#Relationship between Date and Revenue
one.way <- aov(Revenue ~ Date, data = Data)
summary(one.way)
#Posthoc
TukeyHSD(one.way)
#Result:p-value < 0.05 but not statistically significant for the Tukey test(see adjusted p-values), therefore Date has no effect on Revenue and will not be selected for the prediction model

#Relationship between Month and Revenue
one.way <- aov(Revenue ~ Month, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result:p-value < 0.05 but not statistically significant for the Tukey test, therefore Month has no effect on Revenue and will not be selected for the prediction model

#Relationship between Age_Group and Revenue
one.way <- aov(Revenue ~ Age_Group, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result:p-value < 0.05 at 95% confidence, therefore Age_Group has an effect on Revenue and will be selected for the prediction model

#Relationship between Customer_Gender and Revenue
one.way <- aov(Revenue ~ Customer_Gender, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Customer_Gender is also selected for the prediction

#Relationship between Country and Revenue
one.way <- aov(Revenue ~ Country, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value > 0.05, Country is not selected for the prediction

#Relationship between State and Revenue
one.way <- aov(Revenue ~ State, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value > 0.05, State is not selected for the prediction

#Relationship between Product_Category and Revenue
one.way <- aov(Revenue ~ Product_Category, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Product_Category is also selected for the prediction

#Relationship between Product and Revenue
one.way <- aov(Revenue ~ Product, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Product is also selected for the prediction

#Relationship between Sub_Category and Revenue
one.way <- aov(Revenue ~ Sub_Category, data = Data)
summary(one.way)
TukeyHSD(one.way)
#Result: p-value < 0.05, Sub_Category is also selected for the prediction

#List of categorical variables to be used in prediction of Revenue
#Age_Group, Customer_Gender, Product_Category, Product and Sub_Category



#iv)Relationships between continuous variables and the target "Revenue"
#Relationship between Day and Revenue
cor.test(Data$Revenue, Data$Day, 
         method="pearson", use="complete.obs")
#Result:p-value >0.05, therefore no correlation. Exclude Day from prediction analysis

#Relationship between Year and Revenue
cor.test(Data$Revenue, Data$Year, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=-0.013. Very low negative correlation. Year is not included

#Relationship between Customer_Age and Revenue
cor.test(Data$Revenue, Data$Customer_Age, 
         method="pearson", use="complete.obs")
##Result: p<0.05, r2=0.034. Very low positive correlation. Customer_Age is not included

#Relationship between Order_Quantity and Revenue
cor.test(Data$Revenue, Data$Order_Quantity, 
         method="pearson", use="complete.obs")
##Result: p<0.05, r2=0.284. Low but significant correlation. Order_Quantity is included

#Relationship between Unit_Cost and Revenue
cor.test(Data$Revenue, Data$Unit_Cost, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.468. Moderate but significant correlation. Unit_Cost is included

#Relationship between Unit_Price and Revenue
cor.test(Data$Revenue, Data$Unit_Price, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.6042. Moderate but significant correlation. Unit_Price is included

#Relationship between Profit and Revenue
cor.test(Data$Revenue, Data$Profit, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.9726. Very high and significant correlation. Profit is included

#Relationship between Cost and Revenue
cor.test(Data$Revenue, Data$Cost, 
         method="pearson", use="complete.obs")
#Result: p<0.05, r2=0.9353. Very high and significant correlation. Cost is included

#Alternatively generate a correlation plot and correlation matrix
#First subset continuous variables including Revenue
Cont2 = Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", "Unit_Price", "Profit", "Cost", "Revenue")]
library(Hmisc)
rcorr(as.matrix(Cont2))
library(corrplot)
corrplot(cor(Cont2))
library(ggcorrplot)
ggcorrplot(cor(Cont2))
#Correlation matrix
cor(Cont2)

#Continuous variables selected.
#If we place our cut-off correlation at +/-0.3, we select the following variables
#Unit_Cost, Unit_Price, Profit and Cost




##B. Split data in to training and testing dataset
#Data can be split as 70-80% training data, and 20-30% testing data
#But remember our target variable is Revenue which is in column 10
library(caTools)
# set seed to ensure you always have same random numbers generated
set.seed(123)
# splits the data in the ratio mentioned in SplitRatio (70%. 
sample = sample.split(Data_final,SplitRatio = 0.7)
# create a training set 
train_set =subset(Data_final,sample ==TRUE)
# create a testing set
test_set=subset(Data_final, sample==FALSE)

##C. Generate a multilinear regression model
multi_model <- lm(Revenue ~ ., data=train_set)
summary(multi_model)
#Result: Interecept shows the Revenue when each of the predictor variables is zero
#For instance, Revenue at Unit_Cost 0, is -3.8e-12 (p>0.05), but 1.00 at Profit of 0 (p<0.05).
#The R^2 value explains the accuracy of the model (ranges from 0-1 or 0 to 100%)
#R^2 = 0: the model explains nothing
#R^2 = 1: the model explains everything
#0 < R^2 < 1: the model explains part of the variability
#Therefore our model is 100% accurate in predicting Revenue


##D. Model Validation
#i) Used to predict the Revenue
prediction_revenue <- predict(multi_model, newdata = test_set)
prediction_revenue
#Save the predicted revenue
write.csv(prediction_revenue, file = "Predicted Revenue.csv")

##ii)Root Mean Square Error and Root Mean Square Error
#This validates the model's prediction
#metric will inform you how wrong your model is on average
mse <- mean((test_set$Revenue - prediction_revenue)^2)
paste("Mean Squared Error",mse)
#Result: mse = 9.17e-23
rmse <- sqrt(mse)
paste("Root Mean Squared Error",rmse)
#Result: rmse = 9.57e-12
#mse and rmse show that on average the model is wrong in predicting the Revenue by those values
#These values are very small and show a strong prediction model

#**iii)Check the importance of each variable in predicting Revenue
library(Boruta)
boruta_output <- Boruta(Revenue ~ ., data = train_set, doTrace = 0)
boruta_output
#Getting average importance of each predictor variable
importances <- attStats(rough_fix_mod)
importances <- importances[importances$decision != "Rejected", c("meanImp", "decision")]
importances[order(-importances$meanImp), ]
#Plotting the result
plot(boruta_output, ces.axis = 0.7, las = 2, xlab = "", main = "Importance of Variables in Predicting Revenue")

#iii) Using the model to predict Revenue for a hypothetical bike_sales data in Uganda in 2020-2021
Uganda = readxl::read_xlsx("Bike_sales_Uganda.xlsx")

#Notice this dataset doesn't have Revenue which we have to predict
#It also has some columns that were not selected during the model generation
#So we remove the useless columns
Uganda_new = Uganda[c("Unit_Cost", "Unit_Price", "Profit", "Cost", "Age_Group", "Customer_Gender", "Sub_Category", "Product", "Product_Category", "Revenue")]
revenue_Uganda <- predict(multi_model, newdata = Uganda_new)
revenue_Uganda
#Save Revenue prediction for Uganda
write.csv(revenue_Uganda, file = "Uganda_Revenue.csv")

#iv) Check the accuracy in making prediction for Uganda
mse <- mean((test_set$Revenue - revenue_Uganda)^2)
paste("Mean Squared Error",mse)
#Result: mse = 33940.78
rmse <- sqrt(mse)
paste("Root Mean Squared Error",rmse)
#Result: rmse = 184.23
#The model is wrong in predicting Revenue in Uganda by 184.23
#Therefore the model was not as great on making predictions on the Uganda data
#This can be attributed to the lack of data pre-processing