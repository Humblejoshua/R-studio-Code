######ASSIGNMENT 1: MARKING GUIDE #############
#######TOTAL: 20 MARKS ##########

###QUESTION 1: Check to see if all the continuous variables are normally distributed. (3 MARKS) 
#Setwd
setwd("/Users/keksmacbookair/Desktop/course work/Intro to Data science/data sets")
#Import necessary libraries
library(readxl)
library(tidyverse)
library(Hmisc)
library(dlookr)
library(ggplot2)
library(dgof)

#Import dataset called "Wholesale customers data.xlsx"
Assignment1 = read_excel("Wholesale customers data.xlsx")
#The dataset has 440 observations and 8 variables

#Check the datatypes of the 8 variables
sapply(Assignment1,class)
#All 8 variables are numeric, however;
#the variables "Channel" and "Region" are categorical
#The variables "Fresh" to "Delicassen" are continuous

#a) Checking distribution of continuous variables (1.5 MARKS)
#First subset the continuous variables
Continuous_variables = select(Assignment1, Fresh:Delicassen)
#Check distribution using describe function to find skewdness
descriptive_continuous = describe(Continuous_variables)
descriptive_continuous
#Result: All of the continuous variables are not normally distributed (skewness > 0.5)
#All the continuous variables are positively skewed.

##Alternatively using histograms and Shapiro-Wilk's tests
#E.g. Histogram of variable "Fresh"
ggplot(data = Assignment1, aes(x = Fresh)) +
  geom_histogram() +
  labs(x="Fresh", 
       title="Distribution of Fresh Goods in a Wholesale")

#Or Normality can be plotted using Q-Q plots
plot_normality(Assignment1, Fresh)

#Shapiro-Wilk's test
shapiro.test(Assignment1$Fresh)
#Result: W=0.78144, p=<2.2e-16. The p-value is <0.05, therefore Fresh is not normally distributed.


#B)Checking distribution of cataegorical variables. (1.5 MARKS)
#First subset the categorical variables
Categorical_variables = select(Assignment1, Channel, Region)

#View distribution using a boxplot
ggplot(data = Assignment1, mapping = aes(x = Channel)) +
  geom_boxplot() +
  labs(x="Channel", 
       title="Distribution of Wholesale Channels")
#OR
boxplot(Assignment1$Channel)
#OR a barplot
ggplot(data = Assignment1, mapping = aes(x = Channel)) +
  geom_bar() +
  labs(x="Channel", 
       title="Distribution of Wholesale Channels")

#Barplot of Region
ggplot(data = Assignment1, mapping = aes(x = Region)) +
  geom_bar() +
  labs(x="Region", 
       title="Distribution of Wholesale Regions")
#Result: There are 2 Channels, Channel 1 has the higher counts
#There are 3 Regions, Region 3 has the highest Wholesale count, and 2 the lowest.


###QUESTION 2: Transform the dataset to exclude any missing information (1 MARK)
#Check for missing data
sum(is.na(Assignment1))
#Result: There's no missing data


###QUESTION 3: Show the outliers in only the continuous variable in the dataset (3 MARKS)
#A) Outliers in the variable "Fresh" (0.5 MARKS)
#IQR=13806.00
#Q1=3127.75
#Q3=16933.75
Tmin_fresh = 3127.75-(1.5*13806.00) 
Tmax_fresh = 16933.75+(1.5*13806.00) 
Outliers_fresh = Assignment1$Fresh[which(Assignment1$Fresh < Tmin_fresh | Assignment1$Fresh > Tmax_fresh)]
#View outliers in Fresh
Outliers_fresh
#Result: Fresh has 20 outliers 

#B) Outliers in the variable "Milk" (0.5 MARKS)
#IQR=5657.25
#Q1=1533.00
#Q3=7190.25
Tmin_milk = 1533.00-(1.5*5657.25) 
Tmax_milk = 1533.00+(1.5*5657.25) 
Outliers_milk = Assignment1$Milk[which(Assignment1$Milk < Tmin_milk | Assignment1$Milk > Tmax_milk)]
#View outliers in Fresh
Outliers_milk
#Result: Milk has 66 outliers
#Marks will be awarded for the other 4 variables



##QUESTION 4:Transform the dataset and handle the outliers in the continuous variables if any. (3 MARKS)
# create a detect outlier function
detect_outlier <- function(x) {
  
  # calculate first quantile
  Quantile1 <- quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3 - Quantile1
  
  # return true or false for outliers
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5)
}

# create a remove outlier function
remove_outlier <- function(dataframe, columns = names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

Assignment1_new = remove_outlier(Assignment1, c('Fresh', 'Milk', 'Grocery', 'Frozen', 'Detergents_Paper', 'Delicassen'))
#Result: After removal of outliers, the dataset has 318 observations and 8 variables



##QUESTION 5: Save the newly transformed dataset as a csv file called “Your_Last_Name.csv (e.g. Kayla.csv) (1 MARK)
write_csv(Assignment1_new, "Kyalo.csv")


##QUESTION 6:Import the above saved csv file as an R data frame called “Assignment1_Your_Last_Name (e.g. Assignment1_Kyalo) (1 MARK)
Assignment1_Kyalo=read_csv("Kyalo.csv")
#Result: The dataframe has 318 observations and 8 variables


##QUESTION 7:Which of the continuous variables generates the highest annual spending? (2 MARKS)
#Subset the continuous variables
Cont_variables=select(Assignment1_Kyalo, Fresh:Delicassen)
#Calculate the total amounts for each variable
spending_fresh=sum(Cont_variables$Fresh)
spending_milk=sum(Cont_variables$Milk)
spending_grocery=sum(Cont_variables$Grocery)
spending_frozen=sum(Cont_variables$Frozen)
spending_deter=sum(Cont_variables$Detergents_Paper)
spending_delicassen=sum(Cont_variables$Delicassen)
#Result: Fresh goods generate the highest expenditure of 3,090,377



##QUESTION 8:Which retail channel do customers use the most? (2 MARKS)
ggplot(data = Assignment1_Kyalo, mapping = aes(x = Channel)) +
  geom_bar() +
  labs(x="Channel", 
       title="Distribution of Wholesale Channels")
#Customers use Channel 1 the most

#Alternatively, filter and count
count(filter(Assignment1_Kyalo, Channel == 1))
count(filter(Assignment1_Kyalo, Channel == 2))
#Result: Channel 1 = 232 customers, Channel 2 = 86 customers


##QUESTION 9:Which Region has the lowest purchasing power? (2 MARKS)
total_1 = sum(filter(Assignment1_Kyalo, Region == 1))
total_2 = sum(filter(Assignment1_Kyalo, Region == 2))
total_3 = sum(filter(Assignment1_Kyalo, Region == 3))
#Result: Region 2 has the lowest purchasing power with 707,236
#Region 3 has the highest purchasing power with 5,685,718



##QUESTION 10:Which Region spends most on Milk? (2 MARKS)
#Use a boxplot
#First convert the categorical variable Region in to a factor
Assignment1_Kyalo$Region = as.factor(Assignment1_Kyalo$Region)

#Use a boxplot
ggplot(data = Assignment1_Kyalo, mapping = aes(x = Region, y = Milk)) +
  geom_boxplot() +
  labs(x="Region", 
       y="Expenditure on Milk",
       title="Expenditure on Milk in Each Region")

#Or a frequency plot
ggplot(data = Assignment1_Kyalo, mapping = aes(x = Milk)) + 
  geom_freqpoly(mapping = aes(colour = Region), binwidth = 500) +
  labs(x="Expenditure on Milk", 
       title="Expenditure on Milk in Each Region")
#Result: Region 3 spends the most on Milk.