#### ASSIGNMENT 2: MARKING GUIDE ######
#Set working directory

#Import libraries
library(tidyverse)
library(Hmisc)
library(dlookr)
library(dplyr)
library(ggplot2)
library(dgof)
library(WriteXLS)
library(car)
library(rstatix)
library(ggpubr)
library(AICcmodavg)

###Q1: Explore the dataset and identify the datatypes of each variable (1 MARK)
Assignment2 = readxl::read_xlsx("Loan_Approval_Data.xlsx")
#Dataframe has 614 observations and 12 variables
#View data types
sapply(Assignment2,class)
#OR
str(Assignment2)
#Result:7 Categorical variables (character): Loan_ID, Gender, Married, Dependent, Education, Self_Employed, Loan_Status
#5 Continuous variables: ApplicantIncome, CoapplicantIncome, Loan_Amount_Term, Credit_History


###Q2: Transform the dataset to remove missing data and outliers if any  (3 MARKS)
#a)Check for missing data (1 MARK)
sum(is.na(Assignment2))
colSums(is.na(Assignment2))
#There are 150 missing observations within the following variables;
#Gender: 13 missing
#Married: 2 missing
#Dependents: 15 missing
#Education: 2 missing
#Self Employed: 32 missing
#Loan_Amount: 22 missing
#Loan_Amount_Term: 14 missing
#Credit_History: 50 missing

#b)Handling missing data (1 MARK)
#Either remove missing data
Assignment2_omit=na.omit(Assignment2)
print(Assignment2_omit)
#Result: New dataframe has 478 observations and 12 variables

#OR imputing with median for continuous variables variables
Assignment2_impute_median = Assignment2 %>% 
  mutate_if(is_numeric(), function(x) ifelse(is.na(x), median(x, na.rm = T), x))
#And impute with mode for categorical variables
Assignment2_impute_mode = Assignment2_impute_median %>% 
  mutate_if(is_character(), function(x) ifelse(is.na(x), mode(x, na.rm = T), x))
#Result: Dataframe still has 614 observations and 12 variables.

#b)Handling Outliers (1 MARK)
#Check for outliers using skewness
library(dplyr)
skewness_result = Assignment2_omit %>%
  describe() %>%
  select(described_variables, skewness) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))
#view result
skewness_result
#Result:The following variables are highly skewed
#ApplicantIncome: 6.906832
#CoapplicantIncome:5.873310
#LoanAmount: 2.354199
#Loan_Amount_Term: -2.326609
#Credit_History: -2.030294

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

Assignment2_new = remove_outlier(Assignment2_omit, c('ApplicantIncome', 'CoapplicantIncome', 'LoanAmount', 'Loan_Amount_Term', 'Credit_History'))
#Result: After removal of outliers, the dataset has 299 observations and 12 variables


###Q3: Save the transformed dataset as a new dataset (either csv or excel) (1 MARK)
write_csv(Assignment2_new, "Assignment2_Kyalo.csv")


#NOTE FOR Q4-Q6: FULL MARKS ONLY AWARDED FOR EXPLANATIONS NOT FOR CODES AND RESULTS

###Q4: Show relationships between continuous variables (5 MARKS)
#E.g comparing ApplicantIncome and CoapplicantIncome
#Using a scatterplot
ggplot(data = Assignment2_new, mapping = aes(x = ApplicantIncome, y = CoapplicantIncome)) + 
  geom_point() +
  labs(x="Income of an Applicant", 
       y="Income of a Coapplicant",
       title="Relationship between the incomes of bank loan applicants and their coapplicants")

#Statistical relationship
cor.test(Assignment2_new$ApplicantIncome, Assignment2_new$CoapplicantIncome, 
         method="pearson", use="complete.obs")
#Result: There's an low inverse relationship (negative correlation) between ApplicantIncome and CoapplicantIncome (r=-0.254, p=8.563e-6)
#In otherwords, loan applicants with a high income are to some degree (25%) likely to have a coapplicant with a low income, and vice versa.



###Q5: Show relationships between a categorical variable and a continuous variable (5 MARKS)
#Comparing the LoanAmount and Loan_Status
ggplot(data = Assignment2_new, mapping = aes(x = Loan_Status, y = LoanAmount)) +
  geom_boxplot() +
  labs(x="Loan Status", 
       y="Loan Amount Requested",
       title="A bank's decision making on loan approval based on the amount requested")
one.way <- aov(LoanAmount ~ Loan_Status, data = Assignment2_new)
summary(one.way)
TukeyHSD(one.way)
#Result: The amount of money requested for by a loan applicant has no statistical effect on whether the bank approves thier loan or not (F=0.052, p=0.82)
#Alternatively: There's no relationship between LoanAmount and Loan_Status.




###Q6: Show the relationship between two categorical variables (5 MARKS)
#Comparing Education and Loan_Status
#Visuals (Heatmap)
Assignment2_new %>% 
  count(Loan_Status, Education) %>%  
  ggplot(mapping = aes(x = Loan_Status, y = Education)) +
  geom_tile(mapping = aes(fill = n))  +
  labs(x="Loan Status", 
       y="Education Level",
       title="A bank's decision making on loan approval based on Education level")

ggplot(data = Assignment2_new) +
  geom_count(mapping = aes(x = Loan_Status, y = Education)) +
  labs(x="Loan Status", 
       y="Education Level",
       title="A bank's decision making on loan approval based on Education level")
#Generate chisquare stats
Contigeny_table = table(Assignment2_new$Loan_Status,Assignment2_new$Education) 
#View the table
Contigeny_table
#Then apply the cisquare test
print(chisq.test(Contigeny_table))
#Result: Although the numbers show that Graduate level applicants are granted a loan more than those denied,
#This is not statisically true (X^2=3.1076, p=0.2114) as the two variables are independent of each other.
#Therefore Education has no impact on whether an applicant is granted a loan or not.