library(caret)
library(caTools)
library(MASS)
library(car)
library(GGally)
library(ROCR)
library(dplyr)
library(e1071)
library(plotrix)
library(ggplot2)
library(gridExtra)

#The in_time and out_time files need to be merged to get employee punch time and hence the average time

getTimeSheetData <- function(employee_time){
  
  in_time<- read.csv("in_time.csv", stringsAsFactors = F)
  in_time_stack <- stack(in_time[,-1])
  in_time_stack <- cbind(in_time_stack, in_time[,1])
  names(in_time_stack)<-c("In_Time", "Date", "EmployeeID")
  
  out_time<- read.csv("out_time.csv", stringsAsFactors = F)
  out_time_stack <- stack(out_time[,-1])
  out_time_stack <- cbind(out_time_stack, out_time[,1])
  names(out_time_stack)<-c("Out_Time", "Date", "EmployeeID")
  
  setdiff(in_time_stack$EmployeeID,out_time_stack$EmployeeID)
  setdiff(in_time_stack$Date,out_time_stack$Date)
  
  timeCalc<- merge(in_time_stack,out_time_stack, by=c("EmployeeID", "Date"), all = F)
  
  timeCalc$In_Time <- as.POSIXct(gsub("-", "/", timeCalc$In_Time), format = "%Y/%m/%d %H:%M:%S")
  timeCalc$Out_Time <- as.POSIXct(gsub("-", "/", timeCalc$Out_Time), format = "%Y/%m/%d %H:%M:%S")
  
  timeCalc$workingHours <- timeCalc$Out_Time - timeCalc$In_Time
  
  avgWorkHours <- aggregate(as.numeric(workingHours)~EmployeeID, timeCalc, mean)
  names(avgWorkHours)<-c("EmployeeID", "AvgWorkingHours")
  
  employee_time <- merge(employee_time, avgWorkHours, by = "EmployeeID")
  
  
  return(employee_time)
}
#---------------------------------------------------------------------------------------------------------------#
#DATA PREPARATION

# Read all the data files 
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = F)

str(general_data)
str(employee_survey)
str(manager_survey)


# Let us check if all records are unique 
length(unique(general_data$EmployeeID))  #4410 entries
length(unique(employee_survey$EmployeeID))  #4410 entries
length(unique(manager_survey$EmployeeID))  #4410 entries

setdiff(general_data$EmployeeID,employee_survey$EmployeeID) # Identical employeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey$EmployeeID) # Identical employeeID across these datasets
setdiff(employee_survey$EmployeeID,manager_survey$EmployeeID) # Identical employeeID across these datasets

# let us form a merged master data frame

master_data1 <- merge(general_data,employee_survey, by="EmployeeID", all = F)
master_data <- merge(master_data1,manager_survey, by="EmployeeID", all = F)


# Observe and verify the data

str(master_data) 
# 4410 observation with 29 variables


# Add average working time 
master_data <- getTimeSheetData(master_data)


# Get rid of all columns having same value
master_data <- master_data[vapply(master_data, function(x) length(unique(x)) > 1, logical(1L))]


# Verify if average working hours per employee has been correctly computed 
View(master_data)


# After refering to Data Dictionary and EDA, we can say that:
# The dependent variable is: Attrition

# The nominal variables are: 
# EmployeeID, Age, DistanceFromHome, JobLevel, MonthlyIncome, NumberOfCompaniesWorked, PercentSalaryHike,
# StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion,
# YearsWithCurrentManager, 

# The categorical variables are:
# BusinessTravel, Department, Education, EducationField, Gender, JobRole, MaritalStatus, EnvironmentSatisfaction,
# JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating


# Finding the missing value & imputation
sum(is.na(master_data))# 111 na are there
sapply(master_data, function(x) sum(is.na(x)))
master_data <- master_data[!is.na(master_data$EnvironmentSatisfaction),]
master_data <- master_data[!is.na(master_data$JobSatisfaction),]
master_data <- master_data[!is.na(master_data$WorkLifeBalance),]


#replace N/A value to 0 for NumCompaniesWorked & TotalWorkingYears

master_data$NumCompaniesWorked[is.na(master_data$NumCompaniesWorked)==T] <-0
master_data$TotalWorkingYears[is.na(master_data$TotalWorkingYears)==T] <-0
sum(is.na(master_data))
# no NA values avaialable


#Outlier treatment for all numeric vars
boxplot(master_data$Age)
quantile(master_data$Age,seq(0,1,0.01))
#no outliers present

boxplot(master_data$DistanceFromHome)
quantile(master_data$DistanceFromHome,seq(0,1,0.01))
#no outliers present

boxplot(master_data$JobLevel)
quantile(master_data$JobLevel,seq(0,1,0.01))
#no outliers present

boxplot(master_data$MonthlyIncome) 
#outliers exist
quantile(master_data$MonthlyIncome,seq(0,1,0.01))
summary(master_data$MonthlyIncome)

box <- boxplot.stats(master_data$MonthlyIncome, coef = 2)
out <- box$out
General <- master_data[ !master_data$MonthlyIncome %in% out, ]
master_data <- General
boxplot(master_data$MonthlyIncome) # ~ 100 outliers removed
summary(master_data$MonthlyIncome)

boxplot(master_data$NumCompaniesWorked) 
quantile(master_data$NumCompaniesWorked,seq(0,1,0.01))
#No outliers exist

boxplot(master_data$PercentSalaryHike)
quantile(master_data$PercentSalaryHike,seq(0,1,0.01))
#no outliers exist

boxplot(master_data$StockOptionLevel)
quantile(master_data$StockOptionLevel,seq(0,1,0.01))
#no outliers exist

boxplot(master_data$TotalWorkingYears)
quantile(master_data$TotalWorkingYears,seq(0,1,0.01))
# outliers exist
summary(master_data$TotalWorkingYears)

box <- boxplot.stats(master_data$TotalWorkingYears, coef = 2)
out <- box$out
General <- master_data[ !master_data$TotalWorkingYears %in% out, ]
master_data <- General
boxplot(master_data$TotalWorkingYears) # ~ 100 outliers removed
summary(master_data$TotalWorkingYears)

boxplot(master_data$TrainingTimesLastYear)
quantile(master_data$TrainingTimesLastYear,seq(0,1,0.01))
#very few outliers exist

boxplot(master_data$YearsAtCompany)
quantile(master_data$YearsAtCompany,seq(0,1,0.01))
#outliers exist
summary(master_data$YearsAtCompany)

box <- boxplot.stats(master_data$YearsAtCompany, coef = 2)
out <- box$out
General <- master_data[ !master_data$YearsAtCompany %in% out, ]
master_data <- General
boxplot(master_data$YearsAtCompany)
summary(master_data$YearsAtCompany)

boxplot(master_data$YearsSinceLastPromotion)
quantile(master_data$YearsSinceLastPromotion,seq(0,1,0.01)) 
#outliers exist
summary(master_data$YearsSinceLastPromotion)

box <- boxplot.stats(master_data$YearsSinceLastPromotion, coef = 2.5)
out <- box$out
General <- master_data[ !master_data$YearsSinceLastPromotion %in% out, ]
master_data <- General
boxplot(master_data$YearsSinceLastPromotion) 
summary(master_data$YearsSinceLastPromotion)

boxplot(master_data$YearsWithCurrManager)
quantile(master_data$YearsWithCurrManager,seq(0,1,0.01)) 
#No outliers exist

boxplot(master_data$AvgWorkingHours)
quantile(master_data$AvgWorkingHours,seq(0,1,0.01)) 
#No outliers exist
#--------------------------------------------------------------------------------------------------------------#

# EDA: UNIVARIATE AND BIVARIATE ANALYSIS
ggplot(master_data, 
       aes(x = master_data$Age, y = master_data$Education, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
#It appears many employees who quit are having education below Master's.


ggplot(master_data, 
       aes(x = master_data$TotalWorkingYears, y = master_data$JobRole, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
#it appears many employees with less than 10 years experience are likely to quit

ggplot(master_data, 
       aes(x = master_data$AvgWorkingHours, y = master_data$Gender, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
#higher the working hours, higher the attrition

ggplot(master_data, 
       aes(x = master_data$MonthlyIncome, y = master_data$Gender, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
#employees with Lower monthly income quit, may be to find a better job opportunity

ggplot(master_data, 
       aes(x = master_data$EnvironmentSatisfaction, y = master_data$JobSatisfaction, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
#low job satisfaction and environment satisfaction is leading to high attrition

ggplot(master_data, 
       aes(x = master_data$Age, y = master_data$Gender, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
#Attrition seems to subside over time. More Aged an employee, lesser is the Attrition

#Bivariate to check if any correlation between BusinessTravel, Department with Attrition
ggplot(master_data, 
       aes(x = master_data$Department, y = master_data$BusinessTravel, color = as.factor(master_data$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()


#Bivariate to check if there is any correlation between Marital Status, Gender & Attrition
ggplot(master_data, 
       aes(x = master_data$MaritalStatus, y = master_data$Gender, 
           color = as.factor(master_data$Attrition))) + geom_point(alpha = 0.2) + geom_jitter()

#Univariate to check if there is any correlation between Gender & Attrition
p1 <-ggplot(master_data[master_data$Gender == "Female",], aes(x=MaritalStatus, y = (..count..)/sum(..count..), 
                                                      fill=factor(Attrition))) + 
  geom_bar(position="fill") + scale_y_continuous(labels = "percent") +
  xlab("Female Attrition") +
  ylab("Percentage")+ 
  ggtitle("Status of Female Attrition on the Basis of the Marital Status") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)

p2 <-ggplot(master_data[master_data$Gender != "Female",], aes(x=MaritalStatus, y = (..count..)/sum(..count..), 
                                                      fill=factor(Attrition))) + 
  geom_bar(position="fill") + scale_y_continuous(labels = "percent") +
  xlab("Male Attrition") + 
  ylab("Percentage")+ 
  ggtitle("Status of Male Attrition on the Basis of the Marital Status") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 

grid.arrange(p1, p2)

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(master_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(master_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(master_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,align = "h")

plot_grid(ggplot(master_data, aes(x=JobRole,fill=Attrition))+ geom_bar(), 
          ggplot(master_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          ggplot(master_data, aes(x=as.factor(NumCompaniesWorked),fill=Attrition))+ geom_bar(), 
          ggplot(master_data, aes(x=as.factor(Education),fill=Attrition))+ geom_bar(),align = "h")


#-----------------------------------------------------------------------------------------------------------------#
#DATA SCALING

#Standardizing all continuous variables 

master_data$Age<- scale(master_data$Age) 
master_data$DistanceFromHome<-scale(master_data$DistanceFromHome)
master_data$JobLevel<-scale(master_data$JobLevel)
master_data$MonthlyIncome<-scale(master_data$MonthlyIncome)
master_data$NumCompaniesWorked<-scale(master_data$NumCompaniesWorked)
master_data$PercentSalaryHike<-scale(master_data$PercentSalaryHike)
master_data$StockOptionLevel<-scale(master_data$StockOptionLevel)
master_data$TotalWorkingYears<-scale(master_data$TotalWorkingYears)
master_data$TrainingTimesLastYear<-scale(master_data$TrainingTimesLastYear)
master_data$YearsAtCompany<-scale(master_data$YearsAtCompany)
master_data$YearsSinceLastPromotion<-scale(master_data$YearsSinceLastPromotion)
master_data$YearsWithCurrManager<-scale(master_data$YearsWithCurrManager)
master_data$AvgWorkingHours<-scale(master_data$AvgWorkingHours)
#----------------------------------------------------------------------------------------------------------------#
#CREATING DUMMY VARIABLES

#Attrition Rate

AttritionRate <- sum(master_data$Attrition)/nrow(master_data) 
#16.82% Attrition-Rate

# converting target variable Attrition from No/Yes character to levels 0/1 
master_data$Attrition<- ifelse(master_data$Attrition=="Yes",1,0)

str(master_data)

#creating a dataframe of categorical variables
General_Cat<- master_data[,c(4, 5, 7, 8, 9, 11, 12, 22, 23, 24, 25, 26)]

# converting categorical variables to factor
General_fact<- data.frame(sapply(General_Cat, function(x) factor(x)))
str(General_fact)

# creating dummy variables for all factor attributes
dummies<- data.frame(sapply(General_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =General_fact))[,-1]))


# Create the final master file based on dummy value column join
General_Final<-cbind(master_data[,-c(4, 5, 7, 8, 9, 11, 12, 22, 23, 24, 25, 26)],dummies)


# Verify the number of observations and variables
str(General_Final)
#all are in int/num. 3847 obs of 52 variables

#--------------------------------------------------------------------------------------------------------------------#
# To convert the data in Test and Train
# 70% - train data
# 30% - test data

set.seed(100)
indices = sample.split(General_Final$Attrition, SplitRatio = 0.7)
train_data = General_Final[indices,]
test_data = General_Final[!(indices),]

#------------------------------------------------------------------------------------------------------------------#
model_1 <- glm(Attrition~., data = train_data, family = "binomial")
summary(model_1)
#AIC = 1870

step_model <- stepAIC(model_1, direction = "both")
summary(step_model)
#AIC = 1839.1

#consider all variables with "-"  sign from the last stepAIC model and build the next model
model_2 <- glm(Attrition~ MaritalStatus.xMarried +  EducationField.xMedical + EducationField.xMarketing +
                 EducationField.xOther + JobRole.xResearch.Director + JobRole.xManager + JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")

summary(model_2)
vif(model_2)

#exclude MaritalStatus.xMarried as its p-value is high
model_3 <- glm(Attrition~  EducationField.xMedical + EducationField.xMarketing +
                 EducationField.xOther + JobRole.xResearch.Director + JobRole.xManager + JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
summary(model_3)
#AIC = 1839.6
vif(model_3)

#remove EducationField.xMarketing as its p-value is high
model_4 <- glm(Attrition~  EducationField.xMedical  +
                 EducationField.xOther + JobRole.xResearch.Director + JobRole.xManager + JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
summary(model_4)
#AIC = 1842
vif(model_4)

#remove EducationField.xOther as its p-value is high
model_5 <- glm(Attrition~  EducationField.xMedical  +
                  JobRole.xResearch.Director + JobRole.xManager + JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
summary(model_5)
#AIC = 1844.2
vif(model_5)

#remove EducationField.xMedical as its p-value is high
model_6 <- glm(Attrition~  
                 JobRole.xResearch.Director + JobRole.xManager + JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
summary(model_6)
#AIC = 1842.9
vif(model_6)

#remove JobRole.xResearch.Director as its p-value is high
model_7 <- glm(Attrition~  
                 JobRole.xManager + JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
summary(model_7)
#AIC = 1846.1
vif(model_7)

#remove JobRole.xManager as its p-value is high
model_8 <- glm(Attrition~  
                 JobRole.xHuman.Resources +
                 BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
summary(model_8)
#AIC = 1853.8
vif(model_8)

#remove JobRole.xHuman.Resources as its p-value is high
model_9 <- glm(Attrition~ BusinessTravel.xTravel_Rarely + JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")
                 
summary(model_9)
#AIC = 1859.7
vif(model_9)

#remove BusinessTravel.xTravel_Rarely as its p-value and vif are both high
model_10 <- glm(Attrition~  JobInvolvement.x3 + JobSatisfaction.x3 + WorkLifeBalance.x4 + 
                 JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                 JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                 Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                 NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                 BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")


summary(model_10)
#AIC = 1866.7
vif(model_10)

#remove JobSatisfaction.x3 as its p-value is comparitively high
model_11 <- glm(Attrition~  JobInvolvement.x3 + WorkLifeBalance.x4 + 
                  JobSatisfaction.x2 + JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")


summary(model_11)
#AIC = 1875.8
vif(model_11)

#remove JobSatisfaction.x2 as its p-value is comparitively high
model_12 <- glm(Attrition~  JobInvolvement.x3 + WorkLifeBalance.x4 + 
                  JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")


summary(model_12)
#AIC = 1877.3
vif(model_12)

#remove JobInvolvement.x3 as its p-value is comparitively high
model_13 <- glm(Attrition~  WorkLifeBalance.x4 + 
                  JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")


summary(model_13)
#AIC = 1883.1
vif(model_13)

#remove WorkLifeBalance.x4 as its p-value and vif are comparitively high
model_14 <- glm(Attrition~ JobRole.xManufacturing.Director +  Age + WorkLifeBalance.x2 + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")

                  

summary(model_14)
#AIC = 1891.6
vif(model_14)

#remove WorkLifeBalance.x2 as its p-value is comparitively high
model_15 <- glm(Attrition~ JobRole.xManufacturing.Director +  Age  + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 +WorkLifeBalance.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")



summary(model_15)
#AIC = 1896.6
vif(model_15)

#remove WorkLifeBalance.x3 as its p-value is comparitively high
model_16 <- glm(Attrition~ JobRole.xManufacturing.Director +  Age  + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2 + Department.xSales + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")



summary(model_16)
#AIC = 1904.5
vif(model_16)

#even though the p-value is low, remove Department.xSales  as its vif is comparitively high
model_17 <- glm(Attrition~ JobRole.xManufacturing.Director+ Age  + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2  + 
                  Department.xResearch...Development + EnvironmentSatisfaction.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")



summary(model_17)
#AIC = 1928.2
vif(model_17)

#remove Department.xResearch...Developmen  as its p-value is comparitively high
model_18 <- glm(Attrition~ JobRole.xManufacturing.Director+ Age  + TrainingTimesLastYear +
                  JobRole.xManufacturing.Director + EnvironmentSatisfaction.x2  + 
                  EnvironmentSatisfaction.x3 + 
                  NumCompaniesWorked + YearsWithCurrManager + TotalWorkingYears + YearsSinceLastPromotion +
                  BusinessTravel.xTravel_Frequently + MaritalStatus.xSingle + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x4 + AvgWorkingHours, data = train_data, family = "binomial")



summary(model_18)
#AIC = 1926.6
vif(model_18)

# We are left with 14 predictor variables now. 
# 1. JobRole.xManufacturing.Director                               
# 2. Age             
# 3. TrainingTimesLastYear 
# 4. EnvironmentSatisfaction.x3
# 5. EnvironmentSatisfaction.x2                         
# 6. NumCompaniesWorked 
# 7. YearsWithCurrManager                 
# 8. TotalWorkingYears           
# 9. YearsSinceLastPromotion 
# 10.MaritalStatus.xSingle  
# 11.BusinessTravel.xTravel_Frequently                            
# 12.JobSatisfaction.x4 
# 13.EnvironmentSatisfaction.x4                   
# 14.AvgWorkingHours 
 

# This seems to be an fairly optimal model as all have low VIF < 2 and low p-value. 
# Let us evaluate the model now.

final_model <- model_18
summary(final_model)

#-------------------------------------------------------------------------------------------------------------------#
#TEST DATA

#predicted probabilities of Attrition for test data

predict_test = predict(final_model, type = "response", 
                    newdata = test_data[,-3])

summary(predict_test)

test_data$prob <- predict_test
View(test_data)

#-------------------------------------------------------------------------------------------------------------------#
#MODEL EVALUATION
           
test_actual_attrition<-factor(ifelse(test_data$Attrition==1,"Yes","No"))


#Probability_Cutoff>=0.5
test_predict_attrition<-factor(ifelse(predict_test >= 0.50, "Yes", "No"))
test_confusion <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_confusion

#Probability_Cutoff>=0.4
test_predict_attrition<-factor(ifelse(predict_test >= 0.40, "Yes", "No"))
test_confusion <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_confusion

#function for finding the Optimal Probability Cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(predict_test >= cutoff, "Yes", "No"))
  confusion <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- confusion$overall[1]
  sens <- confusion$byClass[1]
  spec <- confusion$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(predict_test)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="blue",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


op_p_cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]

#Optimal P_Cutoff
test_predict_attrition<-factor(ifelse(predict_test >= op_p_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
conf_final

#           Accuracy    : 0.7478 
#           Sensitivity : 0.7526          
#           Specificity : 0.7469 

#--------------------------------------------------------------------------------------------------------------#
# Now let us determine the goodness of our model using KS Statistic method
# let us chose cutoff value using the Gain, Lift and KS-Statistic methods
#KS -statistic - Test Data By Sharma Method ######
# Ref: https://datascience.stackexchange.com/questions/19493/what-is-a-good-method-to-generate-the-ks-statistic-in-r

test_predict_attrition <- ifelse(test_predict_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#using it on test data
pred_object_test<- prediction(test_predict_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

plot(performance_measures_test,main=paste0(' KS=',round(max(ks_table_test)*100,1),'%'))
lines(x = c(0,1),y=c(0,1), col = "red")

max(ks_table_test)
#[1] 0.49945 # Its > 40% 

#calculating area under curve
area_under_curve<- performance(pred_object_test,measure="auc")
auc <- area_under_curve@y.values[[1]]
print(auc)
#[1] 0.74973

#------------------------------------------------------------------------------------------------------------------#
