
# Support Vector Machines - Assignment

## loading packeges
library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(caTools)
library(ggplot2)
library(gridExtra)

##data preperation

train <- read.csv("~/Desktop/SVM Dataset/SVM assignment Dataset/mnist_train.csv", header=FALSE)
View(mnist_train)

test <- read.csv("~/Desktop/SVM Dataset/SVM assignment Dataset/mnist_test.csv", header=FALSE)
View(mnist_test)


#Checking for Duplicates
nrow(train[!duplicated(train), ])
nrow(test[!duplicated(test), ])


#Checking the structure, dimentions and first few records of train and test dataset
str(train)
dim(train)  
head(train)
#60000 rows and 785 Columns

str(test)
dim(test)   
head(test)
#10000 rows and 785 columns


#checking for missing values "NA" in train dataset
sapply(train,function(x) sum(which(is.na(x))))
# 0 NA values

#checking for missing values "NA" in test dataset
sapply(test,function(x) sum(which(is.na(x))))
# 0 NA values


#checking for blank values
sapply(train,function(x) length(which(x==" "))) 
# 0 blank values

# checking for blank values
sapply(test,function(x) length(which(x==" "))) 
# 0 blank values


#changing first column names for both train and test as it signifies the class 
colnames(train)[1] <- "class"
colnames(test)[1] <- "class"

#adding a column signifying train & test data in both data frames 
train$type <- "train"
test$type <- "test"

# Lets combine both test and train together for data undestanding #
combinedData <- rbind(train,test)

# Lets see if there are any NAs in the combined Data Set #
which(sapply(combinedData,function(x) sum(is.na(x))) != 0)
# No NAs

# Lets see if some features have same value in them
same_value_columns <- which(sapply(combinedData,function(x) length(unique(x)) == 1))
length(same_value_columns) 
# 65 columns 


# 65 column have same value present in the combined(test & train) dataset.

# If the value in each column is the same then, we can say that axis of the dimension is a constant
# and will not suffice any variance while modeling and  getting valuable information for prediction.
# Hence we safely remove those columns

combinedData2 <- combinedData %>% select(-same_value_columns)

# According to problem statement, each pixel provides the density of black in that pixel.
# Therefore, values range from 0 to 255. Anything beyond this range are considered outliers.
# checking for block with values less than 0 and more than 255.


sapply(combinedData2, function(x) length(which(x <0 & x>255)))
# no outliers

# getting back to train and test data.

train2 <- combinedData2 %>% filter(type == "train") %>% select(-type)
test2 <- combinedData2 %>% filter(type == "test") %>% select(-type)

# Convert the number column to factors for both data set #
train2$class <- as.factor(train2$class)
test2$class <- as.factor(test2$class)

# Since the training data is very large, we sample out some data for the training the model

# Case 1:
# Going by the 80/20 rule for train and test data, 
# test data is 10000 rows, our train set should be 40000 rows. 
# i.e 1/3rd less than the original 60000 rows.

# Case 2:
# We have 10000 rows in our test data, we can do the training on double the data
# thereby making the training data set as 20000 rows, reducing the train set by 66.67%,

# However, we need to check the distribution of data for better understanding.

set.seed(100)

case1_train_sampled <- sample.split(train2$class,SplitRatio = 0.6667)
case1_train <- train2[case1_train_sampled,]
nrow(case1_train) 
# 40003

case2_train_sampled <- sample.split(train2$class,SplitRatio = 0.3333)
case2_train <- train2[case2_train_sampled,]
nrow(case2_train)
# 19997


G01 <- ggplot(case1_train) + 
  geom_bar(aes(x=as.factor(class)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,4700,500)) +
  labs(title="Distribution Case 1\nTrainSet = 40003 rows",x="Numbers",y="Count")

G02 <- ggplot(case2_train) + 
  geom_bar(aes(x=as.factor(class)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,2400,200)) +
  labs(title="Distribution Case 2\nTrainSet = 19997 rows",x="Numbers",y="Count")

grid.arrange(G01,G02)

# Conclusion : Both distributions are uniform, and also within the classified range.
# In order to keep the computation time low, we go with Case 2.

# Note       
# Since the data is uniform all possible types are taken care of during sampling.
# Assuming that each class (2,3,4 etc) can be considered with the sampled data.
# We also assume that we are not missing any surprise patterns of pixels
# or specific classes in test data set.


train_set <- case2_train
test_set <- test2

# Data Preparation completed


## Model Preparation 

## Building a standard LINEAR SVM model and observing its performance

digit_recog <- ksvm(class ~ ., data = train_set, scale=FALSE, kernel= "vanilladot")
# Note: warning due to scaling, can be ignored

digit_recog

# Hyper Parameter : C = 1
# Support Vectors : 4388

# evaluating model accuracy on the training data
  
train_digit_recog <- predict(digit_recog,train_set)
confusionMatrix(train_digit_recog,train_set$class)
# Net Accuracy of Model for train data = 1.0

# evaluating model accuracy on the test data 

test_digit_recog <- predict(digit_recog,test_set)
confusionMatrix(test_digit_recog,test_set$class)
# Net Accuracy of Model for test data = 0.9132

## Building a RBF - KERNEL NON-LINEAR SVM model and observing if its performance 

digit_recog_RBF <- ksvm(class~.,data=train_set,scale=FALSE,kernel="rbfdot")
# Note: warning due to scaling, can be ignored.

digit_recog_RBF

# Hyper Parameter : C = 1  &  sigma = 1.63691367429906e-07
# Support Vectors : 6018
# Training error : 0.016302

# evaluating model accuracy on the train data

eval_train_digit_recog_RBF <- predict(digit_recog_RBF,train_set)
confusionMatrix(eval_train_digit_recog_RBF,train_set$class)
# Train Set Net Accuracy = 0.9837

# evaluating model accuracy on the test data

eval_test_digit_recog_RBF <- predict(digit_recog_RBF,test_set)
confusionMatrix(eval_test_digit_recog_RBF,test_set$class)
# Net Accuracy of model = 0.9673


## Conclusion 
# The RBF model is performing better (with default kernel parameters). 
# Since there is larger gap between train accuracy and test accuracy with linear modeling.
# Therefore, we further our study with the RBF non-linear model through cross validation for better results.

# We now build the cross validation based on;
# The Sigma built by default RBF model, sigma = 1.63334422481776e-07
# As we saw that in the linear model, we got an accuracy of 0.9132 & sigma is of the order 1e-7, 
# Therefore,there is not much non-linearity in the data.
# However, having some non-linearity definitely helps the accuracy.


## Model Evaluation & Cross Validation 

# Cross validation for the RBF Model, with following 
# Cross Validation folds = 3
# Range of sigma = 0.63e-7, 1.63e-7, 2.63e-7
# Range of C = 1 2 3

# Note : For faster computation, we keep limited levels for grids and cross validation
# The cross validation process can take upto a few hours
# In order to get granular analysis, we can consider increasing the levels in sigma & C for fine analysis.


trainControl <- trainControl(method = "cv",number = 3,verboseIter=TRUE)

metric <- "Accuracy"

set.seed(90)

grid <- expand.grid(.sigma = c(0.63e-7,1.63e-7,2.63e-7),.C=c(1,2,3))

# Note : 
# We see warnings that can be ignored (for every iteration of cross validation due to scaling). 


non_linear_RBF.fit <- train(class~.,data=train_set,method="svmRadial",metric=metric,
                            tuneGrid=grid,trControl=trainControl)
                            
non_linear_RBF.fit

#Support Vector Machines with Radial Basis Function Kernel 

#19997 samples
#719 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#No pre-processing
#Resampling: Cross-Validated (3 fold) 
#Summary of sample sizes: 13331, 13332, 13331 
#Resampling results across tuning parameters:

#  sigma     C  Accuracy   Kappa    
#6.30e-08  1  0.9419912  0.9355242
#6.30e-08  2  0.9508425  0.9453620
#6.30e-08  3  0.9543931  0.9493084
#1.63e-07  1  0.9621442  0.9579242
#1.63e-07  2  0.9679951  0.9644267
#1.63e-07  3  0.9698454  0.9664835
#2.63e-07  1  0.9691953  0.9657613
#2.63e-07  2  0.9729459  0.9699299
#2.63e-07  3  0.9743461  0.9714863

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 2.63e-07 and C = 3.


# ploting the data 
plot(non_linear_RBF.fit)


## Building the final model with C = 3 and sigma = 2.63e-07

non_linear_RBF_final <- ksvm(class~., data=train_set,kernel="rbfdot",scale=FALSE,
                             C=3,kpar=list(sigma=2.63e-7))

non_linear_RBF_final


#Support Vector Machine object of class "ksvm" 

#SV type: C-svc  (classification) 
#parameter : cost C = 3 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  2.63e-07 

#Number of Support Vectors : 6287 


# Checking training accuracy
 
eval_train_non_linear_RBF_final <- predict(non_linear_RBF_final,train_set)
confusionMatrix(eval_train_non_linear_RBF_final,train_set$class)
# Net Train Accuracy = 0.9991

# Checking the test accuracy
 
eval_test_non_linear_RBF_final <- predict(non_linear_RBF_final,test_set)
confusionMatrix(eval_test_non_linear_RBF_final,test_set$class)
# Net test Accuracy = 0.9754 


# There is very less difference between train and test accuracy, 
# Therefore there is no over-fitting.
# The model can predict digits correctly using Non-Linear SVM to a large extent.


## Conclusion

# Non-linearity in the data is present but is vary less since the value of the hyper parameter sigma is of 1e-7. 
# However, it is also seen that having this magnitude of non-lineariry helps in increasing the performance accuracy.
# After cross validation through RBF-kernel, we observe maximum accuracy can be seen with C=3 and sigma=2.63e-7.
# Therefore, the train accuracy is very good.
# With these hyper parameters, the test accuracy is comparable with train accuracy, eliminating chances of over fitting. 
# Also the specificity and sensitivity across different levels (1,2,3,etc.) is very good for both train and test.

# Therefore,
# The Final Hyper Parameters :
# C=3 & sigma = 2.63e-7