# Importing the dataset

airbnb = read.csv("D:/APU/AML CT046-3-M-AML/AML assignment doc/AirBnB listings in major US cities.csv")
View(airbnb)
str(airbnb)

library(ggplot2)
library(caTools)

# dimensions
dim(airbnb) # dimension

#first 6 records
head(airbnb)

summary(airbnb)

# Explore the variables
library(DataExplorer)

library(dplyr)

plot_str(airbnb)


# feature selection******************************#

airbnb1 <- drop_columns(airbnb, c('thumbnail_url','name','longitude','latitude','instant_bookable','host_since', 'host_response_rate',
                                      'host_identity_verified','first_review', 'neighbourhood','city','amenities', 'description', 'cancellation_policy', 
                                      'host_has_profile_pic','property_type','id','zipcode','last_review'))

View(airbnb1)
head(airbnb1)
str(airbnb1)

#missing values********************************************************************#



plot_missing(airbnb1)
colSums(sapply(airbnb1,is.na)) # shows columns with missing values
str(airbnb1)


#1 Imputation of missing values *********************************************
install.packages('mice')
library('mice')
methods(mice)
md.pattern(airbnb1)

imputed_ds_airbnb1 <- mice(airbnb1, m=5,maxit=3, method = 'rf')

Final_imputed_ds <- complete (imputed_ds_airbnb1)
View(Final_imputed_ds)
plot(imputed_ds_airbnb1)   # check repeated numerous iteration
densityplot(imputed_ds_airbnb1)

airbnb1<-Final_imputed_ds
sum(is.na(airbnb1))
plot_missing(airbnb1) # check missing values 


boxplot(airbnb1$ bedrooms, xlab= ' bedrooms')

# convert airbnb1 column svi_rank to factor to impute in categorical***************

airbnb1$cleaning_fee <- as.factor(airbnb1$cleaning_fee)
str(airbnb1$cleaning_fee)
unique(airbnb1$cleaning_fee)


airbnb1$cleaning_fee = factor(airbnb1$cleaning_fee,
                           levels = c('True', 'False'),
                           labels = c(1, 0))

airbnb1$bed_type <- as.factor(airbnb1$bed_type)
str(airbnb1$bed_type)
unique(airbnb1$bed_type)

airbnb1$bed_type = factor(airbnb1$bed_type,
                              levels = c('Real Bed','Futon', 'Pull-out Sofa', 'Couch', 'Airbed'),
                              labels = c(1, 2, 3, 4, 5))

airbnb1$room_type <- as.factor(airbnb1$room_type)
str(airbnb1$room_type)
unique(airbnb1$room_type)


airbnb1$room_type = factor(airbnb1$room_type,
                              levels = c('Entire home/apt', 'Private room', 'Shared room'),
                              labels = c(1, 2, 3))


View(airbnb1)
str(airbnb1)


# Histogram Plot
plot_histogram(airbnb1) 

# Density Plot 
plot_density(airbnb1)  

#Box Plot 
plot_boxplot(airbnb1, 'log_price')


#Correlation among st independent variables
plot_correlation(airbnb1)







#Linear Regression~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Split the data set into the Training set and Test set~~~~~~~

library(data.table)
install.packages('caTools')
library('caTools')
set.seed(123)

# data spliting
split = sample.split(airbnb1$log_price, SplitRatio = 0.7)
training_set = subset(airbnb1, split == TRUE)
test_set = subset(airbnb1, split == FALSE)

# check dimensions for test and train set

dim(training_set)
dim(test_set)

head(training_set)
head(test_set)

# fitting model 

regressor_1 = lm(formula = log_price ~ ., data = training_set)
#summary of the model parameters
summary(regressor_1)

install.packages('jtools')
library(jtools)
summ(regressor_1, confint = TRUE)

#Predicting the Test set results~~~~~~~~~~~~~~~~~~~~

y_pred = predict(regressor_1, newdata = test_set)
y_pred
table(y_pred, test_set$log_price) 

#RMSE on TEST set~~~~~~~~~~
sqrt(mean((test_set$log_price-y_pred)^2))
library(caret)
summary(y_pred)


# Root mean squared error
RMSE(y_pred, test_set$log_price) 
# Mean Absolute Error
MAE(y_pred, test_set$log_price)



#``````````SVM MODEL
head(training_set)
head(test_set)

library(ggplot2)
library(e1071)


svm_model <- svm(log_price~., data = training_set)
summary(svm_model)

pred = predict (svm_model, test_set)
pred
table(pred, test_set$log_price)

library(caret)
summary(pred)
# Root mean squared error
RMSE(pred, test_set$log_price) 
# Mean Absolute Error
MAE(pred, test_set$log_price) 

#  SVM model using the Linear model

svm_model_2 = svm (log_price~., data = test_set, kernel = "linear")
summary (svm_model_2)

pred2 = predict (svm_model_2, test_set)
pred2
table(pred2, test_set$log_price)

library(caret)
summary(pred2)
RMSE(pred2, test_set$log_price) # Root mean squared error
MAE(pred2, test_set$log_price) # Mean Absolute Error


