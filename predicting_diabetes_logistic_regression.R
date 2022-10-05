# The first step is to read in the data; We shall store it in an object called ds_diabetes (where ds stands for dataset)
# The top row of the dataset has column headings, hence we set header=T
ds_diabetes = read.csv(file = 'DiabetesData.csv', sep=",", header = T)

# Evaluate if that worked by printing the first 6 lines of the data set with the following command
head(ds_diabetes)

# Data types - this is a "data frame", a list-like object with a tabular structure
str(ds_diabetes)

# Some plots using the "$" operator to select specific columns of the data frame
hist(ds_diabetes$pregnant, xlab = "Pregnant", main="Histogram of variable pregnant")
hist(ds_diabetes$glucose, xlab = "Glucose", main="Histogram of variable glucose")

# Note that when using the freq=FALSE argument the histogram is not a percentage, 
# it is  normalized (so the total area is equal to 1).
hist(ds_diabetes$glucose, xlab = "Glucose", main="Histogram of normalized variable glucose", freq=F)

# Scatterplot of two variables
plot(ds_diabetes$glucose,ds_diabetes$pressure, main= "Scatterplot",xlab="Glucose", ylab="Pressure")

# Quick evaluation of the response variable using a frequency table.
ftable(ds_diabetes$diabetes)

# To evaluate statistics of all variables at once, use "summary".
# Note, different quantities evaluated for different types of data.
summary(ds_diabetes)

## Missing values are present! That is a problem we will ignore for now, and simply replace missing values with zero.

# replace NA's with zero (note, creating a new object called ds_diabetes2)
ds_diabetes2 = ds_diabetes

# Replace NA's with zeroes
ds_diabetes2[is.na(ds_diabetes2)] = 0

# Let's see if that worked using the summary function again
summary(ds_diabetes2)
# There is one more issue to resolve.
# The response variable, which is diabetes, is a character string.
# As far as R is concerned this means there are as many possible outomes as there are unique strings.
# However there are only 2 outcomes, namely "POS" and "NEG".
# We convert the Diabetes column to a factor which will resolve this issue
ds_diabetes2$diabetes = as.factor(ds_diabetes2$diabetes)


# Now our data is adequate for our needs, let's move on to the modeling part!

############################
## Logistic regression - can we predict whether an individual is diabetic? 
############################

# Split the data into test and train. 
# The training data is used to fit the logistic regression model. 
# The test data is used to calculate predictions and accuracy measures to evaluate the model. 

train = ds_diabetes2[69:768,] # select the first 700 observations for training
test = ds_diabetes2[1:68,] # select the last 68 observations for testing

model = glm(diabetes ~.,family=binomial(link='logit'), data=train)

# Evaluate model - don't worry about what the output means now, that will be addressed in later modules
# You might have a slight different result in python due to the optimization procedure
summary(model)

# Prediction  - this computes the probability that an individual is diabetic
fitted = predict(model,newdata=test,type='response')

# Let us see what the predictions are. The predictions are not binary!
fitted

# Add a threshold and transform it
threshold = 0.5
fitted = ifelse(fitted > threshold,1,0)

## Compute accuracy and confusion matrix
# First transform the factor to numeric and subtract 1. 
# This converts the output to either 0 or 1.
test_numeric = as.numeric(test$diabetes)-1

# Evaluate the average miss classifier
misclass = mean(fitted != test_numeric) # The operator != evaluates which elements are different element-wise
print(paste('Accuracy',1-misclass))

# Confusion matrix 
table(fitted,test_numeric)

# Remove the triceps column and run the same model and compare performace
ds_diabetes3 = ds_diabetes2[, -4]

############################
## Logistic regression - can we predict whether an individual is diabetic? 
############################

# Split the data into test and train. 
# The training data is used to fit the logistic regression model. 
# The test data is used to calculate predictions and accuracy measures to evaluate the model. 

train = ds_diabetes3[69:768,] # select the first 700 observations for training
test = ds_diabetes3[1:68,] # select the last 68 observations for testing

model = glm(diabetes ~.,family=binomial(link='logit'), data=train)

# Evaluate model - don't worry about what the output means now, that will be addressed in later modules
# You might have a slight different result in python due to the optimization procedure
summary(model)

# Prediction  - this computes the probability that an individual is diabetic
fitted = predict(model,newdata=test,type='response')

# Let us see what the predictions are. The predictions are not binary!
fitted

# Add a threshold and transform it
threshold = 0.5
fitted = ifelse(fitted > threshold,1,0)

## Compute accuracy and confusion matrix
# First transform the factor to numeric and subtract 1. 
# This converts the output to either 0 or 1.
test_numeric = as.numeric(test$diabetes)-1

# Evaluate the average miss classifier
misclass = mean(fitted != test_numeric) # The operator != evaluates which elements are different element-wise
print(paste('Accuracy',1-misclass))

# Confusion matrix 
table(fitted,test_numeric)

# Summary
# The above result suggests that removing the column triceps had little / no effect on the accuracy of the model.

# Remove all the columns that have any na/ missing data
ds_diabetes4 = ds_diabetes[ , colSums(is.na(ds_diabetes)) == 0]
ds_diabetes4$diabetes = as.factor(ds_diabetes4$diabetes)

############################
## Logistic regression - can we predict whether an individual is diabetic? 
############################

# Split the data into test and train. 
# The training data is used to fit the logistic regression model. 
# The test data is used to calculate predictions and accuracy measures to evaluate the model. 

train = ds_diabetes4[69:768,] # select the first 700 observations for training
test = ds_diabetes4[1:68,] # select the last 68 observations for testing

model = glm(diabetes ~.,family=binomial(link='logit'), data=train)

# Evaluate model - don't worry about what the output means now, that will be addressed in later modules
# You might have a slight different result in python due to the optimization procedure
summary(model)

# Prediction  - this computes the probability that an individual is diabetic
fitted = predict(model,newdata=test,type='response')

# Let us see what the predictions are. The predictions are not binary!
fitted

# Add a threshold and transform it
threshold = 0.5
fitted = ifelse(fitted > threshold,1,0)

## Compute accuracy and confusion matrix
# First transform the factor to numeric and subtract 1. 
# This converts the output to either 0 or 1.
test_numeric = as.numeric(test$diabetes)-1

# Evaluate the average miss classifier
misclass = mean(fitted != test_numeric) # The operator != evaluates which elements are different element-wise
print(paste('Accuracy',1-misclass))

# Confusion matrix 
table(fitted,test_numeric)

# Summary
# The above result suggests that even though the columns had missing data, 
# because the accuracy went down from 67.64% to 55.88%, 
# there were some useful information in them that was not worth getting rid of.