# Lab: Cross Validation

#--------------------------------------------------------------------------

# The Validation Set Approach
## Loading Packages 
library(ISLR)
attach(Auto)

## Validation Set Approach to split data
set.seed(1)
train = sample(392, 196)
Auto.test = Auto[-train, ] 
mpg.test = mpg[-train] # test_y

## Fitting Linear Regression Model
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

## Predict using Linear Regression Model
lm.pred = predict(lm.fit, Auto.test) # y_hat

## Check Prediction accuracy
mean((mpg.test - lm.pred)^2) # test MSE = 26.14


## Refitting same model with polynomial regressions.
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
lm.pred2 = predict(lm.fit2, Auto.test)
mean((mpg.test - lm.pred2)^2) # test MSE = 19.82

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
lm.pred3 = predict(lm.fit3, Auto.test)
mean((mpg.test - lm.pred3)^2) # test MSE = 19.78

## Using a different split of observations into train and test set
set.seed(2)
train = sample(392, 196)
Auto.test = Auto[-train, ] 
mpg.test = mpg[-train]

## Fitting models, predict, and check prediction accuracy
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
lm.pred = predict(lm.fit, Auto.test)
mean((mpg.test - lm.pred)^2) # test MSE = 23.30

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
lm.pred2 = predict(lm.fit2, Auto.test)
mean((mpg.test - lm.pred2)^2) # test MSE = 18.90

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
lm.pred3 = predict(lm.fit3, Auto.test)
mean((mpg.test - lm.pred3)^2) # test MSE = 19.26

# A model that predicts mpg using a quadratic function of horsepower performs better than a model that involves only a linear function of horsepower,and there is little evidence in favor of a model that uses a cubic function of horsepower.

#--------------------------------------------------------------------------

# Leave-One-Out Cross-Validation (LOOCV)
## Loading Packages
library(boot)

## Fitting Linear Regression Model with glm() function
lm.fit = lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
glm.fit = glm(mpg ~ horsepower, data = Auto) # this is also a linear regression
coef(glm.fit)

## Compute LOOCV estimate for the test error
loocv.err = cv.glm(Auto, glm.fit)
loocv.err$delta # LOOCV estimate for the test MSE

## Automate this procedure for increasingly complex polynomial fits
loocv.error = rep(0, 5)
for (i in 1:5){
        glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
        loocv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
loocv.error # shows LOOCV estimate for test error up to 5th order of polynomial fits

# According to the LOOCV algorithm, we see a sharp drop in the estimated test MSE between the linear and quadratic fits, but then no clear improvement from using higher-order polynomials.

#--------------------------------------------------------------------------

# K-Fold Cross-Validation 

## Compute K-Fold Cross-Validation estimate for the test error
set.seed(17)
kfoldcv.error  = rep(0, 10)
for (i in 1:10){
        glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
        kfoldcv.error[i] = cv.glm(Auto,glm.fit, K = 10)$delta[1]
}
kfoldcv.error

# We still see little evidence that using cubic or higher-order polynomial terms lead to lower test error than simply using a quadratic fit.

