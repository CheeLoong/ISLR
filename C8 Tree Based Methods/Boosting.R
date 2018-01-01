## Boosting (Boston data)

## load packages
library(gbm)
library(ISLR) ; library(MASS)

## create train and test set
set.seed(1)
train = sample(1 : nrow(Boston), nrow(Boston) / 2)
Boston.train <- Boston[train, ]
Boston.test <- Boston[-train, ]

## train the model (boosting)
boost.fit = gbm(medv ~., data = Boston.train, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.fit)

## produce partial dependence plots
par(mfrow = c(1, 2))
plot(boost.fit, i = "rm") # house prices are increasing with rm
plot(boost.fit, i = "lstat") # house prices are decreasing with lstat

## produce prediction on the test data using fitted model
boost.pred = predict(boost.fit, newdata = Boston.test, n.trees = 5000)
medv.test = Boston.test$medv
mean((boost.pred - medv.test )^2) # the boosting test MSE is 11.8%

#-----------------------------------------------------------------------------

## train the model (boosting with a larger shrinkage parameter)
boost.fit = gbm(medv ~., data = Boston.train, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
summary(boost.fit)

## produce prediction on the test data using fitted model
boost.pred = predict(boost.fit, newdata = Boston.test, n.trees = 5000)
mean((boost.pred - medv.test )^2) # the boosting test MSE (with shrink = 0.2) is 11.4%
