# Lab : Multiple Linear Regression

## Load data and fit MLR
library(MASS)
library(ISLR)
attach(Boston)
lm.fit = lm(medv ~ lstat + age, data = Boston) # fit all variables
summary(lm.fit)

## MLR on all variables
lm.fit = lm(medv ~., data = Boston) 
summary(lm.fit)
summary(lm.fit)$r.sq # R-squared
summary(lm.fit)$sigma # RSE 

## Compute VIFs
library(car)
vif(lm.fit) # compute VIF

## MLR on all variables and exclude subset of th variables
lm.fit1 = lm(medv~. -age, data = Boston) # fit all variables but exclude age variable 
summary(lm.fit1)
lm.fit1 = update(lm.fit, ~. -age) # can also update current linear model

## Interaction Terms 
summary(lm(medv ~ lstat*age, data = Boston)) # interaction term

## Non-linear Transformations of the Predictors
lm.fit2 = lm(medv ~ lstat + I(lstat^2)) # quadratic regresion
summary(lm.fit2)
lm.fit = lm(medv ~ lstat) # linear regression
anova(lm.fit, lm.fit2) # the F-stats and associated p-values provides clear evidence that quadratic model is far superior to the linear model.
par(mfrow = c(2,2))
plot(lm.fit2) # only a little discernible pattern in the residuals

lm.fit5 = lm(medv ~ poly(lstat, 5)) # fit a fifth-order polynomial regression
summary(lm.fit5)
summary(lm(medv~log(rm), data = Boston)) # log transformation on predictors

## Qualitative Predictors
str(Carseats)
names(Carseats)
lm.fit = lm(Sales~. + Income: Advertising + Price: Age, data = Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

## Writing Functions
LoadLibraries = function(){
        library(ISLR)
        library(MASS)
        print("The libraries have been loaded")
}
LoadLibraries()
