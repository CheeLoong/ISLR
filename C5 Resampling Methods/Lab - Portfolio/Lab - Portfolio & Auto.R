# Lab: The Bootstrap

#--------------------------------------------------------------------------

# Estimating the Accuracy of a Statistic of Interest

## Loading Packages
library(ISLR)
library(boot)

## Create function that computes the statistic of interest
alpha.fn = function(data, index){
        X = data$X[index]
        Y = data$Y[index]
        return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100) # estimate alpha using all 100 observations

## Create bootstrap estimates for alpha by sampling observations from data with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) 

## Automate the bootstrap procedure for 1,000 times
boot(Portfolio, alpha.fn, R = 1000)

# The final output shows that using the original data, ˆα =0.5758, and that the bootstrap estimate for SE(ˆα) is 0.0886.

#--------------------------------------------------------------------------

# Estimating the Accuracy of a Linear Regression Model

## Create function that returns the intercept and slope estimates for the linear regression model
boot.fn = function(data, index){
        return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

## Create bootstrap estimates for the intercept and slope by sampling observations from data with replacement
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

## Automate the bootstrap procedure for 1,000 times
boot(Auto, boot.fn, R = 1000)
# The standard error estimates for B0_hat and B1_hat obtained using the bootstrap is 0.86 and 0.0074 respectively. 

## Comparing with the standard formulas 
summary(lm(mpg ~ horsepower, data = Auto))$coef
# The standard error estimates for B0_hat and B1_hat obtained using the standard formulas is 0.717 and 0.0064 respectively. 







 
 