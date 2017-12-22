# Lab : Simple Linear Regression

# Libraries
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)
attach(Boston)
?Boston

# Simple Linear Regression
lm.fit = lm(medv ~ lstat, data = Boston)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit) # coefficient estimates only
confint(lm.fit) # confidence interval for coefficient estimates

predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence") # confidence interval for average prediction of medv given lstat
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction") # prediction interval for single prediction of medv given lstat

# Plot Y against X 
plot(lstat, medv)
abline(lm.fit)

# Explore plotting options
abline(lm.fit, lwd = 3) # adjust width 
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red") # adjust data points colour
plot(1:20, 1:20, pch = 1:20) # exploring pch options
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")

# Diagnostic plot 
par(mfrow = c(2,2)) # make 2x2 panels
plot(lm.fit)

par(mfrow = c(1,1))
plot(predict(lm.fit), residuals(lm.fit)) # residual plot
plot(predict(lm.fit), rstudent(lm.fit)) # studentized residuals
plot(hatvalues(lm.fit)) # leverage statistics
which.max(hatvalues(lm.fit)) # identifies the index of the highest leverage

