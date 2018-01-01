# Chapter 3 Linear Regression Revision

## Data simulation
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, sd = sqrt(0.25))
y <- -1 + 0.5 * x + eps
length(y)

## run least squares linear model using lm() function
lm.fit <- lm(y ~ x)
summary(lm.fit)

## run least squares linear model using standard formulae manually
## estimating the beta coefficients
xbar = mean(x)
ybar = mean(y)
beta1 <- (sum((x - xbar)*(y - ybar))) / sum((x - xbar)^2) 
beta0 <- ybar - beta1 * xbar

## assessing the accuracy of the coeficient estimates
est.y <- beta0 + beta1 * x
e <- y - est.y
RSS <- sum(e^2)

n <- length(y)
RSE <- sqrt(RSS / (n-2)) # a.k.a sigma hat
ssx <- sum((x - mean(x))^2)
seBeta1 <- RSE / sqrt(ssx)
seBeta0 <- RSE * sqrt(((1 / n) + (xbar^2 / ssx)))

tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1

## assessing the accuracy of the model
TSS <- sum((y - ybar)^2)
R.squared <- (1 - (RSS / TSS))
RSE

#-------------------------------------------------------------------------------

## how to get p-value of the F-stats? (More useful on Multiple Linear Regression)
pf(85.99, 1, 98, lower.tail = F)

#-------------------------------------------------------------------------------

## Checking the assumptions of linear regression + additional insights
# (i) run correlation matrix, pick high correlation pairs to include interaction term
# (ii) in the pairwise scatter plot, look out for non-linear patterns (log, quadratic etc.) and include relevant transformation (e.g. taking log of response) in our model.
# (iii) in plot(lm.fit) residuals vs fitted plot, check for non-linear pattern / heteroscedasticity.
# (iv) in plot(lm.fit) Q-Q plot, check for non-normality of the residuals
# for issue (iii) or (iv) try taking log transformation, or add quadratic term to the model.
# (v) use vif() to check for multi-collinearity.
vif(lm.fit)
# (vi) use the studentized residuals against fitted values displays potential outliers (|>3|).
plot(predict(lm.fit), rstudent(lm.fit))
# (vii) use hatvalues(lm.fit) to plot the leverage stats, and find ones thats 2-3 times more than the average leverage stats
summary(lm.fit)
mn <- mean(hatvalues(lm.fit))
plot(hatvalues(lm.fit), type = "h")
abline(h = mn, col = "blue")
abline(h = 3*mn, col = "red")
