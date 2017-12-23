# Logistic function vs Logit (extra) 

## load packages
library(ISLR)
attach(Smarket)
names(Smarket)
head(Smarket)
str(Smarket)
summary(Smarket)
cor(Smarket[-9])

## fit logistic regression
glm.fit2 = glm(Direction ~ Lag1, data = Smarket, family = binomial)
summary(glm.fit2)$coef

## log odds / logit
# manual 
b0 = summary(glm.fit2)$coef[1,1] 
b1 = summary(glm.fit2)$coef[2,1]
x = Smarket$Lag1[1:6]
logit = b0 + b1*x # log odds / logit
logit
# formula 
glm.probs2 = predict(glm.fit2)
head(glm.probs2)

## logistic function, P(Y = 1 | X)
# manual 
px = exp(logit)/(1 + exp(logit))
px
# formula
glm.probs2p = predict(glm.fit2, type = "response")
head(glm.probs2p)