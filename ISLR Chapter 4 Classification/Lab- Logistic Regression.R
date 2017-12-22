# Lab: Logistic Regression
## Loading Packages
library(ISLR)
attach(Smarket)
names(Smarket)
head(Smarket)
str(Smarket)
summary(Smarket)
cor(Smarket[-9]) # no substantial coorelation, except Year and Volume, by plotting the data we see volume is increasing over time

## Fitting Logistic Regression Model
glm.fit = glm(Direction ~ .-Year -Today, data = Smarket, family = binomial)
summary(glm.fit) # no statistical significant predictors, all pvalues > 0.05
summary(glm.fit)$coef # extract coefficient estimates table
summary(glm.fit)$coef[,4] # extract pvalues only

## Estimate Predicted Probabilities using Logistic Function 
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10] 

## Convert Predicted Probabilities into Class Labels
contrasts(Direction) # show dummy coding
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

## Produce Confusion matrix to check prediction accuracy
table(glm.pred, Direction)
(145 + 507) / 1250 
mean(glm.pred == Direction) ## 100 - 52.2 = 47.8% training error rate

#--------------------------------------------------------------------------

## Split data into train and test set
train = (Year < 2005)
Smarket.2005 = Smarket[!train,] 
dim(Smarket.2005)
Direction.2005 = Direction[!train] # test y

## Fitting Logistic Regression Model (Using only train data)
glm.fit = glm(Direction ~.-Today-Year, data = Smarket, family = binomial, subset = train)
summary(glm.fit)$coef

## Estimate Predicted Probabilities using Logistic Function
glm.probs = predict(glm.fit, Smarket.2005, type = "response")

## Convert Predicted Probabilities into Class Labels
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

## Product Confusion matrix to check prediction accuracy
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
logistic_test_error = mean(glm.pred != Direction.2005) # test error rate = 52%

#--------------------------------------------------------------------------

## Refitting model with just Lag1 & Lag2
glm.fit = glm(Direction~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
summary(glm.fit)$coef

## Estimate Predicted Probabilities using Logistic Function 
glm.probs = predict(glm.fit, Smarket.2005, type = "response")

## Convert Predicted Probabilities into Class Labels
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

## Produce Confusion matrix to check prediction accuracy
table(glm.pred, Direction.2005)
mean(glm.pred != Direction.2005) # test error rate = 44%, this confusion matrix shows that on days when logistic regression predicts an increase n the market, it has a 58% accuracy rate. This suggests a possible trading strategy of buying on days when the model predicts an increasing market, and avoid buying when a decrease is predicted. However, need more investigation to check whether it was real or by chance.

## Predict the returns associated with particular values of "Lag" 
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")



































