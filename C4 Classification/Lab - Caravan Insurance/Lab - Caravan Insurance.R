# An Application to Caravan Insurance Data

# K Nearest Neighbour
## Loading Packages
library(ISLR)
library(class)
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822 # only about 6% of people purchased caravan insurance

## Standardizing data so all variables are on a comparable scale (mean = 0, sd = 1)
standardized.X = scale(Caravan[, -86]) # exclude col 86, the qualitative Purchase variable
var(Caravan[,1])
mean(Caravan[,1])
var(Caravan[,2])
mean(Caravan[,2])
var(standardized.X[,1])
mean(standardized.X[,1])
var(standardized.X[,2])
mean(standardized.X[,2])

## Split data into train and test set (first 1000 as test, remaining as train)
test = 1 : 1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

## Fitting KNN model on the training data using K = 1
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)

## Produce Confusion matrix to check prediction accuracy
table(knn.pred, test.Y)
mean(knn.pred != test.Y) # test error rate = 11.8%
mean(test.Y != "No") # 6% purchased caravan insurance, so a naive model achieve a test error rate of 6%, but in this case, the fraction of individuals that are correctly predicted to buy insurance is of interest, not the overall error rate. 
9/(68+9) # 11.7% correct prediction, double the rate of random guessing (i.e. 6%)

## Fitting KNN model on the training data using K = 3
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/(21+5) # 19% correct prediction.

## Fitting KNN model on the training data using K = 5
knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/(11+4) # 26.7% correct prediction.

#--------------------------------------------------------------------------

# Logistic Regression
## Fitting Logistic Regression Model
train = -test
glm.fit = glm(Purchase ~ ., data = Caravan, family = binomial, subset = train)
summary(glm.fit)

## Estimate Predicted Probabilities using Logistic Function 
glm.probs = predict(glm.fit, Caravan[test,], type = "response")
glm.probs[1:10] 

## Convert Predicted Probabilities into Class Labels
contrasts(Purchase) # show dummy coding
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"

## Produce Confusion matrix to check prediction accuracy (threshold = 50%)
table(glm.pred, test.Y) 
0/(0+7) # 50% threshold predicts only 7 observation would purchase, and all wrong!

## Convert Predicted Probabilities into Class Labels using a lower threshold
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .25] = "Yes"

## Produce Confusion matrix to check prediction accuracy (threshold = 25%)
table(glm.pred, test.Y)
11/(22+11) # Using a 25% threshold, our correct prediction is 33.3%.
