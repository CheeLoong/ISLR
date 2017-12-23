# Lab: KNN
## Loading Packages
library(class)

## Setting up Input 1: Matrix containing predictors associated with training data
train.X = cbind(Lag1, Lag2)[train,]
head(train.X)

## Setting up Input 2: A matrix containing the predictors associated with the test data
test.X=cbind(Lag1,Lag2)[!train ,] 
head(test.X)

## Setting up Input 3: A vector containing the class labels for the training observations
train.Direction = Direction[train]

## Use knn() function to predict, arbitrarily set k, the number of nearest neighbour
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)

## Produce Confusion matrix to check prediction accuracy
table(knn.pred, Direction.2005)
mean(knn.pred != Direction.2005) # test error rate = 50%

## Repeat analysis with K = 3
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
knn_test_error = mean(knn.pred != Direction.2005) # test error rate = 46.4%

#--------------------------------------------------------------------------

## Summary
summary = data.frame(logistic_test_error, lda_test_error, qda_test_error, knn_test_error)
summary # QDA provides the best results of the methods that we have examined so far.
