## Fitting Regression Tree (Lab: Boston data)

## loading library
library(MASS)
set.seed(1)

## create training and test set
train = sample(1 : nrow(Boston), nrow(Boston) / 2)
Boston.test <- Boston[-train, ]

## fitting regression tree
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)

## plot the decision tree
plot(tree.boston)
text(tree.boston, pretty = 0)

## using cv to determine the right amount terminal nodes
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

## lets say we want to prune anyway?
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

## predicting on test data using fitted model
tree.pred = predict(tree.boston, newdata = Boston.test )
medv.test = Boston.test$medv
plot(tree.pred , medv.test)
abline(0, 1)
mean((tree.pred - medv.test)^2) # test mse is 25.05% 
