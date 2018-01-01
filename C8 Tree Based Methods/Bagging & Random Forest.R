## Bagging and Random Forests (Boston data)

## load packages
library(randomForest)

## create training and test set
set.seed(1)
train = sample(1 : nrow(Boston), nrow(Boston) / 2)
Boston.test <- Boston[-train, ]

## train the model (Bagging - m = p)
bag.fit = randomForest(medv ~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.fit

## predicting on test data using fitted model
bag.pred = predict(bag.fit, newdata = Boston.test)
medv.test = Boston.test$medv
plot(bag.pred, medv.test)
abline(0, 1)
mean((bag.pred - medv.test)^2) # test mse is 13.34%

#--------------------------------------------------------

## train the model (with custom selection of trees)
bag.fit = randomForest(medv ~., data = Boston, subset = train, mtry = 13, ntree = 25)
bag.fit

## prediciting on test data using fitted model
bag.pred = predict(bag.fit, newdata = Boston.test)
mean((bag.pred - medv.test)^2)  # test mse is 15.97%

#--------------------------------------------------------

## train the model (random forest - m = sqrt(p))
set.seed(1)
rf.fit = randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = TRUE)
rf.fit

## prediciting on test data using fitted model
rf.pred = predict(rf.fit, newdata = Boston.test)
mean((rf.pred - medv.test)^2) # test mse is 11.48% 

## viewing importance of the variables
importance(rf.fit)
varImpPlot(rf.fit)

# random forest test mse : 11.48% , bagging test mse: 13.34%, regression tree test mse: 25.05%, in this example random forest dominates other methods.
