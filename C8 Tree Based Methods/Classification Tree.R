## Fitting Classification Tree (Lab: Carseats data)


## loading library
library(tree)
library(ISLR)
attach(Carseats)
head(Carseats)

## create binary response
High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

## fitting classification tree
tree.carseats = tree(High ~. -Sales, Carseats)
summary(tree.carseats) # training error rate is 9%

## plot the decision tree
plot(tree.carseats)
text(tree.carseats, pretty = 0)

#------------------------------------------------------------------

## create training and test sets
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]
High.test = Carseats.test$High

## fitting classification tree
tree.carseats = tree(High ~. -Sales, Carseats, subset = train)

## predicting on test data using fitted model
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) # 71.5% correct predictions

## more on pruning, but since we're doing ensemble learning, I'll skip on pruning

