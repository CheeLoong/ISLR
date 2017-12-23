# Lab: LDA
## Loading Packages
library(MASS)

## Fitting LDA model
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit) # plots of linear discriminants

## Estimate Predicted Probabilities
lda.pred = predict(lda.fit, Smarket.2005)

## Extract the predicted class
names(lda.pred)
lda.class = lda.pred$class

## Produce Confusion matrix to check prediction accuracy
table(lda.class, Direction.2005)
lda_test_error = mean(lda.class != Direction.2005) # test error rate = 44%

## Applying 50% threshold to posterior probabilities of decrease to count the prediction
sum(lda.pred$posterior[,1] >= 0.5) # predicted 70 market decrease
sum(lda.pred$posterior[,1] < 0.5) # predicted 182 market increase

## Applying 90% threshold to posterior probabilities of decrease to count the prediction
sum(lda.pred$posterior[,1] >= 0.9) # predicted 0 market decrease
max(lda.pred$posterior[,1]) # greatest posterior prob of decrease
sum(lda.pred$posterior[,1] < 0.9) # predicted 252 market increase

#--------------------------------------------------------------------------
# Lab: QDA

## Fitting QDA model
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

## Estimate Predicted Probabilities
qda.pred = predict(qda.fit, Smarket.2005)

## Extract the predicted class
qda.class = qda.pred$class

## Produce Confusion matrix to check prediction accuracy
table(qda.class, Direction.2005)
qda_test_error = mean(qda.class != Direction.2005) # test error rate = 40%