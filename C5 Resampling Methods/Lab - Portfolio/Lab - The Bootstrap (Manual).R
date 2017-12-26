# The Bootstrap (manual)

## Create function that computes the statistic of interest
alpha.fn = function(data, index){
        X = data$X[index]
        Y = data$Y[index]
        return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100) # estimate alpha using all 100 observations

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) 

## Perform bootstrap by repeatedly sampling observations from data with replacement
set.seed(10)
alpha.hat = rep(0, 1000)
for (i in 1:1000){
        alpha.hat[i] = alpha.fn(Portfolio, sample(100, 100, replace = T))+(i*0)
}
head(alpha.hat)

## mean over all of the alpha estimates
alpha.bar = mean(alpha.hat)
alpha.bar

## standard deviation of the estimate, SE(alpha_hat)
alpha.SE = sqrt((1/999)*sum((alpha.hat - alpha.bar)^2))
alpha.SE / sqrt(1000)
