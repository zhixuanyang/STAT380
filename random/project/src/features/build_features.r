rm(list = ls())
library(data.table)
library(Metrics)
library(caret)
library(glmnet)

# Question from Class: What is the probability of a random matrix begin intervitble?

zeros = c()

for (n in 1:30) {
  q = c()
  for (i in 1:10000) {
    M = matrix(sample(c(0, 1), n^2, replace = TRUE), n, n)
    d = round(det(M), 0)
    q = c(q, d)
  }
  totzero = sum(q == 0)
  zeros = c(zeros, totzero)
  cat(sum(q == 0), " / 10,000\n")
  hist(q)
}
print(zeros)

plot(zeros / 10000, xlab = "dim(M)", ylab = "% non-invertible")


# What happens to MSE and RSE as number of features increases?

data = fread("train_extra.csv")
remove = c("id", "current_date", "future_date")
data[, (remove) := NULL]

# Split the data into two equal sized train and test

idx = sample(1:nrow(data), nrow(data) / 2)
length(idx)
# There are 2419 obs in the training set.
# Let's pump up the number of features to 2,500

n = 2500 - (ncol(data)-1)
n
# We need to add 1990 extra features.  Chances are these will be linearly independent

M = as.data.frame(matrix(sample(c(0, 1), n * nrow(data), replace = TRUE), ncol = n))
setnames(M, paste0("Extra_", 501:(500 + 1990)))
colnames(M)
data = cbind(data, M)
train = data[idx]
test  = data[!idx]
sizes = c(10, seq(50, 500, 50), seq(1000, 2500, 500))
sizes
results = c()
for (size in sizes) {
  cat("size = ", size, "\n")
  fit = lm(future_price ~ ., data = train[, 1:(size + 1)])
  pred = predict(fit, test, type = "response")
  results = rbind(results, data.table(size = size, 
                                      train.rmse    = rmse(train$future_price, fit$fitted.values),
                                      train.r.sqr   = summary(fit)$r.squared,
                                      train.adj.r.sqr = summary(fit)$adj.r.squared,
                                      test.rmse     = rmse(test$future_price, pred)))
}
results
plot(train.r.sqr ~ size, data = results, type = "l",
     xlab = "number of covariates", ylab = "r^2")
plot(train.adj.r.sqr ~ size, data = results, type = "l",
     xlab = "number of covariates", ylab = "adj r^2")
plot(train.rmse ~ size, data = results, type = "l",
     xlab = "number of covariates", ylab = "training rmse")
plot(test.rms ~ size, data = results, type = "l",
     xlab = "number of covariates", ylab = "testing rmse")

# Moral: be careful with high dimensions!
# Forward stepwise selection
# Ridge Regression
# Lasso Regression
# are all useful when preforming regression in high-dimensions

# Note, both forward and backward stepwise selection fail since we have *PERFECT* fits

fit.forward = step(fit, direction = "forward")
fit.backward = step(fit, direction = "backward")

# Let's do ridge regression with just the important data and with the garbage
# data added. To keep the code simple, I am not going to do the cross validation
# method that we learned on Monday.

y.train = train$future_price
y.test  = test$future_price
dummies = dummyVars(future_price ~ ., data = data)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)
real.data.idx = 1:13

gl.train.real <- cv.glmnet(train[, real.data.idx], 
                           y.train, 
                           nfolds = 50,
                           alpha = 1, 
                           family = "gaussian")
gl.train.all <- cv.glmnet(train,
                          y.train,
                          nfolds = 50,
                          alpha = 1,
                          family = "gaussian")
bestlam.real = gl.train.real$lambda.min
bestlam.all  = gl.train.all$lambda.min

pred.real = predict(gl.train.real, s = bestlam.real, newx = test[, real.data.idx])
pred.all  = predict(gl.train.all,  s = bestlam.all,  newx = test)
coefs.real = predict(gl.train.real, s = bestlam.real, newx = test[, real.data.idx], type = "coefficients")
coefs.all  = predict(gl.train.all, s = bestlam.all, new = test, type = "coefficients")

# How many coefficients did we end up with in each case?
sum(coefs.real != 0)   
sum(coefs.all  != 0)   # a lot more!

coefs.real
coefs.all[coefs.all[, 1] != 0, ]  # Note some of the "real" predictors didn't show up.


# How about RMSE?

rmse(y.test, pred.real)
rmse(y.test, pred.all)  
# We ended up with alot of junk, BUT it didn't hurt the prediction much

