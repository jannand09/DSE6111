
# Load libraries

library(ISLR2)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(boot)


# Load data set

# Code provided by https://archive.ics.uci.edu/dataset/320/student+performance
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

# Use student data from d2 which is Portuguese class data
student.data <- d2

# Import liver disorder data

liver <- read.table("bupa.data", sep=",", encoding = "UTF-8")
colnames(liver) <- c("mcv", "alkphos", "sgpt", "sgot", "gammagt", "drinks", "selector")

liver <- as.data.frame(liver)

# Check correlations between predictors and the number of drinks
liver_cor <- cor(liver[,-7], use="complete.obs")
print(liver_cor[6, ])

# Multiple linear regression with all predictors
lm.liver <- lm(drinks ~ ., data = liver[,-7])
summary(lm.liver)

# Import wine quality data

white.quality <- read.csv("winequality-white.csv", sep=";", na.strings = "?", stringsAsFactors = T)
View(white.quality)

red.quality <- read.csv("winequality-red.csv", sep=";", na.strings = "?", stringsAsFactors = T)
View(red.quality)

# Check correlations between predictors and the number of drinks
white_cor <- cor(white.quality, use="complete.obs")
print(white_cor[12, ])

# Multiple linear regression with all predictors
lm.white <- lm(quality ~ ., data = white.quality)
summary(lm.white)

# Create training data
set.seed(20)
train.white <- sample(1:nrow(white.quality), 0.5 * nrow(white.quality))
test.white <- (-train.white)

# Train the multiple linear regression model with selected predictors
lm.train <- lm(quality ~ volatile.acidity + residual.sugar + free.sulfur.dioxide
               + density + pH + sulphates + alcohol, data = white.quality,
               subset = train.white)
summary(lm.train)

lm.predict <- predict(lm.train, white.quality[test.white, ])
lm.mse <- mean((lm.predict - white.quality[test.white, ]$quality)^2)
lm.mse

# Ridge Regression

# Create matrix of x, the predictors, and vector of y, the response
x <- model.matrix(quality ~ ., white.quality)[, -12]
y <- white.quality$quality

y.test <- y[test.white]

# Create a lambda grid and use it to form ridge regression model
lambda.grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train.white, ], y[train.white], alpha = 0, lambda = lambda.grid,
                    thresh = 1e-12)

# Determine the bets lambda, or tuning parameter, using cross-validation
set.seed(2)
cv.out <- cv.glmnet(x[train.white, ], y[train.white], alpha = 0)
plot(cv.out)
bestlam.ridge <- cv.out$lambda.min
bestlam.ridge

# Predict the response of test data using ridge regression with best tuning parameter
ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = x[test.white, ])
ridge.mse <- mean((ridge.pred - y.test)^2)
ridge.mse


# The Lasso
lasso.mod <- glmnet(x[train.white, ], y[train.white], alpha = 1, lambda = lambda.grid)
plot(lasso.mod)

# Perform cross-validation to determine best tuning parameter
set.seed(4)
cv.out <- cv.glmnet(x[train.white, ], y[train.white], alpha = 1)
plot(cv.out)
bestlam.lasso <- cv.out$lambda.min
bestlam.lasso

# Predict the response of test data and calculate MSE
lasso.pred <- predict(lasso.mod, s = bestlam.lasso, newx = x[test.white, ])
lasso.mse <- mean((lasso.pred - y.test)^2)
lasso.mse


# Partial Least Squares

# Create PLS model on the white wine quality data
set.seed(5)
pls.fit <- plsr(quality ~ ., data = white.quality, subset = train.white, scale = T,
                validation = "CV")
summary(pls.fit)

# Plot MSEP over the number of components
validationplot(pls.fit, val.type = "MSEP")
axis(side=1, at=seq(1, 20, by=1))

# Predict quality of the wine using PLS
pls.pred <- predict(pls.fit, x[test.white, ], ncomp=2)
pls.mse <- mean((pls.pred - y.test)^2)
pls.mse


# Best Subset Selection

# Use validation set approach to determine best subset selection model
regfit.best <- regsubsets(quality ~ ., data = white.quality[train.white, ], nvmax = 11)

# Create test matrix
test.mat <- model.matrix(quality ~ ., data = white.quality[test.white, ])

# Compute test MSE for all possible amounts of variables used in the model
val.errors <- rep(NA, 13)
for (i in 1:11) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((white.quality$quality[test.white] - pred)^2)
}

# Get coefficient estimates for model with best subset collection
best.subset <- which.min(val.errors)
val.errors[best.subset]
coef(regfit.best, best.subset)


# Write custom function for prediction with subset selection models

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# Forward Step-wise Subset Selection Using Cross-Validation

k <- 10
n <- nrow(white.quality)
set.seed(11)
folds <- sample(rep(1:k, length = n))
f.cv.errors <- matrix(NA, k, 11,
                    dimnames = list(NULL, paste(1:11)))

for (j in 1:k) {
  fstep.fit <- regsubsets(quality ~ .,
                         data = white.quality[folds != j, ],
                         nvmax = 11,
                         method = "forward")
  for (i in 1:11) {
    pred.forward <- predict.regsubsets(fstep.fit, white.quality[folds == j, ], id = i)
    f.cv.errors[j, i] <- mean((white.quality$quality[folds == j] - pred.forward)^2)
  }
}

forward.cv.errors <- apply(f.cv.errors, 2, mean)
forward.cv.errors
par(mfrow = c(1,1))
plot(forward.cv.errors, type = "b")

which.min(forward.cv.errors)
forward.mse <- forward.cv.errors[["11"]]
forward.mse

# Backward Step-wise Subset Selection Using Cross Validation

k <- 10
n <- nrow(white.quality)
set.seed(11)
folds <- sample(rep(1:k, length = n))
b.cv.errors <- matrix(NA, k, 11,
                    dimnames = list(NULL, paste(1:11)))

for (j in 1:k) {
  bstep.fit <- regsubsets(quality ~ .,
                          data = white.quality[folds != j, ],
                          nvmax = 11,
                          method = "backward")
  for (i in 1:11) {
    pred.backward <- predict.regsubsets(fstep.fit, white.quality[folds == j, ], id = i)
    b.cv.errors[j, i] <- 
      mean((white.quality$quality[folds == j] - pred.backward)^2)
  }
}

backward.cv.errors <- apply(b.cv.errors, 2, mean)
backward.cv.errors
par(mfrow = c(1,1))
plot(backward.cv.errors, type = "b")

backward.mse <- backward.cv.errors[["11"]]
backward.mse
