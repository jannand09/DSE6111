
# Load libraries

library(ISLR2)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(boot)
library(tree)
library(randomForest)
library(gbm)
library(BART)


# Import wine quality data

wine.data <- read.csv("winequality-white.csv", sep=";", na.strings = "?", stringsAsFactors = T)
View(wine.data)

# Check correlations between predictors and the number of drinks
wine_cor <- cor(wine.data, use="complete.obs")
print(wine_cor[12, ])

# Multiple linear regression with all predictors
lm.wine <- lm(quality ~ ., data = wine.data)
summary(lm.wine)

# Create training data
set.seed(10)
train.wine <- sample(1:nrow(wine.data), 0.5 * nrow(wine.data))
test.wine <- (-train.wine)

# Train the multiple linear regression model with selected predictors
lm.train <- lm(quality ~ volatile.acidity + residual.sugar + free.sulfur.dioxide
               + density + pH + sulphates + alcohol, data = wine.data,
               subset = train.wine)
summary(lm.train)

res <- resid(lm.train)
plot(fitted(lm.train), res)
abline(0,0)

lm.predict <- predict(lm.train, wine.data[test.wine, ])
lm.mse <- mean((lm.predict - wine.data[test.wine, ]$quality)^2)
lm.mse

# Ridge Regression

# Create matrix of x, the predictors, and vector of y, the response
x <- model.matrix(quality ~ ., wine.data)[, -1]
y <- wine.data$quality

y.test <- y[test.wine]

# Create a lambda grid and use it to form ridge regression model
lambda.grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train.wine, ], y[train.wine], alpha = 0, lambda = lambda.grid,
                    thresh = 1e-12)

# Determine the best lambda, or tuning parameter, using cross-validation
set.seed(2)
cv.out <- cv.glmnet(x[train.wine, ], y[train.wine], alpha = 0)
plot(cv.out)
bestlam.ridge <- cv.out$lambda.min
bestlam.ridge

# Predict the response of test data using ridge regression with best tuning parameter
ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = x[test.wine, ])
ridge.mse <- mean((ridge.pred - y.test)^2)
ridge.mse


# The Lasso
lasso.mod <- glmnet(x[train.wine, ], y[train.wine], alpha = 1, lambda = lambda.grid)
plot(lasso.mod)

# Perform cross-validation to determine best tuning parameter
set.seed(2)
cv.out <- cv.glmnet(x[train.wine, ], y[train.wine], alpha = 1)
plot(cv.out)
bestlam.lasso <- cv.out$lambda.min
bestlam.lasso

# Predict the response of test data and calculate MSE
lasso.pred <- predict(lasso.mod, s = bestlam.lasso, newx = x[test.wine, ])
lasso.mse <- mean((lasso.pred - y.test)^2)
lasso.mse


# Partial Least Squares

# Create PLS model on the wine wine quality data
set.seed(2)
pls.fit <- plsr(quality ~ ., data = wine.data, subset = train.wine, scale = T,
                validation = "CV")
summary(pls.fit)


# Plot MSEP over the number of components
validationplot(pls.fit, val.type = "MSEP")
axis(side=1, at=seq(1, 20, by=1))

# Predict quality of the wine using PLS
pls.pred <- predict(pls.fit, newdata = x[test.wine, ], ncomp=3)
pls.mse <- mean((pls.pred - y.test)^2)
pls.mse


# Best Subset Selection

# Write custom function for prediction with subset selection models

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# Use validation set approach to determine best subset selection model
regfit.best <- regsubsets(quality ~ ., data = wine.data[train.wine, ], nvmax = 11)

# Create test matrix
test.mat <- model.matrix(quality ~ ., data = wine.data[test.wine, ])

# Compute test MSE for all possible amounts of variables used in the model
val.errors <- rep(NA, 13)
for (i in 1:11) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((wine.data$quality[test.wine] - pred)^2)
}

# Get coefficient estimates for model with best subset collection
best.subset <- which.min(val.errors)
val.errors[best.subset]
coef(regfit.best, best.subset)

# Best subset selection using cross-validation method

k <- 10
n <- nrow(wine.data)
set.seed(11)
folds <- sample(rep(1:k, length = n))
cv_sub.errors <- matrix(NA, k, 11,
                      dimnames = list(NULL, paste(1:11)))

for (j in 1:k) {
  cv_sub.fit <- regsubsets(quality ~ .,
                          data = wine.data[folds != j, ],
                          nvmax = 11)
  for (i in 1:11) {
    pred.cv_sub <- predict.regsubsets(cv_sub.fit, wine.data[folds == j, ], id = i)
    cv_sub.errors[j, i] <- mean((wine.data$quality[folds == j] - pred.cv_sub)^2)
  }
}

cv_sub.cv.errors <- apply(cv_sub.errors, 2, mean)
par(mfrow = c(1,1))
plot(cv_sub.cv.errors, type = "b")

which.min(cv_sub.cv.errors)
cv_sub.mse <- cv_sub.cv.errors[["8"]]
cv_sub.mse

# Forward Step-wise Subset Selection Using Cross-Validation

k <- 10
n <- nrow(wine.data)
set.seed(11)
folds <- sample(rep(1:k, length = n))
f.cv.errors <- matrix(NA, k, 11,
                    dimnames = list(NULL, paste(1:11)))

for (j in 1:k) {
  fstep.fit <- regsubsets(quality ~ .,
                         data = wine.data[folds != j, ],
                         nvmax = 11,
                         method = "forward")
  for (i in 1:11) {
    pred.forward <- predict.regsubsets(fstep.fit, wine.data[folds == j, ], id = i)
    f.cv.errors[j, i] <- mean((wine.data$quality[folds == j] - pred.forward)^2)
  }
}

forward.cv.errors <- apply(f.cv.errors, 2, mean)
forward.cv.errors
par(mfrow = c(1,1))
plot(forward.cv.errors, type = "b")

which.min(forward.cv.errors)
forward.mse <- forward.cv.errors[["8"]]
forward.mse

# Backward Step-wise Subset Selection Using Cross Validation

k <- 10
n <- nrow(wine.data)
set.seed(11)
folds <- sample(rep(1:k, length = n))
b.cv.errors <- matrix(NA, k, 11,
                    dimnames = list(NULL, paste(1:11)))

for (j in 1:k) {
  bstep.fit <- regsubsets(quality ~ .,
                          data = wine.data[folds != j, ],
                          nvmax = 11,
                          method = "backward")
  for (i in 1:11) {
    pred.backward <- predict.regsubsets(fstep.fit, wine.data[folds == j, ], id = i)
    b.cv.errors[j, i] <- 
      mean((wine.data$quality[folds == j] - pred.backward)^2)
  }
}

backward.cv.errors <- apply(b.cv.errors, 2, mean)
backward.cv.errors
par(mfrow = c(1,1))
plot(backward.cv.errors, type = "b")

which.min(backward.cv.errors)
backward.mse <- backward.cv.errors[["11"]]
backward.mse

# Regression Tree

# Create regression tree model for quality as response
tree.wine <- tree(quality ~ ., data = wine.data, subset = train.wine)
summary(tree.wine)

# Plot the regression tree
plot(tree.wine)
text(tree.wine, pretty = 0)

tree.pred <- predict(tree.wine, newdate = wine.data[test.wine, ])
tree.mse <- mean((tree.pred - y.test)^2)
tree.mse

# Attempt to prune the tree and get better test results

tree.cv <- cv.tree(tree.wine)
plot(tree.cv$size, tree.cv$dev, type = "b")

prune.wine <- prune.tree(tree.wine, best = 6)

# Plot the pruned regression tree
plot(prune.wine)
text(prune.wine, pretty = 0)

prune.pred <- predict(prune.wine, newdata = wine.data[test.wine, ])
prune.mse <- mean((prune.pred - y.test)^2)
prune.mse

# Bagging

# Create bagging model to predict wine quality
set.seed(90)
bag.wine <- randomForest(quality ~ ., data = wine.data, subset = train.wine,
                         mtry = (ncol(wine.data) - 1), importance = T)
bag.wine

bag.pred <- predict(bag.wine, newdata = wine.data[test.wine, ])
bag.mse <- mean((bag.pred - y.test)^2)
bag.mse

# Random Forest

# Create a random forest model using default m = p/3
set.seed(90)
rf.wine <- randomForest(quality ~ ., data = wine.data[train.wine, ],
                        importance = T)

importance(rf.wine)
varImpPlot(rf.wine)

rf.pred <- predict(rf.wine, newdata = wine.data[test.wine, ])
rf.mse <- mean((rf.pred - y.test)^2)
rf.mse

# Boosting

tunings <- c(0.001, 0.005, 0.01, 0.015, 0.020)
boost.errors <- data.frame(lambda = tunings,
                           training.error = rep(NA, length(tunings)),
                           test.error = rep(NA, length(tunings)))

for (x in 1:length(tunings)) {
  set.seed(91)
  boost.wine <- gbm(quality ~ ., data = wine.data[train.wine, ],
                    distribution = "gaussian", n.trees = 1000,
                    interaction.depth = 4, shrinkage = tunings[x])
  boost.errors[x, "training.error"] <- mean(boost.wine$train.error)
}

for (x in 1:length(tunings)) {
  set.seed(91)
  boost.wine <- gbm(quality ~ ., data = wine.data[train.wine, ],
                    distribution = "gaussian", n.trees = 1000,
                    interaction.depth = 4, shrinkage = tunings[x])
  yhat.boost <- predict(boost.wine, newdata = wine.data[test.wine, ],
                        n.trees = 1000)
  boost.errors[x, "test.error"] <- mean((yhat.boost - y.test)^2)
}

boost.errors

