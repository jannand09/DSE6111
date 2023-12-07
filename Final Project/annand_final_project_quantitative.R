
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
train.white <- sample(nrow(white.quality), 0.8 * nrow(white.quality))

# Train the multiple linear regression model with selected predictors
lm.train <- lm(quality ~ volatile.acidity + residual.sugar + free.sulfur.dioxide
               + density + pH + sulphates + alcohol, data = white.quality,
               subset = train.white)
summary(lm.train)

lm.predict <- predict(lm.train, white.quality[-train.white, ])
lm.mse <- mean((lm.predict - white.quality[-train.white, ]$quality)^2)
lm.mse

# Ridge Regression

# Create matrix of x, the predictors, and vector of y, the response
x <- model.matrix(quality ~ ., white.quality)[, -12]
y <- white.quality$quality

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
ridge.pred <- predict(ridge.mod, s = bestlam.ridge, newx = x[-train.white, ])
ridge.mse <- mean((ridge.pred - y[-train.white])^2)
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
lasso.pred <- predict(lasso.mod, s = bestlam.lasso, newx = x[-train.white, ])
lasso.mse <- mean((lasso.pred - y[-train.white])^2)
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
pls.pred <- predict(pls.fit, x[-train.white, ], ncomp=3)
pls.mse <- mean((pls.pred - y[-train.white])^2)
pls.mse
