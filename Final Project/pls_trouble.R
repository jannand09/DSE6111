# Load libraries

library(ISLR2)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(boot)
library(tree)
library(BART)



# Import wine quality data

white.quality <- read.csv("winequality-white.csv", sep=";", na.strings = "?", stringsAsFactors = T)
View(white.quality)

# Create training data
set.seed(10)
train.white <- sample(1:nrow(white.quality), 0.5 * nrow(white.quality))
test.white <- (-train.white)

# Create matrix of x, the predictors, and vector of y, the response
x <- model.matrix(quality ~ ., white.quality)[, -1]
y <- white.quality$quality

y.test <- y[test.white]

# Partial Least Squares

# Create PLS model on the white wine quality data
set.seed(2)
pls.fit <- plsr(quality ~ ., data = white.quality, subset = train.white, scale = T,
                validation = "CV")
summary(pls.fit)

pls.fit$fitted.values

# Plot MSEP over the number of components
validationplot(pls.fit, val.type = "MSEP")
axis(side=1, at=seq(1, 20, by=1))

# Predict quality of the wine using PLS
pls.pred <- predict(pls.fit, newdata = x[test.white, ], ncomp=3)
pls.mse <- mean((pls.pred - y.test)^2)
pls.mse