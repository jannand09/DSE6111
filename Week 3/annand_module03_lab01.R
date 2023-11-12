## Load libraries

library(ISLR2)
library(MASS)
library(e1071)
library(class)

## Question 13

weekly <- Weekly
View(weekly)

### Part A

# Get names of columns in weekly dataset
names(weekly)
# get dimensions of the Weekly dataset
dim(weekly)

# Produce summary statistics for each column
summary(weekly)

# Get correlation matrix for weekly data
weekly_cor <- cor(weekly[, -9])
View(weekly_cor)

attach(weekly)
plot(Volume)

attach(weekly)
plot(Year, Volume)

# The only two variables that show a strong correlation are Volume and Year.
# All other variables are not strongly correlated to each other. The columns that
# represent the percentage returns for a given day all have the same Min and Max
# values. Distributions and means are very similar, as well.

### Part B

# Create logistic regression with Direction as target and all other variables as
# predictors
glm.weekly <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = weekly, family = binomial
)

summary(glm.weekly)

# Lag2 appears to be statistically significant because it has a p-value < 0.05.

### Part C

weekly.probs <- predict(glm.weekly, type = "response")
weekly.pred <- rep("Down", 1089)
weekly.pred[weekly.probs > 0.5] = "Up"

table(weekly.pred, Direction)
mean(weekly.pred == Direction)

# The confusion matrix tells us that our logistic regression model does well at
# predicting when the market will go up; however, it is majorly inaccurate in 
# predicting when the market will go down. Overall, the model correctly predicts
# the movement of the market 56.1% of the time.

### Part D

# Create a set of test data from the weekly dataset that includes observations
# from 2009 to 2010.
train <- (Year < 2009)
weekly.2008 <- weekly[!train, ]
dim(weekly.2008)
Direction.2008 <- Direction[!train]

glm.train <- glm(
  Direction ~ Lag2, data = weekly, family = binomial, subset = train)
test.probs <- predict(glm.train, weekly.2008, type = "response")
test.pred <- rep("Down", 104)
test.pred[test.probs > 0.5] <- "Up"

table(test.pred, Direction.2008)
mean(test.pred == Direction.2008)

# Using only Lag2 as a predictor, the logistic model correctly predicts the direction
# of the market 62.5% of the time on the test data.

### Part E

lda.fit <- lda( Direction ~ Lag2, data = weekly, subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, weekly.2008)
lda.class <- lda.pred$class
table(lda.class, Direction.2008)
mean(lda.class == Direction.2008)

# Linear discriminant analysis correctly predicts the direction of the market
# 62.5% of the time.

### Part F

qda.fit <- qda(Direction ~ Lag2, data = weekly, subset = train)
qda.fit

qda.class <- predict(qda.fit, weekly.2008)$class
table(qda.class, Direction.2008)
mean(qda.class == Direction.2008)

# Quadratic discriminant analysis correctly predicts the direction of the market
# 58.7% of the time; however, it incorrectly predicts when the market goes down
# in every test case.

### Part G

# Create matrix of predictors for train and test data and vector of responses for
# training data
train.X <- as.matrix(weekly[train, ]$Lag2)
test.X <- as.matrix(weekly.2008$Lag2)
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2008)
mean(knn.pred == Direction.2008)

# KNN when k = 1 predicts the direction of the market correctly 50% of the time.

### Part H

nb.fit <- naiveBayes(Direction ~ Lag2, data = weekly, subset = train)
nb.fit

nb.class <- predict(nb.fit, weekly.2008)
table(nb.class, Direction.2008)
mean(nb.class == Direction.2008)

# Naive Bayes correctly predicts the direction of the market
# 58.7% of the time; however, it incorrectly predicts when the market goes down
# in every test case. 

### Part I

results <- data.frame(algorithm = c("Logistic", "LDA", "QDA", "KNN", "Naive Bayes"),
                      accuracy = c(mean(test.pred == Direction.2008),
                                   mean(lda.class == Direction.2008),
                                   mean(qda.class == Direction.2008),
                                   mean(knn.pred == Direction.2008),
                                   mean(nb.class == Direction.2008)))

results

# Logistic regression and LDA provide the best results.

## Question 16

boston <- Boston
