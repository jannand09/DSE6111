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
summary(boston)
cor_boston <- cor(boston)
View(cor_boston)

# Create qualitative variable that indicates if crim value is above or below median
crim_median <- median(boston$crim)
boston$mcrim <- apply(boston, 1, FUN = function(x) if(x[1] > crim_median) "above"
                      else "below")
boston$mcrim <- as.factor(boston$mcrim)

# Create training and test sets

smp_size <- floor(0.75 * nrow(boston))

set.seed(2)
train.ind <- sample(seq_len(nrow(boston)), replace = F, size = smp_size)

train.bos <- boston[train.ind, ]
test.bos <- boston[-train.ind, ]

mcrim.test <- as.factor(boston$mcrim[-train.ind])

# Logistic regression using rad, tax, lstat, indus, black, and medv

glm.boston <- glm(mcrim ~ rad + tax + lstat + indus + black + medv,
                  data = boston, family = binomial, subset = train.ind)

summary(glm.boston)

blog.probs <- predict(glm.boston, test.bos, type = "response")
contrasts(boston$mcrim)

blog.pred <- rep("below", length(test.bos))
blog.pred[blog.probs < 0.5] <- "above"

table(blog.pred, mcrim.test)

(53 + 6) / (53 + 8 + 7 + 6)

# Logistic regression using rad, lstat, indus, black, and medv

glm.boston2 <- glm(mcrim ~ rad + lstat + indus + black + medv,
                  data = boston, family = binomial, subset = train.ind)

summary(glm.boston2)

blog2.probs <- predict(glm.boston2, test.bos, type = "response")
blog2.pred <- rep("below", length(test.bos))
blog2.pred[blog.probs < 0.5] <- "above"

table(blog2.pred, mcrim.test)

# Removing the least significant variable does not improve the results of the logistic
# regression

# LDA using rad, lstat, indus, black, and medv

lda.boston <- lda(mcrim ~ rad + lstat + indus + black + medv,
                  data = boston, subset = train.ind)

lda_bos.pred <- predict(lda.boston, test.bos)
lda_bos.class <- lda_bos.pred$class
table(lda_bos.class, mcrim.test)

(46 + 55) / (46 + 55 + 22 + 4)

# LDA using just lstat and medv

lda.boston2 <- lda(mcrim ~ lstat + medv,
                  data = boston, subset = train.ind)

lda_bos2.pred <- predict(lda.boston2, test.bos)
lda_bos2.class <- lda_bos2.pred$class
table(lda_bos2.class, mcrim.test)

(39 + 46) / (39 + 46 + 29 + 13)

# Naive Bayes using rad, lstat, indus, black, and medv

nb_bos.fit <- naiveBayes(mcrim ~ rad + lstat + indus + black + medv,
                         data = boston, subset = train.ind)

nb_bos.class <- predict(nb_bos.fit, test.bos)
table(nb_bos.class, mcrim.test)
(42 + 56) / (42 + 56 + 26 + 3)

# Naive Bayes using just lstat and medv

nb_bos2.fit <- naiveBayes(mcrim ~ lstat + medv,
                         data = boston, subset = train.ind)

nb_bos2.class <- predict(nb_bos2.fit, test.bos)
table(nb_bos2.class, mcrim.test)

(41 + 48) / (41 + 48 + 27 + 11)

# KNN using rad, lstat, indus, black, and medv

bos.train <- cbind(boston$rad, boston$lstat, boston$indus, boston$black, boston$medv)[train.ind, ]
bos.test <- cbind(boston$rad, boston$lstat, boston$indus, boston$black, boston$medv)[-train.ind, ]
bos.mcrim <- boston$mcrim[train.ind]

set.seed(3)
knn_bos.pred <- knn(bos.train, bos.test, bos.mcrim, k = 1)
table(knn_bos.pred, mcrim.test)
(58 + 47) / (58 + 47 + 10 + 12)

# KNN using rad, lstat, black, and medv k = 3

set.seed(3)
knn_bos2.pred <- knn(bos.train, bos.test, bos.mcrim, k = 3)
table(knn_bos2.pred, mcrim.test)
(56 + 49) / (56 + 49 + 12 + 10)


# KNN using lstat and medv

bos2.train <- cbind(boston$lstat, boston$medv)[train.ind, ]
bos2.test <- cbind(boston$lstat, boston$medv)[-train.ind, ]

set.seed(3)
knn_bos3.pred <- knn(bos2.train, bos2.test, bos.mcrim, k = 3)
table(knn_bos3.pred, mcrim.test)

(45 + 40) / (45 + 40 + 23 + 19)
