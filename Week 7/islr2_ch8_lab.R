library(ISLR2)
library(tree)
library(MASS)

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)

boston.data <- Boston
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")