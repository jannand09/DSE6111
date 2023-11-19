
library(ISLR2)
library(MASS)
library(boot)


boston.data <- Boston

u_medv.fn <- function(data, variable, index) {
  mean(data$variable[index])
}

x <- u_medv.fn(boston.data, medv, 1:100)

y <- boston.data$medv[1:100]
mean(y)


22.53281 - 2*0.3990518
22.53281 + 2*0.3990518

