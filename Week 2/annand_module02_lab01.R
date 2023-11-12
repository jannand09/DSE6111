# Import libraries

library(ISLR2)
library(MASS)

# Question 9

auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)

## Part A

pairs(auto)

## Part B

auto_cor <- cor(auto[,-9], use="complete.obs")
print(auto_cor)

## Part C

lm.auto <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year,
              data = auto)

summary(lm.auto)

### Subpart i
# According to the F-statistic being sufficiently larger than 1, there is a 
# relationship between the response and the predictors.

### Subpart ii
# Weight and Year are statistically significant predictors of mpg because the p-values
# for their coefficient estimates are sufficiently small.

### Subpart iii
# Coefficient for the year variable indicates that with each year younger the car was released
# the car gets 0.7534 miles more per gallon.

# Part D

#Residual plots for fitted multiple linear regression
res <- resid(lm.auto)
plot(fitted(lm.auto), res)
abline(0,0)

plot(predict(lm.auto), residuals(lm.auto))
abline(0,0)
plot(predict(lm.auto), rstudent(lm.auto))
abline(0,0)

#Leverage plot for linear regression
plot(hatvalues(lm.auto))
which.max(hatvalues(lm.auto))

# The residuals plot does not show any unusually large outliers; however, it does
# show some non-linear shape in the data. The leverage lot shows at least one observation
# with unusually high leverage.

## Part E
# Interaction between weight and various variables
summary(lm(mpg ~ weight * year, data = auto))
summary(lm(mpg ~ weight * acceleration, data = auto))
summary(lm(mpg ~ weight * cylinders, data = auto))

# Interaction between year and various variables
summary(lm(mpg ~ year * displacement, data = auto))
summary(lm(mpg ~ weight * acceleration, data = auto))
summary(lm(mpg ~ weight * cylinders, data = auto))

# Strong interaction between weight and year as well as those two predictors with
# other variables

## Part F

lm.log <- lm(mpg ~ log10(cylinders) + log10(displacement) + log10(horsepower) + 
               log10(weight) + log10(acceleration) + log10(year),
             data = auto)

summary(lm.log)

lm.sqrt <- lm(mpg ~ sqrt(cylinders) + sqrt(displacement) + sqrt(horsepower) + 
               sqrt(weight) + sqrt(acceleration) + sqrt(year), data = auto)

summary(lm.sqrt)

# auto.square <- auto
# auto.square[,2:8] <- auto.square[,2:8]**2
# 
# lm.square <- lm(mpg ~ cylinders + displacement + horsepower + weight 
#                 + acceleration + year, data = auto.square)

lm.square <- lm(mpg ~ I(cylinders^2) + I(displacement^2) + I(horsepower^2) 
                + I(weight^2) + I(acceleration^2) + I(year^2), data = auto)

summary(lm.square)


# Question 10

carseats <- Carseats

## Part A

lm.carseat <- lm(Sales ~ Price + Urban + US, data = carseats)
summary(lm.carseat)

## Part B
# When Price increases by one unit and Urban = US = No, sales decrease by 0.54459.
# When Urban = Yes, Sales will be 0.021916 units less than if Urban = No.
# When US = Yes, Sales will be 1.200573 units more than if US = No.

## Part C

## Part D
# We can reject the null hypothesis for Price and US.

## Part E

lm.carseat1 <- update(lm.carseat, ~ . - Urban)
summary(lm.carseat1)


## Part F
# Both models from parts a and e fit the data well based on the F-statistics and
# p-values. 

anova(lm.carseat, lm.carseat1)

# Anova shows that there is no statistical difference between the two models from
# parts a and e.

## Part G

confint(lm.carseat1)

## Part H

plot(predict(lm.carseat1), residuals(lm.carseat1))
abline(0,0)
plot(predict(lm.carseat1), rstudent(lm.carseat1))
abline(0,0)

plot(hatvalues(lm.carseat1))
which.max(hatvalues(lm.carseat1))

# No evidence of outliers, but at least one observation with unusually high leverage.


# Question 14

## Part A

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# regression coefficients are 2 and 0.3

## Part B

cor(x1, x2)
plot(x2, x1)

## Part C

lm.collin <- lm(y ~ x1 + x2)

summary(lm.collin)

# B0 is 2.1305, B1 is 1.4396, and B2 is 1.0097. We can reject the null hypothesis
# that B1 = 0 because the p-value for the estimate is less than 0.05. We cannot 
# reject the null hypothesis that B2 = 0 because the p-value is greater than 0.05.
# The F-statistic for the multiple linear regression is >1 and its corresponding
# p-value is much <0.05.

## Part D

lm.collin1 <- lm(y ~ x1)
summary(lm.collin1)

# We can reject the null hypothesis that B1 = 0 because the p-value for the estimate
# is less than 0.05.

## Part E

lm.collin2 <- lm(y ~ x2)
summary(lm.collin2)

# We can reject the null hypothesis that B1 = 0 because the p-value is <0.05.

## Part F
# These results do contradict each other as we fail to reject the null hypothesis
# for the B2 parameter estimate in part c but reject the null hypothesis for the 
# same parameter estimate in part e. We can tell from part b, though, that x1
# and x2 are collinear, meaning the power fo the hypothesis tests, and thus the
# probability of correctly detecting a non-zero coefficient, is reduced.

## Part F

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.collinf <- lm(y ~ x1 + x2)
summary(lm.collinf)
plot(predict(lm.collinf), residuals(lm.collinf))
abline(0,0)
plot(hatvalues(lm.collinf))

# We now fail to reject the null hypothesis that B1 = 0 and we may reject the null
# hypothesis that B2 = 0. The observation does not appear to be an outlier but
# does have high leverage.

lm.collinf1 <- lm(y ~ x1)
summary(lm.collinf1)
plot(predict(lm.collinf1), residuals(lm.collinf1))
abline(0,0)
plot(hatvalues(lm.collinf1))

# We reject the null hypothesis for the estimate of B1, which is the same as in 
# part d. The residuals have constant variability and there are no obvious outliers.
# No observation stands out with an unusually high leverage.

lm.collinf2 <- lm(y ~ x2)
summary(lm.collinf2)
plot(predict(lm.collinf2), residuals(lm.collinf2))
abline(0,0)
plot(hatvalues(lm.collinf2))

# Similar to part e, we reject the null hypothesis that B1 = 0. Residual plot
# does show any particularly large outliers. There is a high-leverage point.