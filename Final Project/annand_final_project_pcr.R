
library(ISLR2)
library(MASS)
library(pls)

set.seed(1)
train <- sample(1:nrow(Credit), nrow(Credit) / 2)
test <- (-train)

set.seed(2)

pcr.fit <- pcr(Balance ~ ., data = Credit, subset = train, scale = T,
               validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
axis(side=1, at=seq(1,20,by=1))