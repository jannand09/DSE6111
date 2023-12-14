
# Load Libraries

library(ISLR2)
library(MASS)
library(e1071)
library(class)
library(tree)
library(randomForest)
library(gbm)

# Load data set

full.data <- read.table("student_data.csv", sep=";", header = T)
View(full.data)

student.data <- na.omit(full.data)
student.data$Target[student.data$Target=="Enrolled"] <- "Student"
student.data$Target[student.data$Target=="Graduate"] <- "Student"

length(student.data$Target[student.data$Target == "Dropout"]) # 1421 dropout records

student.data$Target <- as.factor(student.data$Target)


# Split data into test and training set
set.seed(1)
train <- sample(1:nrow(student.data), 0.5 * nrow(student.data))
test <- (-train)

# Create logistic regression model using all predictors

log.student <- glm(Target ~ ., data = student.data, family = binomial)

summary(log.student)

sig_predictors <- c("Application.order", "Course", "Previous.qualification",
                    "Nacionality", "Mother.qualification", "Mother.occupation",
                    "Displaced", "Debtor", "Tuition.fees.up.to.date",
                    "Gender", "Scholarship.holder", "Age.at.enrollment", "International",
                    "Curricular.units.1st.sem.approved", "Curricular.units.1st.sem.grade",
                    "Curricular.units.2nd.sem.credited", "Curricular.units.2nd.sem.enrolled",
                    "Curricular.units.2nd.sem.approved", "Curricular.units.2nd.sem.grade",
                    "Unemployment.rate", "Target")

student.data <- student.data[, sig_predictors]

# Train logistic regression model with significant predictors

glm.student <- glm(Target ~ ., data = student.data, subset = train,
                   family = binomial)

glm.probs <- predict(glm.student, student.data[test, ], type = "response")
glm.pred <- rep("Student", nrow(student.data[test, ]))

attach(student.data)
contrasts(Target)

glm.pred[glm.probs < 0.5] <- "Dropout"
table(glm.pred, student.data$Target[test])
glm.error <- (199 + 75) / (199 + 75 + 516 + 1422)
glm.error

# Linear Discriminant Analysis

lda.student <- lda(Target ~ ., data = student.data, subset = train)
lda.student

lda.pred <- predict(lda.student, student.data[test, ])
lda.class <- lda.pred$class
table(lda.class, student.data$Target[test])
lda.error <- (216 + 60) / (499 + 60 + 216 + 1437)
lda.error

# Quadratic Discriminant Analysis

qda.student <- qda(Target ~ ., data = student.data, subset = train)
qda.student

qda.class <- predict(qda.student, student.data[test, ])$class
table(qda.class, student.data$Target[test])
qda.error <- (207 + 132) / (508 + 132 + 207 + 1365)
qda.error


# Naive Bayes Classifier

nb.student <- naiveBayes(Target ~ ., data = student.data, subset = train)
nb.student

nb.class <- predict(nb.student, student.data[test, ])
table(nb.class, student.data$Target[test])
nb.error <- (201 + 159) / (514 + 159 + 201 + 1338)
nb.error

# K-Nearest Neighbor

length(sig_predictors)

train.X <- as.matrix(student.data[train, sig_predictors[-21]])
test.X <- as.matrix(student.data[test, sig_predictors[-21]])
train.target <- student.data$Target[train]

n.values <- c(1, 3, 5, 10)
knn.errors <- data.frame(n.value = n.values,
                         pred.error = rep(NA, length(n.values)))
for (x in 1:length(n.values)) {
  set.seed(2)
  knn.pred <- knn(train.X, test.X, train.target, k = n.values[x])
  knn.errors[x, "pred.error"] <- mean(knn.pred != student.data$Target[test])
}

knn.errors # k = 5 lowest test error

# Classification Tree Model

tree.student <- tree(Target ~ ., data = student.data, subset = train)
summary(tree.student)

plot(tree.student)
text(tree.student, pretty = 0)

tree.pred <- predict(tree.student, student.data[test, ], type = "class")
table(tree.pred, student.data$Target[test])
tree.error <- mean(tree.pred != student.data$Target[test])
tree.error

# Prune classification tree

set.seed(4)
cv.student <- cv.tree(tree.student, FUN = prune.misclass)
cv.student

plot(cv.student$size, cv.student$dev, type = "b")

pruned.student <- prune.misclass(tree.student, best = 7)
plot(pruned.student)
text(pruned.student, pretty = 0)
summary(pruned.student)

pruned.pred <- predict(pruned.student, student.data[test, ], type = "class")
table(pruned.pred, student.data$Target[test])
prune.error <- mean(pruned.pred != student.data$Target[test])
prune.error

# Bagging

set.seed(5)
bag.student <- randomForest(Target ~ ., data = student.data, subset = train,
                            mtry = length(sig_predictors)-1, importance = T)
bag.student

bag.pred <- predict(bag.student, student.data[test, ], type = "class")
table(bag.pred, student.data$Target[test])
bag.error <- mean(bag.pred != student.data$Target[test])
bag.error

# Random Forest using default sqrt(p) predictors

set.seed(5)
rf.student <- randomForest(Target ~ ., data = student.data, subset = train,
                           importance = T)
rf.pred <- predict(rf.student, newdata = student.data[test, ], type = "class")
rf.error <- mean(rf.pred != student.data$Target[test])
rf.error

# Boosting Model

tunings <- c(0.001, 0.01, 0.2, 0.5, 0.75, 1.0)
boost.results <- data.frame(shrinkage = tunings,
                            test.error = rep(NA, length(tunings)))
student.data$Target <- as.numeric(student.data$Target)
student.data$Target <- apply(student.data, 1, FUN = function(x) x[1] - 1)

for (x in 1:length(tunings)) {
  set.seed(6)
  boost.student <- gbm(Target ~ ., data = student.data[train, ],
                       distribution = "bernoulli", n.trees = 1000,
                       interaction.depth = 7, shrinkage = tunings[x])
  boost.pred <- predict(boost.student, newdata = student.data[test, ], 
                        type = "response", n.trees = 1000)
  boost.results[x, "test.error"] <- mean(boost.pred != student.data$Target[test])
}

boost.results
