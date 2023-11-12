# Load library
library(ISLR2)

# QUESTION 8a: Import data set
college <- read.csv("College.csv", na.strings = "?", stringsAsFactors = T)

# QUESTION 8b: Row Names
rownames(college) <- college[, 1]
View(college)

college <- college[, -1]
View(college)

# QUESTION 8c: Exploring the College data

# Part i
summary(college)

# Part ii
pairs(college[, 2:11])

# Part iii
boxplot(college$Outstate ~ college$Private, main = "Boxplot of Out-of-State Tuition by Private status")

# Part iv
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

summary(college)
boxplot(college$Outstate ~ college$Elite, main = "Boxplot of Out-of-State Tuition by Elite status")

# Part v
par(mfrow = c(2,2))

hist(college$Expend)
hist(college$Terminal)
hist(college$S.F.Ratio)
hist(college$perc.alumni)

par(mfrow = c(2,2))

hist(college$Expend)
hist(college$Expend, breaks = 5)
hist(college$Expend, breaks = 20)
hist(college$Expend, breaks = 100)

# Part vi
boxplot(college$Expend ~ college$Elite, main = "Boxplot of Instructional Expenditure by Elite status")
boxplot(college$S.F.Ratio ~ college$Elite, main = "Boxplot of Student/Faculty Ratio Expenditure by Elite status")

college$Acc.Rate <- college$Accept / college$Apps
boxplot(college$Acc.Rate ~ college$Elite, main = "Acceptance Rate by Elite Status")

# QUESTION 9a

auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)
auto <- na.omit(auto)

View(auto)

# QUESTION 9b
quantitative_auto = c('mpg', 'displacement', 'horsepower', 'weight', 'acceleration')
for (x in quantitative_auto) {
  print(range(auto[, x]))
}

# QUESTION 9c
sapply(auto[, quantitative_auto], function(x) c("Mean:" = mean(x, na.rm = T), "Std Dev:" = sd(x)))

# QUESTION 9d
rmv_auto <- auto[-c(10:85),]
sapply(rmv_auto[,quantitative_auto], function(x) c("Mean:" = mean(x, na.rm = T), 
                               "Std Dev:" = sd(x),
                               "Range" = range(x))
       )

#QUESTION 9e
pairs(auto[,quantitative_auto])

# QUESTION 9f
for (predictor in c('displacement', 'horsepower', 'weight', 'acceleration')) {
  attach(auto)
  plot(x=predictor, y=mpg)
  print(paste("Correlation coeff between mpg and ",x, "is ", cor(auto[x], auto$mpg)))
}
attach(auto)
plot(displacement, mpg)

# QUESTION 10a
dim(Boston)
View(Boston)

# QUESTION 10b
pairs(Boston[,-4])

# QUESTION 10c
for (x in c('lstat', 'medv')) {
  print(paste("Correlation coeff between crim and ",x, "is ", cor(Boston[x], Boston$crim)))
}

# QUESTION 10d
boston <- data.frame(Boston)

for (x in c('crim', 'tax', 'ptratio')) {
  print(range(Boston[, x]))
}

print(median(boston$crim))
print(median(boston$tax))
print(median(boston$ptratio))

# QUESTION 10e
length(which(boston$chas == 1))

# QUESTION 10f
median(boston$ptratio)

# QUESTION 10g
min_med <- which.min(boston$medv)
print(min_med)

min_med_df <- boston[min_med,]
View(min_med_df)

for (x in colnames(boston)) {
  print(range(boston[, x]))
}

# QUESTION 10h
length(which(boston$rm > 7))
length(which(boston$rm > 8))

eight_rm <- boston[boston$rm > 8,]
summary(boston[boston$rm > 8,])
summary(boston)
