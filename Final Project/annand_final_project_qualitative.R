
# Load Libraries

library(e1071)
library(class)

# Load data set

full.data <- read.table("student_data.csv", sep=";", header = T, stringsAsFactors = T)
View(full.data)

student.data <- na.omit(full.data)

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


