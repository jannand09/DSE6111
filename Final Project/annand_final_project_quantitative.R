
# Load libraries

library(ISLR2)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(boot)


# Load data set

# Code provided by https://archive.ics.uci.edu/dataset/320/student+performance
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

# Use student data from d2 which is Portuguese class data
student.data <- d2

# Import liver disorder data

liver <- read.table("bupa.data", sep=",", encoding = "UTF-8")
