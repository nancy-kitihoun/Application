## lasso

data<- read.table("D:/M2/SVM/creditcard.csv", header =T ,sep = ",")
attach(data)
ind=sample(2, nrow(data), replace=TRUE ,prob=c(0.7, 0.3))

train=data[ind==1,]
test=data[ind==2,]
library(ROSE)

rose= ROSE(Class~., data=train, N=100000)$data
table(rose$Class)

library(glmnet)
library(caret)
library(tidyverse)

# Dumy code categorical predictor variables
x <- model.matrix(Class~., rose)[,-31]
# Convert the outcome (class) to a numerical variable
y <- rose[,31]


set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

plot(cv.lasso)

cv.lasso$lambda.min

cv.lasso$lambda.1se

coef(cv.lasso, cv.lasso$lambda.min)

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
plot(model)
