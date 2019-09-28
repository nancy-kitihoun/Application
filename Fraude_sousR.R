

###J'ai fait deux échnatillons d'apprentissage sur lequels vous pouvez travailler
#le premier c'est under
#le deuxième c'est rose


data= read.table("D:/Cours M2 ESA/SVM/Projet/creditcard.csv", header=T, sep=",")
str(data)
data$class=as.factor(data$Class)
table(data$class)
prop.table(table(data$class))
barplot(prop.table(table(data$class)))

ind=sample(2, nrow(data), replace=TRUE ,prob=c(0.7, 0.3))

train=data[ind==1,]
test=data[ind==2,]

table(train$class)

install.packages("ROSE")
library(ROSE)


#faire le both pour le sampling

both=ovun.sample(class~., data=train, method="both", N=1000)$data
table(both$class)

library(randomForest)
library(caret)
install.packages("e1071")
library(e1071)

rfboth=randomForest(class~., data=both)
confusionMatrix(predict(rfboth, test), test$class, positive="1")

#under sampling

under=ovun.sample(class~., data=train, method="under", N=686)$data
table(under$class)
rfunder=randomForest(class~., data=under)
confusionMatrix(predict(rfunder, test), test$class, positive="1")


#faire du gradient boosting
library(gbm)

boost=gbm(class~., data=both, distribution="gaussian", n.trees= 500, interaction=4)
summary(boost)

#faire la matrice de confusion pour le boosting, petit problème, revoir
pred.boost=predict(boost, newdata=test, n.trees=500)
mean((pred.boost-test$class)^2)

#des données synthétiques

rose= ROSE(class~., data=train, N=2000)$data
table(rose$class)

#matice de confusion sur ces données après un random forest

rfrose= randomForest(class~., data=rose)

confusionMatrix(predict(rfrose, test), test$class, positive="1")


###essayer de faire un svm sur l'échantillon train avec rose

try=svm(class~., data= rose, kernel="linear", scale=F)















