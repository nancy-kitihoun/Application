
#===================================== I DESCRIPTION DES DONNEES========================================
#***************************************I-1 Importation des donnéées 

data<- read.table("D:/M2/SVM/creditcard.csv", header =T ,sep = ",")

#--------------------------------- une vision des données

attach(data)
head(data)
tail(data) # les 6 derniéres individus
dim(data)   # les dimension de notre base de données
names(data)  # le nom de nos variables
str(data)    # un recapitulatif de nos variables tout en donnant leurs carractéristiques

#----------------------------------- statistique de  base 
summary(data)
cor(data[,-31])

#------------------------------------- pour aller un peu plus loin

install.packages("funModeling")
library(funModeling)

#----------------------------------------la fonction df_status

#le nombre de valeurs nulles (q_zeros) et son pourcentage (p_zeros)
#le nombre de valeurs manquantes (q_na) et son pourcentage (p_na)
#le nombre de valeur infinies (q_inf) et son pourcentage (p_inf)
#le type des variables : facteur, character, numeric, entier, etc. - le nombre de valeurs uniques.

df_status(data)

profiling_num(data) 
# pareil que summary mais etc. ainsi les valeurs de kurtosis et de skewness pour juger de la présence d'outlier et de la symétrie de donnés.

#------------------------------les graphes

plot_num(data[,-31])

boxplot(data[,-31],
        col = c("pink", "red"),                 #Pour la couleur
        main = paste("Boxplot"),     #Pour le titre
        ylab = "Quantiles")              #Pour le titre de l'axe des ordonnées

#------------------------------- pour les variables categorielles

freq(data[,31])
   

 
###-------------------------Echantillionage de nos donnée
#---------------------------le premier c'est under
#---------------------------le deuxième c'est rose

set.seed(1234)

ind=sample(2, nrow(data), replace=TRUE ,prob=c(0.7, 0.3))

train=data[ind==1,]
test=data[ind==2,]



install.packages("ROSE")
library(ROSE)

#faire le rose pour le sampling

rose= ROSE(Class~., data=train, N=10000)$data
table(rose$Class)

#faire le both pour le sampling
both=ovun.sample(Class~., data=train, method="both", N=10000)$data  # sachant qu'on peut aller jusqu'au 2/3 de nos data
table(both$Class)

#under sampling

under=ovun.sample(Class~., data=train, method="under", N=10000)$data
table(under$Class)

 
#========================================II Modélisation du svm avec training egale à rose et under 
#------------------------------------ intallation du packages lié au svm
install.packages("e1071")
library(e1071)
library(parallelSVM) # permet d'effectuer le svm mais avec une grande base de données
 
#------------------------Avec les données Rose 

dat.svm=parallelSVM(Class~., data=rose,  kernel="linear",scale=F, type= 'C',cost=10^(-1:6),gamma =  1^(-1:1))
dat.svm=parallelSVM(Class~., data=rose,  kernel="linear",scale=F, type= 'C')  # tout les 2 marche pour


svm.pred=predict(dat.svm,test[,-31])
table(svm.pred,test$Class)
mean(svm.pred==test$Class)


svm.pred=as.factor(svm.pred)
tes.class=as.factor(test$Class)
confusionMatrix(svm.pred,tes.lass)
#------------------------Avec les données Under (ici donne les meilleurs résultats)
   
   
dat.svm=parallelSVM(Class~., data=under,  kernel="linear",scale=F, type= 'C',cost=10^(-1:6),gamma =  1^(-1:1))
dat.svm=parallelSVM(Class~., data=under,  kernel="linear",scale=F, type= 'C')
svm.pred=predict(dat.svm,test[,-31])

table(svm.pred,test$Class)
mean(svm.pred==test$Class)

svm.pred=as.factor(svm.pred)
tes.class=as.factor(test$Class)
confusionMatrix(svm.pred,tes.lass)
  
   
 ###================================== Autres méthodes de classification============================================
                           ## ***************I. méthode 1: abre de classification**************************
   install.packages("tree")
   install.packages("caret")
   install.packages("ggplot2")
   install.packages("lattice")
   library(ggplot2)
   library(lattice)
   library(caret)
   library(tree)

   
  
tree.class=tree(as.factor(Class)~., under)
summary(tree.class)  
tree.class
   
   
#----------- graph de l'abre
   
     plot(tree.class)
     text(tree.class, pretty=0)
 #----------- prediction
     
   tree.pred=predict(tree.class,test, type = "class")
   table(tree.pred,test$Class)
   mean(tree.pred==test$Class)
   
   tree.pred=as.factor(tree.pred)
   test.class=as.factor(test$Class)
   
   confusionMatrix(tree.pred,test.class,positive="1")

#----------------Matrix de confusion et courbe de roc
   
  

#------------------Classification avec  élageage 
   set.seed(3)
   cv.da=cv.tree(tree.class,FUN=prune.misclass)
   names(cv.da)
   attach(cv.da)
      
#------------- representation du taux d'erreur en fonction de la taille k
#-------------- dev donne le taux erreur
  
   par(mar = rep(2, 4))
   plot(size,dev,type="b")
   plot(k,dev, type="b")
   
#--------------- abre de classification
   
     prune.cla=prune.misclass(tree.class,best=3)
     plot(prune.cla)
     text(prune.cla, pretty=0)
   
#--------------- quelle est la performance de cet arbre à 3 noeuds termineaux sur l'ensemblede test?
     
tree.pred=predict(prune.cla,test,type="class")
table(tree.pred,test$Class)
     
tree.pred=as.factor(tree.pred)
test.class=as.factor(test$Class)
confusionMatrix(tree.pred,test.class)

     
#************************************ II.éme Méthode: Bagging et Random Forests *******************
#------------------ Rappelons que le bagging est un cas particulier du random forests
#-------------------
     
install.packages("randomForest")   
library(randomForest)




#==============================================III.modéle 3: l'analyse  discriminate linaire :LDA

install.packages("MASS")
library(MASS)


lda.class=lda(as.factor(Class)~. , data=under)
lda.class

lad_pred.fraud=predict(lda.class,test)
lda.fraud=lad_pred.fraud$class

table(lda.fraud,test$Class)
mean(lda.fraud==test$Class)

lda.fraud=as.factor((lda.fraud))
test.class=as.factor(test$Class)
confusionMatrix(lda.fraud,test.class, positive = "1")
#===========================================Iv Lamya ta partie

#============================================= comparaison entre les différentes méthodes avec une courbe  ou autre chose
# cette partie  ne marche pas encore
lidt_data=data.frame(class=test$Class,TREE=tree.pred)

lift_obj <- lift(as.factor(class) ~ TREE, data = lidt_data)
xyplot(lift_obj)

