###Analyse descriptive des variables


## Importation des données

data=read.table("C:/Users/Hp/Desktop/creditcard.csv",header=T,sep=",")

# On vérifie que c'est un data frame si non on le transforme.
is.data.frame(data)
head(data)
length(data)
attach(data)
summary(data)


#Echantillionage de nos donnée

ind=sample(2, nrow(data), replace=TRUE ,prob=c(0.7, 0.3))

train=data[ind==1,]
test=data[ind==2,]
table(train$Class)

install.packages("ROSE")
library(ROSE)

#Faire le rose pour le sampling

rose= ROSE(Class~., data=train, N=2000)$data
table(rose$Class)

##On travaille sur les données rose
## Selection des variables significatives

# Estimation des paramètres du modèle.

dd.fit=lm(Class~.,rose)
summary(dd.fit)       
                      
                       # Toutes les variables sont significatives sans : Time,V1,V2
                       # V7,V16,V22,V23,V24 et V27.
                        

## On cherche le meilleur modèle

install.packages("leaps")
library(leaps)
choix=regsubsets(Class~.,rose)
summary(choix)
plot(choix)        
                      # Les variables explicatives qui minimisent le BIC (en noir au sommet)sont:
                      la cste, V4,V6,V11,V12,V13,V14,V18 et Amount.


## On compare les deux modèles (Best Modeling) (On travaille qu'avec les variables 
## qu'on a trouvé avec regsubsets

best=lm(Class~V4+V6+V11+V12+V13+V14+V18+Amount,rose)
summary(best)        
                      # ttes les variables sont significatives
                      # En utilisant le critère BIC on a une regression qui utilise 
                      # plus de variables significatives.





## Estimer le modèle de regression logistique

glm.fit=glm(Class~.,data=rose,family=binomial)
summary(glm.fit)

##Prédiction

glm.probs=predict(glm.fit,test, type="response")
glm.pred=rep(0,nrow(test))
glm.pred[glm.probs>.5]=1


#Calculer la matrice de confusion	

table(glm.pred,test$Class)
tx_erreur=mean(glm.pred!=test$Class)
tx_erreur                            ## le taux d'erreur sur l'échantillon test est 1,6%


# la fraction des bonnes classifications.
tx_exact=mean(glm.pred==test$Class)
tx_exact                             
                                     ## le taux exact est 98,3%

## Maintenant, On refait le glm juste avec  les prédicteurs significatifs 


glm.fit1=glm(Class~V3+V4+V5+V6+V9+V10+V11+V12+V13+V14+V16+V17+V20+V21+V25+V28+Amount,data=rose,family=binomial)
summary(glm.fit1)

##Prédiction

glm.probs=predict(glm.fit1,test, type="response")
contrasts(data$Class)
glm.pred=rep(0,nrow(test))
glm.pred[glm.probs>.5]=1


#Calculer la matrice de confusion	

table(glm.pred,test$Class)
tx_erreur=mean(glm.pred!=test$Class)
tx_erreur                            ## le taux d'erreur est 1,5%


# la fraction des bonnes classifications.
tx_exact=mean(glm.pred==test$Class)
tx_exact                            ## le taux exact est 98,4%





###Faire le under pour le sampling( on refait la mme chose mais cette fois ci avec under)

under=ovun.sample(Class~., data=train, method="under", N=686)$data
table(under$Class)

## Estimer le modèle de regression logistique


glm.fit2=glm(Class~.,data=under,family=binomial)
summary(glm.fit2)                                 

##Prédiction

glm.probs=predict(glm.fit2,test, type="response")
glm.pred=rep(0,nrow(test))
glm.pred[glm.probs>.5]=1


#Calculer la matrice de confusion	

table(glm.pred,test$Class)
tx_erreur=mean(glm.pred!=test$Class)
tx_erreur                            ## le taux d'erreur sur l'échantillon test est 4,05%


# la fraction des bonnes classifications.
tx_exact=mean(glm.pred==test$Class)
tx_exact                             
                                     ## le taux exact est 95,9%

## Maintenant, On refait le glm juste avec  les prédicteurs significatifs 


glm.fit2=glm(Class~V3+V4+V5+V6+V9+V10+V11+V12+V13+V14+V16+V17+V20+V21+V25+V28+Amount,data=under,family=binomial)
summary(glm.fit2)

##Prédiction

glm.probs=predict(glm.fit2,test, type="response")
contrasts(data$Class)
glm.pred=rep(0,nrow(test))
glm.pred[glm.probs>.5]=1


#Calculer la matrice de confusion	

table(glm.pred,test$Class)
tx_erreur=mean(glm.pred!=test$Class)
tx_erreur                            ## le taux d'erreur est 4,16%


# la fraction des bonnes classifications.
tx_exact=mean(glm.pred==test$Class)
tx_exact                            ## le taux exact est 95,8%













