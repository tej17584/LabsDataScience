###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Rodrigo Samayoa
## Laboratorio No.1
## Catedratico: Lynette Garc?a
###################################################


##-----------------ZONA LIBRERIAS---------------------------
library(dplyr)
library(rpart)
library(tree)
library(rpart.plot)
library(dummies)
library(nortest)
library(caret)
library(corrplot)
library(e1071)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(stringi)
library(stringr)

#install.packages("rela")
#install.packages("psych")
#install.packages("FactoMineR")
#install.packages("corrplot")
library(rela)
library(psych)
library(FactoMineR)
library(corrplot)
library(factoextra)
##------------------FIN ZONA LIBRERIAS------------------


##-----------------ZONA DIRECTORIOS---------------------------
getwd()
setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/LabsDataScience/Laboratorio1")
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
test <-read.csv("test.csv",stringsAsFactors = FALSE, na.strings = TRUE)
train <-read.csv("train.csv",stringsAsFactors = FALSE, na.strings = TRUE)
head(test[,1:3])
head(train[,1:3])


#--------------------------------ANALISIS VARIABLES PRINCIPALES-----------------------
#variables numericas
# train[,c(2,5,18,19,20,21,35,38,39,44,45,46,47,48,49,50,51,52,53,55,57,62,63,67,68,69,70,71,72,76,77,78)]
pafDatos<-paf(as.matrix(train[,c(2,5,18,19,20,21,35,38,39,45,46,47,48,49,50,51,52,53,55,57,62,63,67,68,69,70,71,72,76,77,78)]))

datosNumericos<-train[,c(2,5,18,19,20,21,35,38,39,45,46,47,48,49,50,51,52,53,55,57,62,63,67,68,69,70,71,72,76,77,78)]
pafDatos$KMO #0.74 La adecuacion de la muestra
pafDatos$Bartlett #21612 Mientras mas alto sea mejor
summary(pafDatos)

#Pero hay que ver el nivel de significación de la prueba
cortest.bartlett(datosNumericos[,-1])


#se muestra la matriz de correlación
cor(datosNumericos[,-1],use = "pairwise.complete.obs")
#normalizamos
compPrinc<-prcomp(datosNumericos, scale = T)
#summary de los componentes
summary(compPrinc)

#Se obtiene el scree plot de las componentes principales.
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))
#Representaci�n de las variables en cada componente
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
var<-get_pca_var(compPrinc)
corrplot(var$cos2, is.corr = F)

#--------------------------------FIN ANALISIS VARIABLES PRINCIPALES-----------------------

