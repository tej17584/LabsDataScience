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
install.packages("arules")
library(arules)
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

library(rela)
library(psych)
library(FactoMineR)
library(corrplot)
library(factoextra)

##------------------FIN ZONA LIBRERIAS------------------


##-----------------ZONA DIRECTORIOS---------------------------
getwd()
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/LabsDataScience/Laboratorio1")
setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/Laboratorio1/LabsDataScience/Laboratorio1")
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


##-----Reglas de asociasion usando el algoritmo de apriori
datosapr <- train[,c(2,5,18,19,20,21,35,38,39,45,46,47,48,49,50,51,52,53,55,57,62,63,67,68,69,70,71,72,76,77,78)]
View(datosapr)

#Recuento de 0s
sum(datosapr$MSSubClass ==0)
sum(datosapr$LotArea ==0)
sum(datosapr$OverallQual ==0)
sum(datosapr$OverallCond ==0)
sum(datosapr$YearBuilt ==0)
sum(datosapr$YearRemodAdd ==0)
sum(datosapr$BsmtFinSF1 ==0) #467
sum(datosapr$BsmtUnfSF ==0 ) #118
sum(datosapr$TotalBsmtSF==0) # 37
sum(datosapr$X2ndFlrSF ==0) #829
sum(datosapr$LowQualFinSF ==0) #1434
sum(datosapr$GrLivArea ==0)
sum(datosapr$BsmtFullBath==0) #856
sum(datosapr$BsmtHalfBath ==0) #1378
sum(datosapr$HalfBath==0)#913
sum(datosapr$FullBath==0 ) #9
sum(datosapr$BedroomAbvGr ==0) #6
sum(datosapr$KitchenAbvGr==0)#1
sum(datosapr$TotRmsAbvGrd ==0)
sum(datosapr$Fireplaces ==0)#690
sum(datosapr$GarageCars ==0)#81
sum(datosapr$GarageArea==0)#81
sum(datosapr$WoodDeckSF ==0)#761
sum(datosapr$OpenPorchSF ==0)#656
sum(datosapr$EnclosedPorch==0)#1252
sum(datosapr$X3SsnPorch ==0)#1436
sum(datosapr$ScreenPorch ==0)#1344
sum(datosapr$PoolArea==0) #1453
sum(datosapr$MiscVal ==0) #1408
sum(datosapr$MoSold ==0)
sum(datosapr$YrSold ==0)

#sin ceros
sum(datosapr$MSSubClass ==0)
sum(datosapr$LotArea ==0)
sum(datosapr$OverallQual ==0)
sum(datosapr$OverallCond ==0)
sum(datosapr$YearBuilt ==0)
sum(datosapr$YearRemodAdd ==0)
sum(datosapr$GrLivArea ==0)
sum(datosapr$TotRmsAbvGrd ==0)
sum(datosapr$MoSold ==0)
sum(datosapr$YrSold ==0)

#menos de 120 ceros
sum(datosapr$TotalBsmtSF==0) # 37
sum(datosapr$FullBath==0 ) #9
sum(datosapr$BedroomAbvGr ==0) #6
sum(datosapr$KitchenAbvGr==0)#1
sum(datosapr$GarageCars ==0)#81
sum(datosapr$GarageArea==0)#81
sum(datosapr$BsmtUnfSF ==0 ) #118


SinCerosData <- datosapr[,c(1:6,12,30,31)]
View(SinCerosData)
str(SinCerosData)

Datosm120 <- datosapr[c(9,16:18,21,22)]
View(Datosm120)
str(Datosm120)

DatosMenosCeros <- datosapr[c(1:6,9,12,16:18,21,22,30,31)]
str(DatosMenosCeros)


##APRIORI
#prueba con dataset sin datos atipicos
reglas<-apriori(SinCerosData, parameter = list(support = 0.2,
                                           confidence = 0.70,
                                           target = "rules"))
inspect(sort(reglas, by = 'confi')[1:15])

#prueba2
reglas<-apriori(Datosm120, parameter = list(support = 0.2,
                                               confidence = 0.70,
                                               target = "rules"))
inspect(sort(reglas, by = 'lift')[1:10])

#prueba dataset con menos ceros (datos atipicos)
reglas<-apriori(DatosMenosCeros, parameter = list(support = 0.2,
                                               confidence = 0.70,
                                               target = "rules"))
inspect(sort(reglas, by = 'lift')[1:15])



