###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Rodrigo Samayoa
## Laboratorio No.2
## Catedratico: Lynette Garc?a
###################################################


##-----------------ZONA LIBRERIAS---------------------------
#install.packages("arules")
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

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(tabula)
##------------------FIN ZONA LIBRERIAS------------------


##-----------------ZONA DIRECTORIOS---------------------------
getwd()
setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/LabsDataScience/Laboratorio2")
#setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/Laboratorio1/LabsDataScience/Laboratorio1")
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
#test <-read.csv("test.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#train <-read.csv("train.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#head(test[,1:3])
#head(train[,1:3])




