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

library(tabulizer)
library(dplyr)
library(stringr)
##------------------FIN ZONA LIBRERIAS------------------


##-----------------ZONA DIRECTORIOS---------------------------
getwd()
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/LabsDataScience/Laboratorio2")
setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/Laboratorio1/LabsDataScience/Laboratorio2")
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
#test <-read.csv("test.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#train <-read.csv("train.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#head(test[,1:3])
#head(train[,1:3])


# Leer de un PDF
# install.packages("tabulizer")
pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar



View(datosImp)
##Se reemplazan "-" o "0" etc por NA en director
datosImp$GasAviacion[datosImp$GasAviacion=="-"] <- NA





