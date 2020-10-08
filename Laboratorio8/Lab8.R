## Guatemala, marzo del 2019
## Mineria de datos
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Guilermo Sandoval 17577
## PROYECTO Minería datos
###################################################


library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering


#install.packages("klaR")
#install.packages("cba")
library(klaR)
library(cba)

library(dplyr)
library(rpart)
library(tree)
library(rpart.plot)
library(foreign)
library(nortest)
library(caret)
library(corrplot)
library(randomForest)
library(normtest) ###REALIZA 5 PRUEBAS DE NORMALIDAD###
library(nortest) ###REALIZA 10 PRUEBAS DE NORMALIDAD###
library(moments) ###REALIZA 1 PRUEBA DE NORMALIDAD###
#install.packages("C50", dependencies = TRUE)
library(C50)


#######_______________________DIRECTORIOS Y LECTURA DE DATOS__________________#############
getwd()
setwd("C:/Users/josea/Desktop/Universidad/2020/MineriaDatos/Proyecto")

DB2014 = read.spss("C:\\Users\\josea\\Desktop\\Universidad\\2020\\MineriaDatos\\Proyecto\\2014Data.sav", to.data.frame=TRUE)
DB2015 = read.csv("2015Data.csv",stringsAsFactors = FALSE, na.strings = TRUE)
DB2016 = read.spss("C:\\Users\\josea\\Desktop\\Universidad\\2020\\MineriaDatos\\Proyecto\\2016Data.sav", to.data.frame=TRUE)
DB2017 = read.spss("C:\\Users\\josea\\Desktop\\Universidad\\2020\\MineriaDatos\\Proyecto\\2017Data.sav", to.data.frame=TRUE)
DB2018 = read.spss("C:\\Users\\josea\\Desktop\\Universidad\\2020\\MineriaDatos\\Proyecto\\2018Data.sav", to.data.frame=TRUE)