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

