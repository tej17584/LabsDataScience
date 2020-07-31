###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Rodrigo Samayoa
## Laboratorio No.2
## Catedratico: Lynette Garc?a
###################################################

#uninstall.packages("stats")
#install.packages("stats")

##-----------------ZONA LIBRERIAS------------------------------------------------------------------
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
library(zoo)
library(xts)
library(lubridate)
library(prophet)

#install.packages("tidyverse")
library(tidyverse)

##------------------FIN ZONA LIBRERIAS-------------------------------------------------------------------


##-----------------ZONA DIRECTORIOS------------------------------------------------------------------------
getwd()
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/LabsDataScience/Laboratorio2")
setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/Laboratorio1/LabsDataScience/Laboratorio2")
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
#test <-read.csv("test.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#train <-read.csv("train.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#head(test[,1:3])
#head(train[,1:3])


# LEER PDF ON DATOS ------------------------------------------------------------------------------------------------------------------------------------------------------------
# install.packages("tabulizer")
pages<-extract_tables("C01-Importaci髇-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar

View(datosImp)

###LIMPIEZA DE LOS DATOS-------------------------------------------------------------------------------------------------------------------------------------------------------

###Se reemplazan "-" por NAs
datosImp$GasAviacion[datosImp$GasAviacion=="-"] <- NA
datosImp$rTurboJet[datosImp$rTurboJet=="-"] <- NA
datosImp$DieselLS[datosImp$DieselLS=="-"] <- NA
datosImp$DieselULS[datosImp$DieselULS=="-"] <- NA
datosImp$PetCoke[datosImp$PetCoke=="-"] <- NA
datosImp$AceitesLub[datosImp$AceitesLub=="-"] <- NA
datosImp$GrasasLub[datosImp$GrasasLub=="-"] <- NA
datosImp$Solventes[datosImp$Solventes=="-"] <- NA
datosImp$Naftas[datosImp$Naftas=="-"] <- NA
datosImp$Ceras[datosImp$Ceras=="-"] <- NA
datosImp$Butano[datosImp$Butano=="-"] <- NA
datosImp$Orimulsion[datosImp$Orimulsion=="-"] <- NA
datosImp$MezclasOleosas[datosImp$MezclasOleosas=="-"] <- NA
datosImp$Diesel[datosImp$Diesel =="-"] <- NA
datosImp$PetroleoReconst[datosImp$PetroleoReconst=="-"] <- NA
datosImp$Kerosina[datosImp$Kerosina=="-"] <- NA
datosImp$MTBE[datosImp$MTBE=="-"] <- NA
datosImp$Asfalto[datosImp$Asfalto=="-"] <- NA

### SE CUENTAN NAs
sum(is.na(datosImp$Anio))
sum(is.na(datosImp$Mes))
sum(is.na(datosImp$GLP))
sum(is.na(datosImp$GasAviacion)) #71
sum(is.na(datosImp$GasSuperior))
sum(is.na(datosImp$GasRegular))
sum(is.na(datosImp$Kerosina)) #36
sum(is.na(datosImp$rTurboJet)) #180
sum(is.na(datosImp$Diesel)) #27
sum(is.na(datosImp$DieselLS)) #204
sum(is.na(datosImp$DieselULS)) #219
sum(is.na(datosImp$Bunker)) 
sum(is.na(datosImp$Asfalto)) #10
sum(is.na(datosImp$PetCoke)) #116
sum(is.na(datosImp$AceitesLub)) #216
sum(is.na(datosImp$GrasasLub)) #216
sum(is.na(datosImp$Solventes)) #216
sum(is.na(datosImp$Naftas)) #227
sum(is.na(datosImp$Ceras)) #216
sum(is.na(datosImp$Butano)) #199
sum(is.na(datosImp$PetroleoReconst)) #211
sum(is.na(datosImp$MTBE)) # 215
sum(is.na(datosImp$Orimulsion)) # 218
sum(is.na(datosImp$MezclasOleosas)) #218
sum(is.na(datosImp$Total)) 

datosImp<- datosImp[-c(46,96,146,196), ] #se remueve filas con datos no numericos

View(datosImp)


## Se omiten NAs de las columnas diesel y se convierten en dataframes
Diesel2 <- na.omit(datosImp$Diesel)
DieselULS2 <- na.omit(datosImp$DieselULS)
DieselLS2 <- na.omit(datosImp$DieselLS)
DieselFrame <- data.frame(Diesel2)
DieselFrame1 <- data.frame(DieselULS2)
DieselFrame2 <- data.frame(DieselLS2)

#View(DieselFrame)
#View(DieselFrame1)
#View(DieselFrame2)

##Se renombran las columnas todas iguales y junta todo
colnames(DieselFrame) #Diesel2
names(DieselFrame1)[names(DieselFrame1) == "DieselULS2"] <- "Diesel2"
names(DieselFrame2)[names(DieselFrame2) == "DieselLS2"] <- "Diesel2"
DieselFull <-  rbind(DieselFrame,DieselFrame1,DieselFrame2)
View(DieselFull)

###FIN LIMPIEZA-------------------------------------------------------------------------------------------------------------------------------------------------------


#Para usarse en el analisis de series de tiempo
View(DieselFull$Diesel2)
View(datosImp$GasSuperior)
View(datosImp$GasRegular)
####ERROR AL HACER PLOT
#-----------------------------ANALISIS DIESEL------------------------------------
TSdata<-ts(DieselFull$Diesel2, start = c(2001,1), end = c(2020,3),frequency = 12)

class(TSdata)
#Saber cuando empieza la serie y cuando termina
start(TSdata)
end(TSdata)
#Saber la frecuencia de la serie
frequency(TSdata)

#hacemos plot
View(TSdata)

plot(TSdata)  #######################################ERROR AL HACER PLOT
abline(reg=lm(TSdata~time(TSdata)), col=c("red"))

plot(aggregate(TSdata,FUN=mean))
dec.TSdata<-decompose(TSdata)
plot(dec.TSdata)


#Aplicaremos una transformaci贸n logar胻mica
logTsData <- log(TSdata)
plot(decompose(logTsData))

#Ver el gr谩fico de la serie
plot(logTsData)
abline(reg=lm(logTsData~time(logTsData)), col=c("red"))
#estacionariedad de la media
#Para saber si hay raices unitarias
adfTest(logTsData)
adfTest(diff(logTsData))

#con una fue suficiente, VALOR D=1
#Gr谩fico de autocorrelaci贸n
acf(logTsData)
#el valor de q=1 o 1.5
pacf(logTsData) 
#despues de ese grafico, entonces el valor p es de 0

#p=0
#d=1
#q=1 o 1.5
#ARIMA= (p,d,q)
#ARIMA= (0,1,1)

#plot estacional
plot(dec.TSdata$seasonal)
acf(diff(logTsData),36)
pacf(diff(logTsData),36)
# Hacer el modelo de Diesel ARIMA
auto.arima(TSdata)

fit <- arima(log(TSdata), c(0, 0, 0),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(TSdata,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(TSdata), c(2, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)

#----------------FIN ANALISIS DIESELLLLL-------------------


#---------------ANALISIS SUPER DIESEL
TSdata2<-ts(datosImp$GasSuperior, start = c(2001,1), end = c(2017,3),frequency = 12)
class(TSdata2)
#Saber cuando empieza la serie y cuando termina
start(TSdata2)
end(TSdata2)
#Saber la frecuencia de la serie
frequency(TSdata2)

#hacemos plot
plot(TSdata2)
abline(reg=lm(TSdata~time(TSdata2)), col=c("red"))

plot(aggregate(TSdata2,FUN=mean))
dec.TSdata2<-decompose(TSdata2)
plot(dec.TSdata2)


#Aplicaremos una transformaci贸n logar胻mica
logTsData2 <- log(TSdata2)
plot(decompose(logTsData2))

#Ver el gr谩fico de la serie
plot(logTsData2)
abline(reg=lm(logTsData2~time(logTsData2)), col=c("red"))
#estacionariedad de la media
#Para saber si hay raices unitarias
adfTest(logTsData2)
adfTest(diff(logTsData2))

#con una fue suficiente, VALOR D=1
#Gr谩fico de autocorrelaci贸n
acf(logTsData2,50)
#el valor de q=2
pacf(logTsData) 
#despues de ese grafico, entonces el valor p es de 0

#p=0
#d=1
#q=2
#ARIMA= (p,d,q)
#ARIMA= (0,1,2)

#plot estacional
plot(dec.TSdata2$seasonal)
acf(diff(logTsData2),36)
pacf(diff(logTsData2),36)
# Hacer el modelo de Diesel ARIMA
auto.arima(TSdata2)

fit2 <- arima(log(TSdata2), c(1, 1, 2),seasonal = list(order = c(1, 1, 1), period = 12))
pred2 <- predict(fit2, n.ahead = 10)
ts.plot(TSdata,2.718^pred2$pred, log = "y", lty = c(1,3))

fit22 <- arima(log(TSdata2), c(1, 1,2 ),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP2 <- forecast(fit22, level = c(10), h = 60)
autoplot(forecastAP2)

#----------------FIN ANALISIS SUPER-------------------


#---------------ANALISIS REGULAR
TSdata3<-ts(datosImp$GasRegular, start = c(2001,1), end = c(2017,3),frequency = 12)
class(TSdata3)
#Saber cuando empieza la serie y cuando termina
start(TSdata3)
end(TSdata3)
#Saber la frecuencia de la serie
frequency(TSdata3)

#hacemos plot
plot(TSdata3)
abline(reg=lm(TSdata~time(TSdata3)), col=c("red"))

plot(aggregate(TSdata3,FUN=mean))
dec.TSdata3<-decompose(TSdata3)
plot(dec.TSdata3)


#Aplicaremos una transformaci贸n logar胻mica
logTSdata3 <- log(TSdata3)
plot(decompose(logTSdata3))

#Ver el gr谩fico de la serie
plot(logTSdata3)
abline(reg=lm(logTSdata3~time(logTSdata3)), col=c("red"))
#estacionariedad de la media
#Para saber si hay raices unitarias
adfTest(logTSdata3)
adfTest(diff(logTSdata3))

#con una fue suficiente, VALOR D=1
#Gr谩fico de autocorrelaci贸n
acf(logTSdata3,50)
#el valor de q=2
pacf(logTsData) 
#despues de ese grafico, entonces el valor p es de 0

#p=0
#d=1
#q=2
#ARIMA= (p,d,q)
#ARIMA= (0,1,2)

#plot estacional
plot(dec.TSdata3$seasonal)
acf(diff(logTSdata3),36)
pacf(diff(logTSdata3),36)
# Hacer el modelo de Diesel ARIMA
auto.arima(TSdata3)

fit2 <- arima(log(TSdata3), c(1, 1, 2),seasonal = list(order = c(1, 1, 1), period = 12))
pred2 <- predict(fit2, n.ahead = 10)
ts.plot(TSdata,2.718^pred2$pred, log = "y", lty = c(1,3))

fit22 <- arima(log(TSdata3), c(1, 1,2 ),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP2 <- forecast(fit22, level = c(10), h = 60)
autoplot(forecastAP2)

#----------------FIN ANALISIS REGULAR-------------------


