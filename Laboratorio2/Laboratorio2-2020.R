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
library(zoo)
library(xts)
library(lubridate)
library(prophet)
##------------------FIN ZONA LIBRERIAS------------------


##-----------------ZONA DIRECTORIOS---------------------------
getwd()
setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/LabsDataScience/Laboratorio2")
#setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/Laboratorio1/LabsDataScience/Laboratorio2")
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
#test <-read.csv("test.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#train <-read.csv("train.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#head(test[,1:3])
#head(train[,1:3])


# Leer de un PDF
# install.packages("tabulizer")
pages<-extract_tables("C01-Importaci髇-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar



View(datosImp)
##Se reemplazan "-" o "0" etc por NA en directorend
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
datosImp$PetroleoReconst[datosImp$PetroleoReconst=="-"] <- NA
datosImp$MTBE[datosImp$MTBE=="-"] <- NA

View(datosImp)


data("AirPassengers")


#DIESEL
TSdata<-ts(datosImp$Diesel, start = c(2001,1), end = c(2017,3),frequency = 12)
class(TSdata)
#Saber cuando empieza la serie y cuando termina
start(TSdata)
end(TSdata)
#Saber la frecuencia de la serie
frequency(TSdata)

#hacemos plot
plot(TSdata)
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





