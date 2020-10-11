## Guatemala, marzo del 2019
## Data Science
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Rodrigo Samayoa 17xxx
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

# Load readtext package
library(readtext)

options(scipen = 999)
library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)


#######_______________________DIRECTORIOS Y LECTURA DE DATOS__________________#############
getwd()
setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8")


######_____________________DATOS DE LA SAT_________________________

##2016
data012016 <- read.delim("dataSAT2016/web_imp_08012016.txt", sep = "|",header=TRUE,row.names = NULL)
data022016 <- read.delim("dataSAT2016/web_imp_08022016.txt", sep = "|",header=TRUE,row.names = NULL)
data032016 <- read.delim("dataSAT2016/web_imp_08032016.txt", sep = "|",header=TRUE,row.names = NULL)
data042016 <- read.delim("dataSAT2016/web_imp_08042016.txt", sep = "|",header=TRUE,row.names = NULL)
data052016 <- read.delim("dataSAT2016/web_imp_08052016.txt", sep = "|",header=TRUE,row.names = NULL)
data062016 <- read.delim("dataSAT2016/web_imp_08062016.txt", sep = "|",header=TRUE,row.names = NULL)
data072016 <- read.delim("dataSAT2016/web_imp_08072016.txt", sep = "|",header=TRUE,row.names = NULL)
data082016 <- read.delim("dataSAT2016/web_imp_08082016.txt", sep = "|",header=TRUE,row.names = NULL)
data092016 <- read.delim("dataSAT2016/web_imp_08092016.txt", sep = "|",header=TRUE,row.names = NULL)
data102016 <- read.delim("dataSAT2016/web_imp_08102016.txt", sep = "|",header=TRUE,row.names = NULL)
data112016 <- read.delim("dataSAT2016/web_imp_08112016.txt", sep = "|",header=TRUE,row.names = NULL)
data122016 <- read.delim("dataSAT2016/web_imp_08122016.txt", sep = "|",header=TRUE,row.names = NULL)

dataSat2016 <- rbind(data012016,
                     data022016,
                     data032016,
                     data042016,
                     data062016,
                     data072016,
                     data082016,
                     data092016,
                     data102016,
                     data112016,
                     data122016)
dataSat2016$Año <- "2016"
View(dataSat2016)

#2017
data012017 <- read.delim("dataSAT2017/web_imp_08012017.txt", sep = "|",header=TRUE,row.names = NULL)
data022017 <- read.delim("dataSAT2017/web_imp_08022017.txt", sep = "|",header=TRUE,row.names = NULL)
data032017 <- read.delim("dataSAT2017/web_imp_08032017.txt", sep = "|",header=TRUE,row.names = NULL)
data042017 <- read.delim("dataSAT2017/web_imp_08042017.txt", sep = "|",header=TRUE,row.names = NULL)
data052017 <- read.delim("dataSAT2017/web_imp_08052017.txt", sep = "|",header=TRUE,row.names = NULL)
data062017 <- read.delim("dataSAT2017/web_imp_08062017.txt", sep = "|",header=TRUE,row.names = NULL)
data072017 <- read.delim("dataSAT2017/web_imp_08072017.txt", sep = "|",header=TRUE,row.names = NULL)
data082017 <- read.delim("dataSAT2017/web_imp_08082017.txt", sep = "|",header=TRUE,row.names = NULL)
data092017 <- read.delim("dataSAT2017/web_imp_08092017.txt", sep = "|",header=TRUE,row.names = NULL)
data102017 <- read.delim("dataSAT2017/web_imp_08102017.txt", sep = "|",header=TRUE,row.names = NULL)
data112017 <- read.delim("dataSAT2017/web_imp_08112017.txt", sep = "|",header=TRUE,row.names = NULL)
data122017 <- read.delim("dataSAT2017/web_imp_08122017.txt", sep = "|",header=TRUE,row.names = NULL)

dataSat2017 <- rbind(data012017,
                     data022017,
                     data032017,
                     data042017,
                     data062017,
                     data072017,
                     data082017,
                     data092017,
                     data102017,
                     data112017,
                     data122017)
dataSat2017$Año <- "2017"
View(dataSat2017)

#2018
data012018 <- read.delim("dataSAT2018/web_imp_08012018.txt", sep = "|",header=TRUE,row.names = NULL)
data022018 <- read.delim("dataSAT2018/web_imp_08022018.txt", sep = "|",header=TRUE,row.names = NULL)
data032018 <- read.delim("dataSAT2018/web_imp_08032018.txt", sep = "|",header=TRUE,row.names = NULL)
data042018 <- read.delim("dataSAT2018/web_imp_08042018.txt", sep = "|",header=TRUE,row.names = NULL)
data052018 <- read.delim("dataSAT2018/web_imp_08052018.txt", sep = "|",header=TRUE,row.names = NULL)
data062018 <- read.delim("dataSAT2018/web_imp_08062018.txt", sep = "|",header=TRUE,row.names = NULL)
data072018 <- read.delim("dataSAT2018/web_imp_08072018.txt", sep = "|",header=TRUE,row.names = NULL)
data082018 <- read.delim("dataSAT2018/web_imp_08082018.txt", sep = "|",header=TRUE,row.names = NULL)
data092018 <- read.delim("dataSAT2018/web_imp_08092018.txt", sep = "|",header=TRUE,row.names = NULL)
data102018 <- read.delim("dataSAT2018/web_imp_08102018.txt", sep = "|",header=TRUE,row.names = NULL)
data112018 <- read.delim("dataSAT2018/web_imp_08112018.txt", sep = "|",header=TRUE,row.names = NULL)
data122018 <- read.delim("dataSAT2018/web_imp_08122018.txt", sep = "|",header=TRUE,row.names = NULL)

dataSat2018 <- rbind(data012018,
                     data022018,
                     data032018,
                     data042018,
                     data062018,
                     data072018,
                     data082018,
                     data092018,
                     data102018,
                     data112018,
                     data122018)
dataSat2018$Año <- "2018"
View(dataSat2018)

#2019
data012019 <- read.delim("dataSAT2019/web_imp_08012019.txt", sep = "|",header=TRUE,row.names = NULL)
data022019 <- read.delim("dataSAT2019/web_imp_08022019.txt", sep = "|",header=TRUE,row.names = NULL)
data032019 <- read.delim("dataSAT2019/web_imp_08032019.txt", sep = "|",header=TRUE,row.names = NULL)
data042019 <- read.delim("dataSAT2019/web_imp_08042019.txt", sep = "|",header=TRUE,row.names = NULL)
data052019 <- read.delim("dataSAT2019/web_imp_08052019.txt", sep = "|",header=TRUE,row.names = NULL)
data062019 <- read.delim("dataSAT2019/web_imp_08062019.txt", sep = "|",header=TRUE,row.names = NULL)
data072019 <- read.delim("dataSAT2019/web_imp_08072019.txt", sep = "|",header=TRUE,row.names = NULL)
data082019 <- read.delim("dataSAT2019/web_imp_08082019.txt", sep = "|",header=TRUE,row.names = NULL)
data092019 <- read.delim("dataSAT2019/web_imp_08092019.txt", sep = "|",header=TRUE,row.names = NULL)
data102019 <- read.delim("dataSAT2019/web_imp_08102019.txt", sep = "|",header=TRUE,row.names = NULL)
data112019 <- read.delim("dataSAT2019/web_imp_08112019.txt", sep = "|",header=TRUE,row.names = NULL)
data122019 <- read.delim("dataSAT2019/web_imp_08122019.txt", sep = "|",header=TRUE,row.names = NULL)

dataSat2019 <- rbind(data012019,
                     data022019,
                     data032019,
                     data042019,
                     data062019,
                     data072019,
                     data082019,
                     data092019,
                     data102019,
                     data112019,
                     data122019)
dataSat2019$Año <- "2019"
View(dataSat2019)

### Unimos la data y nombramos bien las columnas por un problema de **row.names**

dataSat <- rbind(dataSat2016,
                 dataSat2017,
                 dataSat2018,
                 dataSat2019)
View(dataSat)

names(dataSat)[names(dataSat) == "row.names"] <- "Pais.de.Proveniencia_"
names(dataSat)[names(dataSat) == "Pais.de.Proveniencia"] <- "Aduana.de.Ingreso_"
names(dataSat)[names(dataSat) == "Aduana.de.Ingreso"] <- "Fecha.de.la.Poliza_"
names(dataSat)[names(dataSat) == "Fecha.de.la.Poliza"] <- "Partida.Arancelaria_"
names(dataSat)[names(dataSat) == "Partida.Arancelaria"] <- "Modelo.del.Vehiculo_"
names(dataSat)[names(dataSat) == "Modelo.del.Vehiculo"] <- "Marca_"
names(dataSat)[names(dataSat) == "Marca"] <- "Linea_"
names(dataSat)[names(dataSat) == "Linea"] <- "Centimetros.Cubicos_"
names(dataSat)[names(dataSat) == "Centimetros.Cubicos"] <- "Distintivo_"
names(dataSat)[names(dataSat) == "Distintivo"] <- "Tipo.de.Vehiculo_"
names(dataSat)[names(dataSat) == "Tipo.de.Vehiculo"] <- "Tipo.de.Importador_"
names(dataSat)[names(dataSat) == "Tipo.de.Importador"] <- "Tipo.Combustible_"
names(dataSat)[names(dataSat) == "Tipo.Combustible"] <- "Asientos_"
names(dataSat)[names(dataSat) == "Asientos"] <- "Puertas_"
names(dataSat)[names(dataSat) == "Puertas"] <- "Tonelaje_"
names(dataSat)[names(dataSat) == "Tonelaje"] <- "Valor.CIF_"
names(dataSat)[names(dataSat) == "Valor.CIF"] <- "Impuesto_"

View(dataSat)


##### datos del ine___________________________________________________________
DB2016 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2016INE.sav", to.data.frame=TRUE)
DB2017 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2017INE.sav", to.data.frame=TRUE)
DB2018 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2018INE.sav", to.data.frame=TRUE)
DB2019 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2019INE.sav", to.data.frame=TRUE)

View(DB2016)
View(DB2017)
View(DB2018)
View(DB2019)


##________________________________ANALISIS EXPLORATORIO_______________________________________##
## Hacemos un chequeo en general de los datos de la SAT
str(dataSat)

glimpse(dataSat)
str(dataSat)
head(dataSat)

#Filtramos solo para motos
data_motos <- dataSat[dataSat$Tipo.de.Vehiculo_ == 'MOTO',]
#Elimnamos la columna impuesto que tiene nulls
data_motos_ <- data_motos[,c(0:16,17,19)] 


View(data_motos_)
str(data_motos_)

data_motos_$Centimetros.Cubicos_ <- as.numeric(data_motos_$Centimetros.Cubicos_) 
data_motos_$Asientos_ <- as.numeric(data_motos_$Asientos_) 
data_motos_$Puertas_ <- as.numeric(data_motos_$Puertas_) 
data_motos_$Tonelaje_ <- as.numeric(data_motos_$Tonelaje_) 
data_motos_$Año <- as.numeric(data_motos_$Año)
data_motos_$Modelo.del.Vehiculo_ <- as.numeric(data_motos_$Modelo.del.Vehiculo_)

## clusters y kmeans
datos<-data_motos_
irisCompleto<-data_motos_[complete.cases(data_motos_),] #se eliminan NAs
km<-kmeans(na.omit(data_motos_[,c(8,13:15,18)]) ,3)
datos$grupo<-km$cluster








