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

## ANALISIS
library(lattice)
library(DataExplorer)
library(grDevices)
library(factoextra)

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

library(ranger)
library(Metrics)
library(ROCit)
library(kableExtra)


#######_______________________DIRECTORIOS Y LECTURA DE DATOS__________________#############
getwd()
setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8")






###################################   S     A      T   #########################################

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
##View(dataSat2016)

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
##View(dataSat2017)

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
##View(dataSat2018)

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
##View(dataSat2019)

### Unimos la data y nombramos bien las columnas por un problema de **row.names**

dataSat <- rbind(dataSat2016,
                 dataSat2017,
                 dataSat2018,
                 dataSat2019)
#View(dataSat)

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
str(dataSat)
#View(dataSat)


##________________________________ANALISIS EXPLORATORIO_______________________________________##

## Hacemos un chequeo en general de los datos de la SAT
str(dataSat)

#Filtramos solo para motos
data_motos <- dataSat[dataSat$Tipo.de.Vehiculo_ == 'MOTO',]
#Elimnamos la columna impuesto que tiene nulls
data_motos_ <- data_motos[,c(0:3,5:11,16,17,19)] 


glimpse(data_motos_)
str(data_motos_)
head(data_motos_)


# Tablas de frecuencias para variables cualitativas
pais <- table(data_motos_$Pais.de.Proveniencia_)
pais <- sort(pais, decreasing=TRUE)
aduana <- table(data_motos_$Aduana.de.Ingreso_)
aduana <- sort(aduana, decreasing=TRUE)
modelo <- table(data_motos_$Modelo.del.Vehiculo_)
modelo <- sort(modelo, decreasing=TRUE)
marca <- table(data_motos_$Marca_)
marca <- sort(marca, decreasing=TRUE)
linea <- table(data_motos_$Linea_)
linea <- sort(linea, decreasing=TRUE)
distint <- table(data_motos_$Distintivo_)
distint <- sort(distint, decreasing=TRUE)
tipoimp <- table(data_motos_$Tipo.de.Importador_)
tipoimp <- sort(tipoimp, decreasing=TRUE)

View(pais)
View(aduana)
View(modelo)
View(marca)
View(linea)
View(distint)
View(tipoimp)


### variables char a numerico
data_motos_$Centimetros.Cubicos_ <- as.numeric(data_motos_$Centimetros.Cubicos_) 
data_motos_$Asientos_ <- as.numeric(data_motos_$Asientos_) 
data_motos_$Puertas_ <- as.numeric(data_motos_$Puertas_) 
data_motos_$Tonelaje_ <- as.numeric(data_motos_$Tonelaje_) 
data_motos_$Año <- as.numeric(data_motos_$Año)
data_motos_$Modelo.del.Vehiculo_ <- as.numeric(data_motos_$Modelo.del.Vehiculo_)

summary(data_motos_)
plot_intro(data_motos_)


#####FRECUENCIAS
#Conteo
table(data_motos_$Marca_)
#data_motos_ <- data_motos_[data_motos_$Tipo.de.Vehiculo_ == 'MOTO',]
View(data_motos_)

####PROPORCIONES
# proporiciones en frecuencia de las marcas
prop.table(table(data_motos_$Marca_))
#Percentage distribution 
prop.table(table(data_motos_$Marca_, data_motos_$Pais.de.Proveniencia_))

######################### variables categoricas y numericas #################################
str(data_motos_)
cat_vars <- names(data_motos_)[which(sapply(data_motos_, is.character))]
numeric_vars <- names(data_motos_)[which(sapply(data_motos_, is.numeric))]


####Convert character to factors###############3
library(data.table)
setDT(data_motos_)[,(cat_vars) := lapply(.SD, as.factor), .SDcols = cat_vars]

data_motos_.cat <- data_motos_[,.SD, .SDcols = cat_vars]
data_motos_.cont <- data_motos_[,.SD,.SDcols = numeric_vars]

View(data_motos_.cont)
View(data_motos_.cat)

##############Eliminar datos atipicos_________________________________________________-
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

cm_cubicos <- impute_outliers(data_motos_$Centimetros.Cubicos_)
v_cif <- impute_outliers(data_motos_$Valor.CIF_)
v_impuesto <- impute_outliers(data_motos_$Impuesto_)

boxplot(cm_cubicos, col = "blue")
hist(cm_cubicos, col = "blue")
boxplot(v_cif, col = "green")
hist(v_cif, col = "green")
boxplot(v_impuesto, col = "red")
hist(v_impuesto, col = "red")

with(data_motos_, plot(x=Centimetros.Cubicos_, y=Impuesto_))
with(data_motos_, plot(x=Centimetros.Cubicos_, y=Valor.CIF_))
with(data_motos_, plot(x=Impuesto_, y=Valor.CIF_))


# Convertir la variable numerica "pasos" en categorica
# para ello definimos los puntos de corte
breakPoints <- c(0, 149, 500, Inf)
categories <- c("Pequeño", "Mediano", "Grande")

# y cortamos la variable número de pasos segun esta categorizacion
data_motos_$Centimetros.Cubicos_.F <- cut(data_motos_$Centimetros.Cubicos_, breaks = breakPoints, labels = categories)

summary(data_motos_$Centimetros.Cubicos_)
View(data_motos_)

#intento de de grafica de pie
#ggplot(data_motos_,aes(x="",y=Centimetros.Cubicos_, fill=Centimetros.Cubicos_.F))+
#geom_bar(stat = "identity",color="white")+
#  coord_polar(theta="y")
# se hara mejor en excel de acuerdo a infografia

library(openxlsx)
excel_motos_sat <- write.xlsx(data_motos_,".xlsx")
saveWorkbook(excel_motos_sat, file = "motos_sat.xlsx", overwrite = TRUE)






###########################################################################################





###################################   I      N       E   #########################################
##### datos del ine___________________________________________________________
#DB2014 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2014INE.sav", to.data.frame=TRUE)
#DB2015 = read.csv("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2015INE.csv",stringsAsFactors = FALSE, na.strings = TRUE)
#DB2016 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2016INE.sav", to.data.frame=TRUE)
#DB2017 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2017INE.sav", to.data.frame=TRUE)
#DB2018 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2018INE.sav", to.data.frame=TRUE)
#DB2019 = read.spss("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/LabsDataScience/Laboratorio8/2019INE.sav", to.data.frame=TRUE)


DB2014 = read.spss("2014INE.sav", to.data.frame=TRUE)
DB2015 = read.csv("2015INE.csv",stringsAsFactors = FALSE, na.strings = TRUE)
DB2016 = read.spss("2016INE.sav", to.data.frame=TRUE)
DB2017 = read.spss("2017INE.sav", to.data.frame=TRUE)
DB2018 = read.spss("2018INE.sav", to.data.frame=TRUE)
#DB2019 = read.spss("C:/Users/jio9/2019INE.sav", to.data.frame=TRUE)

#View(DB2014)
#View(DB2015)
#View(DB2016)
#View(DB2017)
#View(DB2018)
#View(DB2019)


DB2018$día_ocu <- NULL
DB2018$mupio_ocu <- NULL
DB2018$zona_ocu <- NULL
DB2018$marca_veh <- NULL
DB2018$modelo_veh <- NULL
DB2018$g_modelo_veh <- NULL
DB2018$tipo_eve <- NULL
DB2018$núm_corre <- NULL
names(DB2018)[names(DB2018) == "año_ocu"] <- "anio_ocu"
names(DB2018)[names(DB2018) == "día_sem_ocu"] <- "dia_sem_ocu"
DB2018$depto_ocu <- as.factor(ifelse(DB2018$depto_ocu=="Petén", "Peten", as.character(DB2018$depto_ocu)))
DB2018$depto_ocu <- as.factor(ifelse(DB2018$depto_ocu=="Quiché", "Quiche", as.character(DB2018$depto_ocu)))

#Limpiamos el 2014 a Estándares de 2018 Variables que si dejaremos
DB2014$día_ocu <- NULL
DB2014$mupio_ocu <- NULL
DB2014$zona_ocu <- NULL
DB2014$marca_veh <- NULL
DB2014$modelo_veh <- NULL
DB2014$g_edad <- NULL
DB2014$año_ocu <- 2014
DB2014$hora_ocu <- as.factor(DB2014$hora_ocu)
DB2014$corre_base <- NULL
DB2014$área_geo_ocu <- NULL
DB2014$edad_con <- NULL
DB2014$mayor_menor <- NULL
DB2014$estado_con<- NULL
DB2014$sexo_con <- NULL
DB2014$num_corre <- NULL
DB2014$tipo_eve <- NULL
DB2014$g_hora_5 <- as.factor(ifelse(DB2014$g_hora ==  "00:00 a 05:59" | DB2014$g_hora ==  "06:00 a 11:59" , "Mañana", ifelse(
  DB2014$g_hora ==  "12:00 a 17:59", "Tarde", "Noche"
)))
DB2014$mes_ocu <- as.factor(ifelse(DB2014$mes_ocu=="Septiemre", "Septiembre", as.character(DB2014$mes_ocu)))
DB2014$num_correlativo <- NULL
DB2014$depto_ocu <- as.factor(ifelse(DB2014$depto_ocu=="Santa rosa", "Santa Rosa", as.character(DB2014$depto_ocu)))
DB2014$tipo_veh <- as.factor(ifelse(DB2014$tipo_veh=="Microbus", "Microbús", as.character(DB2014$tipo_veh)))

#limpiamos tildes
names(DB2014)[names(DB2014) == "día_sem_ocu"] <- "dia_sem_ocu"
names(DB2014)[names(DB2014) == "año_ocu"] <- "anio_ocu"
#Limpiamos 2015
DB2015$núm_corre <- NULL
DB2015$día_ocu <- NULL
DB2015$mupio_ocu <- NULL
DB2015$zona_ocu <- NULL
DB2015$marca_veh <- NULL
DB2015$modelo_veh <- NULL
DB2015$g_modelo_veh <- NULL
DB2015$tipo_eve <- NULL
DB2015$área_geo_ocu <- NULL
DB2015$sexo_per <- NULL
DB2015$edad_per <- NULL
DB2015$estado_con <- NULL
DB2015$edad_quinquenales<- NULL
DB2015$g_edad_60ymás<- NULL
DB2015$g_edad_80ymás<- NULL
DB2015$mayor_menor<- NULL
names(DB2015)[names(DB2015) == "año_ocu"] <- "anio_ocu"
names(DB2015)[names(DB2015) == "día_sem_ocu"] <- "dia_sem_ocu"
#limpiamos a profundidadd 015
DB2015$anio_ocu <- as.numeric(DB2015$anio_ocu) 
DB2015$hora_ocu <- as.factor(DB2015$hora_ocu)
#mes
DB2015$mes_ocu <- as.factor(ifelse(DB2015$mes_ocu ==  1 , "Enero", ifelse(
  DB2015$mes_ocu ==  2, "Febrero", ifelse(
    DB2015$mes_ocu ==  3, "Marzo",   ifelse(  
      DB2015$mes_ocu ==  4, "Abril",   ifelse(
        DB2015$mes_ocu ==  5, "Mayo",    ifelse(
          DB2015$mes_ocu ==  6, "Junio",   ifelse(
            DB2015$mes_ocu ==  7, "Julio",   ifelse(
              DB2015$mes_ocu ==  8, "Agosto",  ifelse(
                DB2015$mes_ocu ==  9, "Septiembre",    ifelse(
                  DB2015$mes_ocu ==  10, "Octubre",    ifelse(
                    DB2015$mes_ocu ==  11, "Noviembre",  "Diciembre"
                  ))))))))))))

#día de la semana
DB2015$dia_sem_ocu <- as.factor(ifelse(DB2015$dia_sem_ocu ==  1 , "Lunes", ifelse(
  DB2015$dia_sem_ocu ==  2, "Martes", ifelse(
    DB2015$dia_sem_ocu ==  3, "Miércoles",   ifelse(  
      DB2015$dia_sem_ocu ==  4, "Jueves",   ifelse(
        DB2015$dia_sem_ocu ==  5, "Viernes",    ifelse(
          DB2015$dia_sem_ocu ==  6, "Sábado",  "Domingo"
        )))))))
#hora
DB2015$g_hora <- as.factor(ifelse(DB2015$g_hora ==  1, "00:00 a 05:59", ifelse(
  DB2015$g_hora ==  2, "06:00 a 11:59", ifelse(
    DB2015$g_hora ==  3, "12:00 a 17:59", "18:00 a 23:59"
  ))))
#clasificacion extra de horas
DB2015$g_hora_5 <- as.factor(ifelse(DB2015$g_hora ==  "00:00 a 05:59" , "Mañana", ifelse(
  DB2015$g_hora ==  "06:00 a 11:59", "Mañana", ifelse(
    DB2015$g_hora ==  "12:00 a 17:59", "Tarde", "Noche"
  ))))

#color
DB2015$color_veh <- as.factor(ifelse(DB2015$color_veh ==  1 , "Rojo", ifelse(
  DB2015$color_veh ==  2, "Blanco", ifelse(
    DB2015$color_veh ==  3, "Azul",   ifelse(  
      DB2015$color_veh ==  4, "Gris",   ifelse(
        DB2015$color_veh ==  5, "Negro",  ifelse(
          DB2015$color_veh ==  6, "Verde",  ifelse(
            DB2015$color_veh ==  7, "Amarillo",  ifelse(
              DB2015$color_veh ==  8, "Celeste",  ifelse(
                DB2015$color_veh ==  9, "Corinto",  ifelse(
                  DB2015$color_veh ==  10, "Cafe",  ifelse(
                    DB2015$color_veh ==  11, "Beige",  ifelse(
                      DB2015$color_veh ==  12, "Turqueza",  ifelse(
                        DB2015$color_veh ==  13, "Marfil",  ifelse(
                          DB2015$color_veh ==  14, "Anaranjado",  ifelse(
                            DB2015$color_veh ==  15, "Aqua",  ifelse(
                              DB2015$color_veh ==  16, "Morado",  ifelse(
                                DB2015$color_veh ==  17, "Rosado",  "Ignorado"
                              ))))))))))))))))))

#vehiculo
DB2015$tipo_veh<- as.factor(ifelse(DB2015$tipo_veh ==  1 , "Automóvil", ifelse(
  DB2015$tipo_veh ==  2, "Camioneta", ifelse(
    DB2015$tipo_veh ==  3, "Pick up",   ifelse(  
      DB2015$tipo_veh ==  4, "Motocicleta",   ifelse(
        DB2015$tipo_veh ==  5, "Camión",  ifelse(
          DB2015$tipo_veh ==  6, "Cabezal",  ifelse(
            DB2015$tipo_veh ==  7, "Bus extraurbano",  ifelse(
              DB2015$tipo_veh ==  8, "Jeep",  ifelse(
                DB2015$tipo_veh ==  9, "Microbús",  ifelse(
                  DB2015$tipo_veh ==  10, "Taxi",  ifelse(
                    DB2015$tipo_veh ==  11, "Panel",  ifelse(
                      DB2015$tipo_veh ==  12, "Bus urbano",  ifelse(
                        DB2015$tipo_veh ==  13, "Tractor",  ifelse(
                          DB2015$tipo_veh ==  14, "Moto taxi",  ifelse(
                            DB2015$tipo_veh ==  15, "Furgón",  ifelse(
                              DB2015$tipo_veh ==  16, "Grúa",  ifelse(
                                DB2015$tipo_veh ==  17, "Bus escolar",  ifelse(
                                  DB2015$tipo_veh ==  18, "Bicicleta",  "Ignorado"
                                )))))))))))))))))))
#limpieza Departamentos
DB2015$depto_ocu <- as.factor(ifelse(DB2015$depto_ocu ==  1 , "Guatemala", ifelse(
  DB2015$depto_ocu ==  2, "El Progreso", ifelse(
    DB2015$depto_ocu ==  3, "Sacatepéquez",   ifelse(  
      DB2015$depto_ocu ==  4, "Chimaltenango",   ifelse(
        DB2015$depto_ocu ==  5, "Escuintla",    ifelse(
          DB2015$depto_ocu ==  6, "Santa Rosa",   ifelse(
            DB2015$depto_ocu ==  7, "Sololá",   ifelse(
              DB2015$depto_ocu ==  8, "Totonicapán",  ifelse(
                DB2015$depto_ocu ==  9, "Quetzaltenango",    ifelse(
                  DB2015$depto_ocu ==  10, "Suchitepéquez",    ifelse(
                    DB2015$depto_ocu ==  11, "Retalhuleu",  ifelse( 
                      DB2015$depto_ocu ==  12, "San Marcos",  ifelse(  
                        DB2015$depto_ocu ==  13, "Huehuetenango",  ifelse( 
                          DB2015$depto_ocu ==  14, "Quiche",  ifelse( 
                            DB2015$depto_ocu ==  15, "Baja Verapaz",  ifelse( 
                              DB2015$depto_ocu ==  16, "Alta Verapaz",  ifelse(
                                DB2015$depto_ocu ==  17, "Peten",  ifelse( 
                                  DB2015$depto_ocu ==  18, "Izabal",  ifelse( 
                                    DB2015$depto_ocu ==  19, "Zacapa",  ifelse( 
                                      DB2015$depto_ocu ==  20, "Chiquimula",  ifelse( 
                                        DB2015$depto_ocu ==  21, "Jalapa", "Jutiapa"
                                      ))))))))))))))))))))))

#fin limpieza 2015 

#Limpiamos 2016
DB2016$núm_corre <- NULL
DB2016$día_ocu <- NULL
DB2016$mupio_ocu <- NULL
DB2016$zona_ocu <- NULL
DB2016$marca_veh <- NULL
DB2016$modelo_veh <- NULL
DB2016$g_modelo_veh <- NULL
DB2016$tipo_eve <- NULL
DB2016$área_geo_ocu <- NULL
names(DB2016)[names(DB2016) == "año_ocu"] <- "anio_ocu"
names(DB2016)[names(DB2016) == "día_sem_ocu"] <- "dia_sem_ocu"

#limpiamos 2017
DB2017$núm_corre <- NULL
DB2017$día_ocu <- NULL
DB2017$mupio_ocu <- NULL
DB2017$zona_ocu <- NULL
DB2017$marca_veh <- NULL
DB2017$modelo_veh <- NULL
DB2017$g_modelo_veh <- NULL
DB2017$tipo_eve <- NULL
DB2017$área_geo_ocu <- NULL
names(DB2017)[names(DB2017) == "año_ocu"] <- "anio_ocu"
names(DB2017)[names(DB2017) == "día_sem_ocu"] <- "dia_sem_ocu"


#UNIMOS TODA LA DATA
DBTOTAL <- rbind(DB2015 ,DB2016, DB2017,DB2018)
#View(DBTOTAL)
str(DBTOTAL)


#################HACER REVISION HASTA ACA Y ARREGLAR NUMERO DE COLUMNAS




DBTOTAL2 <-DBTOTAL
#día de la semana
DBTOTAL2$dia_sem_ocu <-as.numeric( ifelse(DBTOTAL2$dia_sem_ocu ==  "Lunes" , 1, ifelse(
  DBTOTAL2$dia_sem_ocu ==  "Martes", 2, ifelse(
    DBTOTAL2$dia_sem_ocu ==  "Miércoles", 3,   ifelse(  
      DBTOTAL2$dia_sem_ocu ==  "Jueves", 4,   ifelse(
        DBTOTAL2$dia_sem_ocu ==  "Viernes", 5,    ifelse(
          DBTOTAL2$dia_sem_ocu ==  "Sábado", 6,  7
        )))))))

#año
DBTOTAL2$anio_ocu <- as.numeric(DBTOTAL2$anio_ocu)

#g_hora
DBTOTAL2$g_hora <- as.numeric(ifelse(DBTOTAL2$g_hora ==  "00:00 a 05:59" ,1 , ifelse(
  DBTOTAL2$g_hora ==  "06:00 a 11:59", 2, ifelse(
    DBTOTAL2$g_hora ==  "12:00 a 17:59", 3, 4
  ))))

#mes
DBTOTAL2$mes_ocu <- as.numeric(ifelse(DBTOTAL2$mes_ocu ==  "Enero" , 1, ifelse(
  DBTOTAL2$mes_ocu ==  "Febrero", 2, ifelse(
    DBTOTAL2$mes_ocu ==  "Marzo", 3,   ifelse(  
      DBTOTAL2$mes_ocu ==  "Abril", 4,   ifelse(
        DBTOTAL2$mes_ocu ==  "Mayo", 5,    ifelse(
          DBTOTAL2$mes_ocu ==  "Junio", 6,   ifelse(
            DBTOTAL2$mes_ocu ==  "Julio", 7,   ifelse(
              DBTOTAL2$mes_ocu ==  "Agosto", 8,  ifelse(
                DBTOTAL2$mes_ocu ==  "Septiembre", 9,    ifelse(
                  DBTOTAL2$mes_ocu ==  "Octubre", 10,    ifelse(
                    DBTOTAL2$mes_ocu ==  "Noviembre", 11,  12
                  ))))))))))))
#Tipo de vehiculo
DBTOTAL2$tipo_veh<- as.numeric(ifelse(DBTOTAL2$tipo_veh ==  "Automóvil" ,1 , ifelse(
  DBTOTAL2$tipo_veh == "Camioneta",2,  ifelse(
    DBTOTAL2$tipo_veh == "Pick up", 3,    ifelse(  
      DBTOTAL2$tipo_veh ==  "Motocicleta",4,    ifelse(
        DBTOTAL2$tipo_veh == "Camión",  5,  ifelse(
          DBTOTAL2$tipo_veh == "Cabezal",  6,   ifelse(
            DBTOTAL2$tipo_veh ==  "Bus extraurbano", 7,  ifelse(
              DBTOTAL2$tipo_veh ==  "Jeep", 8,  ifelse(
                DBTOTAL2$tipo_veh ==  "Microbús", 9,  ifelse(
                  DBTOTAL2$tipo_veh ==  "Taxi", 10,  ifelse(
                    DBTOTAL2$tipo_veh == "Panel",  11,  ifelse(
                      DBTOTAL2$tipo_veh ==  "Bus urbano", 12,  ifelse(
                        DBTOTAL2$tipo_veh == "Tractor",  13,  ifelse(
                          DBTOTAL2$tipo_veh ==  "Moto taxi", 14,   ifelse(
                            DBTOTAL2$tipo_veh == "Furgón",  15,  ifelse(
                              DBTOTAL2$tipo_veh ==  "Grúa", 16,  ifelse(
                                DBTOTAL2$tipo_veh == "Bus escolar",  17,  ifelse(
                                  DBTOTAL2$tipo_veh ==  "Bicicleta", 18,  99
                                )))))))))))))))))))

DBTOTAL2$hora_ocu <- as.numeric(DBTOTAL$hora_ocu)
#Departamento 
DBTOTAL2$depto_ocu <- as.numeric(ifelse(DBTOTAL2$depto_ocu ==  "Guatemala" , 1, ifelse(
  DBTOTAL2$depto_ocu ==  "El Progreso", 2, ifelse(
    DBTOTAL2$depto_ocu ==  "Sacatepéquez", 3,   ifelse(  
      DBTOTAL2$depto_ocu ==  "Chimaltenango", 4,   ifelse(
        DBTOTAL2$depto_ocu ==  "Escuintla", 5,    ifelse(
          DBTOTAL2$depto_ocu ==  "Santa Rosa", 6,   ifelse(
            DBTOTAL2$depto_ocu ==  "Sololá", 7,   ifelse(
              DBTOTAL2$depto_ocu ==  "Totonicapán", 8,  ifelse(
                DBTOTAL2$depto_ocu ==  "Quetzaltenango", 9,    ifelse(
                  DBTOTAL2$depto_ocu ==  "Suchitepéquez", 10,    ifelse(
                    DBTOTAL2$depto_ocu ==  "Retalhuleu", 11,  ifelse( 
                      DBTOTAL2$depto_ocu ==  "San Marcos", 12,  ifelse(  
                        DBTOTAL2$depto_ocu ==  "Huehuetenango", 13,  ifelse( 
                          DBTOTAL2$depto_ocu ==  "Quiche", 14,  ifelse( 
                            DBTOTAL2$depto_ocu ==  "Baja Verapaz", 15,  ifelse( 
                              DBTOTAL2$depto_ocu ==  "Alta Verapaz", 16,  ifelse(
                                DBTOTAL2$depto_ocu ==  "Peten", 17,  ifelse( 
                                  DBTOTAL2$depto_ocu ==  "Izabal", 18,  ifelse( 
                                    DBTOTAL2$depto_ocu ==  "Zacapa", 19,  ifelse( 
                                      DBTOTAL2$depto_ocu ==  "Chiquimula", 20,  ifelse( 
                                        DBTOTAL2$depto_ocu ==  "Jalapa", 21, 22
                                      ))))))))))))))))))))))

#g_hora_5
#clasificacion extra de horas
DBTOTAL2$g_hora_5 <- as.numeric(ifelse(DBTOTAL2$g_hora_5 ==  "Mañana" , 1, ifelse(
  DBTOTAL2$g_hora_5 ==  "Mañana", 2, ifelse(
    DBTOTAL2$g_hora_5 ==  "Tarde",3, 4
  ))))
#color
DBTOTAL2$color_veh <- as.numeric(ifelse(DBTOTAL2$color_veh ==  "Rojo", 1 , ifelse(
  DBTOTAL2$color_veh ==  "Blanco", 2,  ifelse(
    DBTOTAL2$color_veh ==  "Azul", 3,   ifelse(  
      DBTOTAL2$color_veh ==  "Gris", 4,   ifelse(
        DBTOTAL2$color_veh ==  "Negro", 5,  ifelse(
          DBTOTAL2$color_veh ==  "Verde", 6,  ifelse(
            DBTOTAL2$color_veh ==  "Amarillo", 7,  ifelse(
              DBTOTAL2$color_veh == "Celeste",  8,  ifelse(
                DBTOTAL2$color_veh ==  "Corinto", 9,  ifelse(
                  DBTOTAL2$color_veh ==  "Cafe", 10,  ifelse(
                    DBTOTAL2$color_veh == "Beige",  11,  ifelse(
                      DBTOTAL2$color_veh ==  "Turqueza", 12,  ifelse(
                        DBTOTAL2$color_veh ==  "Marfil", 13,  ifelse(
                          DBTOTAL2$color_veh ==  "Anaranjado", 14,  ifelse(
                            DBTOTAL2$color_veh ==  "Aqua", 15,  ifelse(
                              DBTOTAL2$color_veh == "Morado",  16,  ifelse(
                                DBTOTAL2$color_veh == "Rosado",  17, 99
                              ))))))))))))))))))

str(DBTOTAL2)


#View(data_ine)

glimpse(DBTOTAL2)
str(DBTOTAL2)
head(DBTOTAL2)

usefull_vars <- DBTOTAL2[,c(3,7,8,10,12,13,17,19,20,21)] 
#View(usefull_vars)
str(usefull_vars)
head(usefull_vars)

data_motos_ine <- usefull_vars[usefull_vars$tipo_veh == 'Motocicleta',]
View(data_motos_ine)

table(data_motos_ine$sexo_per)
table(data_motos_ine$edad_per)
table(data_motos_ine$estado_con)
table(data_motos_ine$marca_veh)
table(data_motos_ine$color_veh)



edades_motos <- data_motos_ine$edad_per
edades_motos[edades_motos == 'Ignorada'] <- NA
edades_motos <- na.omit(edades_motos)

sexo_motos <- data_motos_ine$sexo_per
sexo_motos[sexo_motos == 'Ignorado'] <- NA
sexo_motos <- na.omit(sexo_motos)

generoEstado <- table(sexo_motos,data_motos_ine$estado_con)
plot(generoEstado, col = c("red", "blue"), main = "genero vs. estado")

chisq.test(generoEstado)

library(openxlsx)
excel_motos_ine <- write.xlsx(data_motos_ine,".xlsx")
saveWorkbook(excel_motos_ine, file = "motos_ine.xlsx", overwrite = TRUE)


#------------------------INCICIA ANALISIS DE ARBOLEs ----------------------
#semilla para el random
set.seed(1)
#DBTOTAL_ARBOLES <- DBTOTAL2[sample(nrow(DBTOTAL2), 500), ] # 2000 filas
DBTOTAL_ARBOLES <- DBTOTAL # 2000 filas

#DBTOTAL_ARBOLES<-DBTOTAL


DBTOTAL_ARBOLES$RESPUESTA <- as.factor(ifelse((DBTOTAL_ARBOLES$dia_sem_ocu!="") & (DBTOTAL_ARBOLES$depto_ocu=="Guatemala") , "CRASH","NONE"))
DBTOTAL_ARBOLES2 <-table(DBTOTAL_ARBOLES$RESPUESTA,DBTOTAL_ARBOLES$mes_ocu)
DBTOTAL_ARBOLES22 <- as.data.frame.matrix(DBTOTAL_ARBOLES2) 
#color

str(DBTOTAL_ARBOLES)
#separando la data

set.seed(123)
train <- sample_frac(DBTOTAL_ARBOLES, .70)
test <- setdiff(DBTOTAL_ARBOLES, train)


arbol_1 <- rpart(formula = train$RESPUESTA ~ ., data =  train[,c(6,10)])
rpart.plot(arbol_1, roundint = FALSE)
prediccion_1 <- predict(arbol_1, newdata = test, type= "class")
#cruzamos la prediccion con matriz de confusion 
confusionMatrix(table(prediccion_1, test[["RESPUESTA"]]))

# Ejecución del modelo de clasificación C5.0
modelo <- C5.0(train$RESPUESTA ~ .,data = train[,c(4,6,10)])
summary(modelo) # Información sobre el modelo
plot(modelo)  #Muestra un nodo en particular
prediccion <- predict(modelo,newdata=test)
confusionMatrix(table(prediccion, test$RESPUESTA))



modeloRF1<-randomForest(train$RESPUESTA~.,data=  train[,c(4,6,7,10)])
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<- test
testCompleto$predRF<-prediccionRF1
plot(modeloRF1)  #Muestra un nodo en particular
cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$RESPUESTA)
cfmRandomForest
#------------------------FINALIZA ANALISIS DE ARBOLEs ----------------------

#######________SHINY________#############
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  theme = "united",
                  "Laboratorio 9 ",
                  tabPanel("General",
                           mainPanel(
                             h2(id="h2", "Información general"),
                             
                             h4("El aumento de tráfico vehicular en la ciudad de Guatemala va en aumento. Si bien la cantidad de automotores crece, el espacio siempre sigue siendo el mismo. Un enfoque interesante es el de las motocicletas. Que si bien el uso de automóvil es uno que ocupa más espacio, las motocicletas ocupan menos y su accesibilidad es mayor."),
                             h4("Mediante un análisis de los datos obtenidos en los portales del INE y la SAT sobre automotores e importaciones, se llega a la conclusión que el bajo coste de una motocicleta, así como el aumento sistemático de impuestos de vehículos durante algunos años, da como resultado que se opte por motocicletas como medio de transporte."),
                             h2(id="h2","Datos relevantes"),
                             column(width = 3,
                                    h2('60.6%'),
                                    p('De las motocicletas analizadas poseen un cilindraje pequeño.')
                             ),
                             column(width = 3,
                                    h2('1.19%'),
                                    p('De las motocicletas son de alto cilindraje, el motivo es tanto el precio como el valor de los impuestos.')
                             ),
                             column(width = 3,
                                    h2('5'),
                                    p( 'Años fueron analizados y en todos, se nota un aumento del impuesto de importación para vehículos. Lo que propicia el uso de motos.')
                             ),
                           ),
                           tags$style(HTML("#h2{color: #F23005;}")),
                  ),
                  tabPanel("Bayes", 
                           h1("Header 1"),
                           tags$style(HTML("#h2{color: #F23005;}")),
                  ),
                  tabPanel("Arboles",
                           h2(id="h2","Árboles Binarios"),
                           h4("Un Árbol de Decisión (o Árboles de Decisiones) es un método analítico que a través de una 
                           representación esquemática de las alternativas disponible facilita la toma de mejores decisiones, 
                              especialmente cuando existen riesgos, costos, beneficios y múltiples opciones."),
                           h4("En este laboratorio, se tomó árboles de decisión para tomar los valores de horas, días, etc en el que sucedían accidentes. En especial con cierto tipo de vehículo.
                              Los resultados de asociar la hora de ocurrencia se muestran en la tabla debajo.
                              Se nota hay horas que son propensas a sufrir accidentes, como la madrugada. En el gráfico derecho, 
                              el factor de NONECRASH es más bajo cuando se acerca entre la 1,2,3,4,5 de la madrugada. "),
                           plotOutput(outputId = "graficoArbol"),
                           h2(id="h2","Otros datos interesantes"),
                           h4("Mediante predicción con árboles de decisión, se tomaron valores tanto de hora, día de la semana, etc para demostrar si es posible predecir los accidentes. 
                              Con un 70% de accuracy, estas variables son buenas para la predicción. Los resultados se muestran en el siguiente texto"),
                           h3(id="h3","Accuracy : 0.6521"),
                           h3(id="h3","95% CI : (0.6415, 0.6627) "),
                           h3(id="h3","No Information Rate : 0.6521"),
                           h3(id="h3","P-Value [Acc > NIR] : 0.5052"),
                           tags$style(HTML("#h2{color: #F23005;}")),
                           tags$style(HTML("#h3{color: #F2441D;}")),
                           ),
                  tabPanel("Regresión lineal")
                  
                )
)


# Define server function  
server <- function(input, output) {
  output$graficoArbol<- renderPlot({
    
    set.seed(123)
    train <- sample_frac(DBTOTAL_ARBOLES, .70)
    test <- setdiff(DBTOTAL_ARBOLES, train)
    
    # Ejecución del modelo de clasificación C5.0
    modelo <- C5.0(train$RESPUESTA ~ .,data = train[,c(6,4,10)])
    summary(modelo) # Información sobre el modelo
    plot(modelo)  #Muestra un nodo en particular
    prediccion <- predict(modelo,newdata=test)
    #confusionMatrix(table(prediccion, test$RESPUESTA))
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
