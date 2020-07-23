# install.packages("rela")
# install.packages("psych")
# install.packages("FactoMineR")
# install.packages("corrplot")
library(rela)
library(psych)
library(FactoMineR)

library(corrplot)


datos<-read.csv("Ejemplo.csv",stringsAsFactors = F)

#Se debe analizar si se puede usar el anÃ¡lisis factorial 
#para formar las combinaciones lineales de las variables
pafDatos<-paf(as.matrix(datos[,2:8]))
pafDatos$KMO #0.42 La adecuaciÃ³n a la muestra es mala
pafDatos$Bartlett #198.58 Mientras mas alto sea mejor
summary(pafDatos)

#Pero hay que ver el nivel de significaciÃ³n de la prueba
cortest.bartlett(datos[,-1])
#el valor p es de 6.87*10-31 lo que lo hace muy pequeÃ±o
#mucho menor a 0.05, esto nos dice que el anÃ¡lisis factorial
#si podrÃ­a funcionar

#se muestra la matriz de correlaciÃ³n
cor(datos[,-1],use = "pairwise.complete.obs")
#se puede ver que el dioxido de azufre estÃ¡ correlacionado con 
# las empresas que tienen mÃ¡s de 20 personas trabajando y posiblemente 
# con la poblaciÃ³n del lugar, a su vez estÃ¡s dos Ãºltimas estÃ¡n
#altamente relacionadas entre si


#Esta funciÃ³n normaliza los datos de una vez
compPrinc<-prcomp(datos[,2:8], scale = T)
compPrinc
# Standard deviations (1, .., p=7):
#   [1] 1.65170 1.22977 1.18109 0.94445 0.58888 0.31668 0.15973
# 
# Rotation (n x k) = (7 x 7):
#   PC1       PC2      PC3       PC4      PC5       PC6
# SO2        0.48969882  0.084576 -0.01435  0.404210  0.73039 -0.183346
# Neg.Temp  -0.31537069 -0.088638 -0.67714 -0.185228  0.16247 -0.610661
# Empresas   0.54116870 -0.225881 -0.26716 -0.026272 -0.16410  0.042734
# Poblacion  0.48758811 -0.282004 -0.34484 -0.113404 -0.34910  0.087863
# Viento     0.24987493  0.055471  0.31127 -0.861901  0.26825 -0.150054
# Precip     0.00018731  0.625879 -0.49204 -0.183937  0.16060  0.553574
# Dias       0.26017907  0.677967  0.10958  0.109761 -0.43997 -0.504947
# PC7
# SO2       -0.1495293
# Neg.Temp   0.0236641
# Empresas   0.7451809
# Poblacion -0.6491255
# Viento    -0.0157654
# Precip     0.0103153
# Dias      -0.0082174

summary(compPrinc)
# Importance of components:
#                         PC1   PC2   PC3   PC4    PC5    PC6     PC7
# Standard deviation     1.65 1.230 1.181 0.944 0.5889 0.3167 0.15973
# Proportion of Variance 0.39 0.216 0.199 0.127 0.0495 0.0143 0.00364
# Cumulative Proportion  0.39 0.606 0.805 0.932 0.9820 0.9964 1.00000
compPrincPCA<-PCA(datos[,-1],ncp=ncol(datos[,-1]), scale.unit = T)

summary(compPrincPCA)

#Se obtiene el scree plot de las componentes principales.
# Como se ve hacen falta 4 de las 7 componentes para explicar más del 80% de la variabilidad
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))

# En la siguiente grÃ¡fica se ilustra la calidad de la representaciÃ³n de los componentes en las dos primeras dimensiones.
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)



# TRABAJANDO CON IRIS
pafIris <- paf(iris[,1:4])
pafIris$KMO #La adecuación muestral no es buena

irisPCA <- PCA(iris[,1:4])
summary(irisPCA)
# Con las primeras 2 dimensiones se explica el 95% de la variancia del conjunto de datos
# En la primera dimensión están muy bien representadas las variables Petal.Length y Petal.Width
# En la segunda dimensión se pueden incluir las variables Sepal.Width y Sepal.Length
# Estas dos primeras componentes se puede interpretar de la siguiente forma:
# PC1: Medidas del Pétalo
# PC2: Medidas del tallo

#Scree Plot
fviz_eig(irisPCA, addlabels = TRUE, ylim = c(0, 80))
#Representación de las variables en cada componente
fviz_pca_var(irisPCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Representación de cada variable en cada componente
var<-get_pca_var(irisPCA)
corrplot(var$cos2, is.corr = F)
#Según la representación de las variables en las componentes se podría incluir en la dimensión 1 pero la interpretabilidad del componente principal sería más complicada.