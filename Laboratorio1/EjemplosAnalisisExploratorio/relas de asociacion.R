# REGLAS DE ASOCIACI?N
install.packages("arules")
library(arules)
datos=mtcars
# El m?nimo nivel de soporte y confianza aceptados
str(datos)
datos$cyl <- as.factor(datos$cyl)
datos$am <- as.factor(datos$am)
datos$vs <- as.factor(datos$vs)
datos$gear <- as.factor(datos$gear)

reglas<-apriori(datos[, c(2,8,9,10)], parameter = list(support = 0.2,
                                        confidence = 0.70,
                                        target = "rules"))
inspect(reglas)
