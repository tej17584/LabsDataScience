# REGLAS DE ASOCIACIÓN
install.packages("arules")
library(arules)
# El mínimo nivel de soporte y confianza aceptados
reglas<-apriori(datos, parameter = list(support = 0.2,
                                        confidence = 0.70,
                                        target = "rules"))
