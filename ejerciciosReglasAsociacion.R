# -----------------------------------------------------------------
# Reglas de asociación
# -----------------------------------------------------------------

library(arules)
library(arulesViz)

# Podemos leer archivos de transacciones en formato "single" y "basket" (por defecto)
tr1 <- read.transactions("data/Compras2.csv")
tr2 <- read.transactions("data/Compras1.csv", format = "single", cols = 1:2)
str(tr1)
itemFrequency(tr1)
itemFrequency(tr2)
itemFrequencyPlot(tr1)

# También podemos generar las transacciones desde un data.frame
load("data/titanic.rdata")
titanic
str(titanic)  # Es un data.frame en el que todas las columnas son nominales (factors)
titanic <- as(titanic, "transactions")  # Convertir los datos a formato de transacción
titanic

itemFrequencyPlot(titanic)  # Soporte de los items que aparecen en las transacciones

reglas <- apriori(titanic)  # Obtener reglas con parámetros por defecto
inspect(reglas)

# Métricas de calidad de las reglas obtenidas
quality(reglas)
quality(reglas)$conviction <- interestMeasure(reglas, "conviction", titanic)  # Obtener métrica adicional de calidad
quality(reglas)

plot(reglas) # Exploración de las reglas
str(reglas)

# Filtrar las reglas que cumplen una condición
mejores <- reglas[quality(reglas)$lift > 1]
inspect(mejores)

# Obtener reglas con un cierto valor en el consecuente y un soporte menor
reglas <- apriori(titanic, appearance = list(rhs = c("Survived=Yes"), default = "lhs"), parameter = list(supp = 0.005))
inspect(reglas)
plot(reglas)

# Relaciones entre los items que generan las reglas
plot(reglas, method="graph", control=list(type="items"))
plot(reglas, method = "paracoord", control=list(reorder=TRUE))

data("AdultUCI")
Adult <- as(AdultUCI, "transactions")  # La conversión falla, hay columnas no discretas
str(AdultUCI)    # Exploramos los datos que contiene para comprobar los atributos continuos

# Discretizamos las columnas con valores continuos siguiendo diferentes estrategias
AdultUCI$fnlwgt <- discretize(AdultUCI$fnlwgt)   # Discretización en varios intervalos
AdultUCI$`education-num` <- NULL    # Eliminar el atributo (en este caso es redundante)

summary(AdultUCI$age) # Converimos la edad numérica en un conjunto de valores nominales
intervalos <- cut(AdultUCI$age, c(0, 18, 30, 45, 65, 91))
valores <- ordered(intervalos, labels = c("Menor", "Joven", "Medio", "Maduro", "Mayor"))
AdultUCI$age <- valores

# Discretizamos el resto de atributos continuos
AdultUCI$`capital-gain` <- discretize(AdultUCI$`capital-gain`)
AdultUCI$`capital-loss` <- discretize(AdultUCI$`capital-loss`)
AdultUCI$`hours-per-week` <- discretize(AdultUCI$`hours-per-week`)

Adult <- as(AdultUCI, "transactions")  # Ahora la conversión funciona sin problemas
itemFrequencyPlot(Adult, support = 0.1, cex.names = 0.75)

# La configuración por defecto genera un gran número de reglas
reglas <- apriori(Adult)
plot(reglas, method = "grouped", cex.names = 0.6)  # Exploramos antecedentes y consecuentes

# Buscamos reglas de asociación que impliquen un nivel de ingresos alto
reglas <- apriori(Adult, parameter = list(supp = 0.01, confidence = 0.5), appearance = list(rhs = c("income=large"), default = "lhs"))
inspect(head(sort(reglas, by = "confidence"), n = 5)) # Ordenamos por confianza y examinamos las primeras 5 reglas
plot(reglas)  # Gráfica con la distribución de las reglas encontradas

# Ajustar soporte y confianza para reducir el número de reglas
reglas <- apriori(Adult, parameter = list(supp = 0.012, confidence = 0.55), appearance = list(rhs = c("income=large"), default = "lhs"))
inspect(reglas)
plot(reglas, method = "graph", control = list(type = "items"))
plot(reglas, method = "paracoord", control=list(reorder=TRUE))

# Recursos adicionales sobre reglas de asociación - http://www.r-bloggers.com/examples-and-resources-on-association-rule-mining-with-r/