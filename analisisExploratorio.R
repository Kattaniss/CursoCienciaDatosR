# -----------------------------------------------------------------
# Exploración del contenido
# -----------------------------------------------------------------
class(iris)  # Clase del objeto
typeof(iris) # Tipo del objeto
str(iris)    # Información sobre su estructura

summary(iris) # Resumen de contenido
head(iris)  # Primeras filas 
tail(iris)  # Últimas filas
# Selección de filas y columnas
iris$Sepal.Length[which(iris$Species == 'versicolor')] 

# -----------------------------------------------------------------
# Estadística descriptiva
# -----------------------------------------------------------------
valores <- as.integer(runif(50, 1, 10))
unlist(list(
  media = mean(valores), 
  desviacion = sd(valores), 
  varianza = var(valores),
  minimo = min(valores), 
  maximo = max(valores),
  mediana = median(valores),
  rango = range(valores),
  quartiles = quantile(valores)))

mean(iris$Sepal.Length)
lapply(iris[,1:4], mean)
mean(iris$Sepal.Length[which(iris$Species == 'versicolor')])
mean(subset(iris, Species == 'versicolor', select=Sepal.Length)$Sepal.Length)
sapply(unique(iris$Species), function(specie) mean(iris$Sepal.Length[iris$Species == specie]))

# Hmisc
if(!is.installed('Hmisc'))
  install.packages('Hmisc')
library('Hmisc')
describe(ebay)

# -----------------------------------------------------------------
# Agrupamiento de datos
# -----------------------------------------------------------------
table(iris$Sepal.Length,iris$Species)  # Conteo para cada lóngitud de sépalo por especie
tail(table(ebay$sellerRating, ebay$currency))  # Los vendedores con mejor reputación operan en dólares

cortes <- seq(from=4, to=8, by=0.5)
seplen <- cut(iris$Sepal.Length,breaks=cortes)  # Discretizar la longitud de sépalo
table(seplen, iris$Species)  

# split, sample, subset
bySpecies <- split(iris,iris$Species) # Separar en grupos segun un factor
str(bySpecies)
mean(bySpecies$setosa$Sepal.Length)

str(covertype)  
subset(covertype, slope > 45 & soil_type == '1', select=c(elevation, slope, class)) # Selección de filas y columnas

subcovertype <- covertype[sample(1:nrow(covertype), nrow(covertype)*.1),] # Selección aleatoria
str(subcovertype)

# -----------------------------------------------------------------
# Ordenación de datos
# -----------------------------------------------------------------
sort(valores)
order(valores)
rank(valores)
rank(valores, ties.method='first')
# Ordenar un data frame por una cierta columna
sortedIris <- iris[order(iris$Petal.Length),]
head(sortedIris)

#-----------------------------------------------------------------
# Gráfica de dispersión
#-----------------------------------------------------------------
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species,  pch = 19,
     xlab = 'Longitud del pétalo', ylab = 'Ancho del pétalo')
title(main = 'IRIS', 
      sub = 'Exploración de los pétalos según especie', 
      col.main = 'blue', col.sub = 'blue')
legend("bottomright", legend = levels(iris$Species),
       col = unique(iris$Species), ncol = 3, pch = 19, bty = "n")

# -----------------------------------------------------------------
# Gráfica de cajas
# -----------------------------------------------------------------
plot(iris$Petal.Length ~ iris$Species)

boxplot(iris$Petal.Length ~ iris$Species)
title(main = 'IRIS', ylab = 'Longitud pétalo', sub = 'Análisis de pétalo por familia')

# -----------------------------------------------------------------
# Histogramas
# -----------------------------------------------------------------
hist(iris)

hist(iris$Sepal.Width, breaks = 12, col = rainbow(12), main = 'Ancho de sépalo', xlab = "Centímetros")
