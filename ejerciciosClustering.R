# -----------------------------------------------------------------
# Clustering (agrupamiento de datos)
# -----------------------------------------------------------------
library(cluster)
library(fpc)
library(stats)
library(ggplot2)

# Agrupamiento con el algoritmo k-means de iris, usando dos de sus atributos y estableciendo el número de grupos
set.seed(4242)
clusters <- kmeans(iris[, c("Petal.Length", "Petal.Width")], 3)
clusters

table(iris$Species, clusters$cluster)  # Examinamos la clase de las muestras contenidas en cada cluster

# Visualización gráfica de los clusters
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(aes(color = as.factor(clusters$cluster), shape = iris$Species), size = 5)
clusplot(iris[, -5], clusters$cluster, color = TRUE, lines = 0)   # Paquete cluster
plotcluster(iris[, -5], clusters$cluster)                         # Paquete fpc

# Estimación del número de grupos a usar
datos <- iris[, -5]
SSE <- rbind(data.frame(Grupos = 1, SSE = nrow(datos) * sum(apply(datos, 2, var))), 
             data.frame(Grupos = 2:10, SSE = unlist(lapply(2:10, function(ngrupos) sum(kmeans(datos, ngrupos)$withinss)))))
ggplot(SSE, aes(Grupos, SSE)) + geom_line() + scale_x_continuous(breaks = 1:10)

# La función pamk sugiere en el miembro nc el número óptimo de clusters a usar
clusters <- kmeans(datos, pamk(datos)$nc)
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(aes(color = as.factor(clusters$cluster), shape = iris$Species), size = 5)
clusplot(iris[, -5], clusters$cluster, color = TRUE, lines = 0)   

# ---- Agrupamiento jerárquico  ------

# Parte de las muestras de iris
datos <- iris[sample(nrow(iris), 50), ]

distancias <- dist(datos[, -5]) # Calculamos las distancias entre muestras
clusters <- hclust(distancias, method = "average")  # y Generamos la jerarquía de clusters

# Visualización del dendograma
plot(clusters, labels = datos$Species)

# Análisis de un posible corte con N grupos
rect.hclust(clusters, k = 4, border = "red")

# Comparar dos soluciones obteniendo estadísticas sobre la mejor
cluster.stats(distancias, cutree(clusters, 4), cutree(clusters, 5))

# ---- Agrupamiento basado en modelo ----
library(mclust)

clPairs(iris[, -5], iris$Species)  # Análisis previo de grupos respecto a la clase

modelo <- Mclust(iris[,-5]) # Generación del modelo de acuerdo a parámetros BIC
summary(modelo)
table(modelo$classification, iris$Species)  # Muestras por clase en cada cluster
plot(modelo, what = "classification")

# Modelo mediante análisis discriminante
modelo <- MclustDA(iris[, -5], iris$Species, modelType = "EDDA")
summary(modelo)
plot(modelo, what = "scatterplot")
plot(MclustDR(modelo), what="boundaries")

# Comprobación del error cometido
unlist(cvMclustDA(modelo, nfold = 10)[2:3])
