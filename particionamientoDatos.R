# -----------------------------------------------------------------
# Particionamiento de datos
# -----------------------------------------------------------------

partitionDistribution <- function(partition) {
  print(lapply(partition, nrow))
  summary(partition$training$Species) / nrow(particion$training) * 100 # Porcentaje de muestras por clase
  summary(partition$test$Species)  / nrow(particion$test) * 100
}

# Primeras n filas para training restantes para test
nTraining <- as.integer(nrow(iris)*.75)
particion <- list(training = iris[1:nTraining, ], test = iris[(nTraining+1):nrow(iris),])
partitionDistribution(particion)

# Selección aleatoria de muestras
set.seed(4242)
indices <- sample(1:nrow(iris), nTraining)
particion <- list(training=iris[indices,], test=iris[-indices,])
partitionDistribution(particion)

# Particionamiento estratificado usando el paquete carte
library(caret)

set.seed(4242)
indices <- createDataPartition(iris$Species, p = .75, list = FALSE)
particion <- list(training=iris[indices,], test=iris[-indices,])
partitionDistribution(particion)

# Creación de múltiples particiones
folds <-createFolds(iris$Species, k = 10)
particion <- lapply(folds, function(indices) list(training=iris[-indices,], test=iris[indices,]))
partitionDistribution(particion$Fold04)
