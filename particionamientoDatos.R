# -----------------------------------------------------------------
# Particionamiento de datos
# -----------------------------------------------------------------

# Primeras n filas para training restantes para test
nTraining <- as.integer(nrow(iris)*.75)
training <- iris[1:nTraining,]
test <- iris[(nTraining+1):nrow(iris),]
nrow(training) + nrow(test) == nrow(iris)
summary(training$Species) / nrow(training) * 100  # Porcentaje de muestras por clase
summary(test$Species)  / nrow(test) * 100

# Otra forma
set.seed(4242)
indices <- sample(1:nrow(iris), nTraining)
particion <- list(training=iris[indices,], test=iris[-indices,])
lapply(particion,nrow)
particion$test
summary(particion$training$Species) / nrow(particion$training) * 100 # Porcentaje de muestras por clase
summary(particion$test$Species)  / nrow(particion$test) * 100

# Particionamiento estratificado usando el paquete carte
library(caret)

set.seed(4242)
indices <- createDataPartition(iris$Species, p = .75, list = FALSE)
particion <- list(training=iris[indices,], test=iris[-indices,])
lapply(particion,nrow)
summary(particion$training$Species) / nrow(particion$training) * 100 # Porcentaje de muestras por clase
summary(particion$test$Species)  / nrow(particion$test) * 100

folds <-createFolds(iris$Species, k = 10)
particion <- lapply(folds, function(indices) list(training=iris[-indices,], test=iris[indices,]))
lapply(particion$Fold04, nrow)
summary(particion$Fold04$training$Species) / nrow(particion$Fold04$training) * 100 # Porcentaje de muestras por clase
summary(particion$Fold04$test$Species)  / nrow(particion$Fold04$test) * 100
