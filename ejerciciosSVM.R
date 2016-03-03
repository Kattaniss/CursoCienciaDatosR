# -----------------------------------------------------------------
# Clasificacion con caret - SVM
# -----------------------------------------------------------------

library(caret)
library(kernlab)
library(e1071)

# Particionamos los datos
set.seed(4242)
indices <- createDataPartition(iris$Species, p = .75, list = FALSE)
training = iris[indices, ]
test = iris[-indices, ] 

# Exploramos la distribución de las muestras según las distintas variables
featurePlot(training[,-5], y = training$Species,  plot='ellipse')
featurePlot(training[,-5], y = training$Species,  plot='box')

# Configuramos la selección del modelo durante entrenamiento
train10CV <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Entrenamos una SVM con kernel RBF
svmRBF <- train(Species ~ ., data = training,
                method = "svmRadial", trControl = train10CV,
                preProc = c("center", "scale"))
svmRBF # Examinamos el resultado de entrenamiento

confusionMatrix(svmRBF) # Matriz de confusión de resultados en entrenamiento

pred <- predict(svmRBF, test)  # Anñalisis de resutados sobre datos de test
confusionMatrix(pred, test$Species)

pred <- predict(svmRBF, test, type = "prob") # Predicción de probabilidades por clase
pred$obs <- test$Species
pred$pred <- predict(svmRBF, test)
multiClassSummary(pred, lev = levels(pred$obs))

# Visualizar los fallos en la predicción
ggplot(pred, aes(x = pred, y = obs)) + geom_jitter(position = position_jitter(width = 0.25, height = 0.25))

# Comparación de la SVM con otros modelos
knnModel <- train(Species ~ ., data = training, method = "knn", trControl = train10CV, preProc = c("center", "scale"))
ldaModel <- train(Species ~ ., data = training, method = "lda", trControl = train10CV, preProc = c("center", "scale"))
svmLinear <- train(Species ~ ., data = training, method = "svmLinear", trControl = train10CV, preProc = c("center", "scale"))
rfModel <- train(Species ~ ., data = training, method = "rf", trControl = train10CV, preProc = c("center", "scale"))

summary(resamples(list(svmRBF = svmRBF, svmLinear = svmLinear, kNN = knnModel, LDA = ldaModel, RF = rfModel)))

# Visualización de la SVM utilizando el paquete e1071
svm <- svm(Species ~ ., data = training)
plot(svm, test, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4)) # Al ser multiclase precisa información sobre dimensiones adicionales

data(spam)  # Probamos con un dataset binario
indices <- createDataPartition(spam$type, p = .75, list = FALSE)
training = spam[indices, ]
test = spam[-indices, ] 

svm <- svm(type ~ ., data = training)  # Entrenar y mostrar el modelo
plot(svm, test, free ~ remove)

# Visualizar los fallos en la predicción
pred <- data.frame(pred = predict(svm, test), obs = test$type)
ggplot(pred, aes(x = pred, y = obs)) + geom_jitter(position = position_jitter(width = 0.25, height = 0.25))
