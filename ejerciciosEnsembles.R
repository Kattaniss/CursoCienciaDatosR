# -----------------------------------------------------------------
# Clasificacion con caret - Ensembles
# -----------------------------------------------------------------

library(caret)
trellis.par.set(caretTheme())

# Particionamos los datos
data(spam)
spam <- spam[sample(nrow(spam), 1250), ]

set.seed(4242)
indices <- createDataPartition(spam$type, p = .75, list = FALSE)
training = spam[indices, ]
test = spam[-indices, ] 

# Configuramos la selección del modelo durante entrenamiento
train10CV <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Entrenamos los modelos multi-clasificadores
gbm <- train(type ~ ., data = training, method = "gbm", trControl = train10CV)         # Boosting
bag <- train(type ~ ., data = training, method = "treebag", trControl = train10CV)     # Bagging
rf <- train(type ~ ., data = training, method = "rf", trControl = train10CV)           # Random forest

plot(gbm)  # Explorar la influencia de parámetros en el modelo
plot(rf)
plot(rf$finalModel, main = "Error vs Número de árboles")

varImpPlot(rf$finalModel, main = "Relevancia de características") # Random Forest facilita un ranking de importancia de las características

# Obtención de uno de los árboles del modelo
getTree(rf$finalModel, 1, labelVar = TRUE)

# Evaluación de los modelos con datos de test
confusionMatrix(predict(gbm, test), test$type)
confusionMatrix(predict(bag, test), test$type)
confusionMatrix(predict(rf, test), test$type)

# Comparar los modelos entre sí
diff <- resamples(list(Boosting = gbm, Bagging = bag, RF = rf))
summary(diff)
dotplot(diff)

# Visualización de árboles individuales
library(party)
arbol <- ctree(Species ~ ., data = iris)
plot(arbol)
