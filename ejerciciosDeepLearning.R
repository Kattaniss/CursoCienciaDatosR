# -----------------------------------------------------------------
# Clasificacion - Deep Learning
# -----------------------------------------------------------------

library(autoencoder)

# Datos de iris quitando la clase para generar un autoencoder que comprima la representación
data <- as.matrix(iris[ , -5])
encoder <- autoencode(data, N.hidden = 2,  # La capa interna tendrá 2 neuronas (bidimensional)
                      unit.type="tanh", lambda = 0.002, beta = 5, rho = 0.5, epsilon = 0.001, 
                      max.iteration = 1000, rescale.flag = TRUE, optim.method="BFGS")

# Obtenemos la versión comprimida del dataset
compressed <- data.frame(predict(encoder, data, hidden.output = TRUE, FALSE)$X.output)
compressed$Species = iris$Species

# Ahora solo hay dos dimensiones y las muestras son casi separables
qplot(X1, X2, data = compressed, color = Species)

# Entrenamiento de una SVM usando la versión comprimida
set.seed(4242)
indices <- createDataPartition(compressed$Species, p = .75, list = FALSE)
training = compressed[indices, ]
test = compressed[-indices, ] 

# Configuramos la selección del modelo durante entrenamiento
train10CV <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Entrenamos una SVM con kernel RBF
svmRBF <- train(Species ~ ., data = training,
                method = "svmRadial", trControl = train10CV,
                preProc = c("center", "scale"))

pred <- predict(svmRBF, test)        # Análisis de resutados sobre datos de test
confusionMatrix(pred, test$Species)  # Comparar con los resultados de la SVM original
