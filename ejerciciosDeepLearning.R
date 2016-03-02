# -----------------------------------------------------------------
# Clasificacion - Deep Learning - Autoencoder
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

# -----------------------------------------------------------------
# Clasificacion - Deep Learning - DBN
# -----------------------------------------------------------------

library(h2o)

# Usamos solo la partición de test
data = read.csv('data/mnist_test.csv')
set.seed(4242)
data <- data[sample(1:nrow(data), 1000), ]
data[ , 1] <- as.factor(data[ , 1])
table(data[ , 1]) # Número de muestras por clase

# Examinamos el contenido de algunas muestras
prev.conf <- par(mfrow = c(5, 5), mai = c(0,0,0,0))
digits <- lapply(1:25, function(row) {
  digit <- matrix(as.numeric(data[row, 2:785]), ncol = 28, byrow = FALSE)
  image(digit[ , 28:1], col = gray(255:0 / 255), axes = FALSE)
})
par(prev.conf)

# Particionamos los datos
indices <- createDataPartition(data[ , 1], p = .75, list = FALSE)
training = data[indices, ]
test = data[-indices, ] 

# Inicialización de H2O y entrenamiento del modelo
h2oInterface <- h2o.init()

# Enviar los datos de los data.frames a la instancia de H2O
h2oTrain <- as.h2o(training)
h2otest  <- as.h2o(test)

# Entrenamos el modelo
modelo <- h2o.deeplearning(2:785, 1, h2oTrain, hidden = c(600,300), epochs = 100)
plot(modelo)  # Evolución del error a medida que ha progresado el entrenamiento

pred <- h2o.predict(modelo, h2otest)  # Predicciones para datos de test

# Evaluación del rendimiento del modelo
h2o.performance(modelo, train = TRUE)
h2o.performance(modelo, h2otest)

h2o.shutdown()



