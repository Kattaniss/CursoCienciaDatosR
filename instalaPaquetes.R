install.packages("caret")
# LINUX: system('wajig install libgtk2.0-dev') # Use 'sudo apt-get install wajig' at the command line if needed, then install Gtk headers
install.packages('RGtk2')
install.packages("rattle")
install.packages("neuralnet")
install.packages("autoencoder")
install.packages("ggplot2")
install.packages("h2o", dependencies = c("Depends", "Suggests"))
install.packages("pROC")

library(rattle)  # WINDOWS: Choose to install Gtk
rattle()

library(h2o)
localH2O <- h2o.init() # It should indicate that H2O is initialized, as well as the Java VM version

# Download https://github.com/fcharte/CursoCienciaDatosR/blob/master/data/datosTrabajo.RData and save it in working directory

load('datosTrabajo.RData')
# Look for the previous file at the "Files" panel, and click it to load it. All the data should appear in the Environment tab

library(caret)
library(neuralnet)
library(autoencoder)
library(ggplot2)
