
library(tidyverse)
library(ggplot2)
library(GGally)
library(psych)
library(cluster)

url_datos <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'

datos.duros <- read.csv(url_datos)
data <- data.frame(datos.duros)

colnames(data) <- c("id",
                    "clump.thickness",
                    "unif.cell.size",
                    "unif.cell.shape",
                    "marg.adhesion",
                    "epith.cell.size",
                    "bare.nuclei",
                    "bland.chroma",
                    "norm.nucleoli",
                    "mitoses",
                    "class")


bool.values <- data$bare.nuclei=='?'
data <- data[!bool.values,]

#Para confirmar que se eliminaron los ?
sum(data$bare.nuclei=='?')


dd <- data

#En el caso de que se quiera cambiar los 4 y 2, por M y B
dd$class <- replace(dd$class,dd$class==2,'Benigno')
dd$class <- replace(dd$class,dd$class==4,'Maligno')

#Se aplica factor
dd[["class"]] <- factor(dd[["class"]])
data[["class"]] <- factor(data[["class"]])
data[["bare.nuclei"]] <- factor(data[["bare.nuclei"]])

#Se pasa a numerico los valores de bare nuclei.
data$bare.nuclei <- as.numeric(data$bare.nuclei)
data$class <- as.numeric(data$class)

#Se elimina la columna ID
data$id <- NULL

##############################
library("e1071")
library("caret")

#Se guardan los datos dentro de una variable auxiliar
data.aux <- dd

#Se separan los datos de entrenamiento (70%), y los de testing (30%)
set.seed(1313)
training.index = createDataPartition(data.aux$class, p=0.7)$Resample1
training.set = data.aux[training.index, ]
test.set = data.aux[-training.index, ]

#Se genera el modelo bayesiano
bayesian.classifier = naiveBayes(class ~ ., data = training.set)

#Se prueba el modelo generado
bayesian.results = predict(object = bayesian.classifier, test.set)
test.set["predicted"] = bayesian.results

#Se genera la matriz de conficion.
conf.matrix.cb = confusionMatrix(table(test.set$class, test.set$predicted))
print(conf.matrix.cb)

