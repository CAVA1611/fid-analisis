#title: "No_Supervisado"
#author: "Belen Paredes y Cesar Velasco"
#date: "21/1/2021"
#output: html_document

#INTRODUCCIÓN

#En este análisis no supervisado  se utiliza un  dataset de las variantes tintas del vino portugués "Vinho Verde", donde se desea optener grupos optimos a partir de las caracteristicas fisicas y quimicas

## Introducción teórica

#El clustering es una tarea que tiene como finalidad principal lograr el agrupamiento de conjuntos de objetos no etiquetados, para lograr construir subconjuntos de datos conocidos como Clusters.  

#Métodos de clusterización de datos a utilizar: 

# K-means: también utiliza las distancias entre puntos. Y agrupo los clusters según lo cerca que están de los centros de gravedad de los clusters. A priori, tienes que saber cuántos clusters vas a crear para que el algoritmos sepa dónde colocar cada punto.
#Gaussian mixture models (GMM): utiliza modelos gaussianos o modelos de distribuciones normales de diferentes formas para que puedas crear grupos en forma de elipses. Para cada punto se le calcula la probabilidad de pertenencia a cada modelo gaussiano



#Paquetes vistos en clases

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#Paquetes  consultados

install.packages("cluster")
library(cluster)
library(corrplot)
library(factoextra)
library(mclust) # https://towardsdatascience.com/gaussian-mixture-models-d13a5e915c8e
library(FactoMineR)
library(rattle) #https://rstudio-pubs-static.s3.amazonaws.com/317830_dde0f80173c047a2badbbe8918e95048.html


#lectura del dataset winequality-red.csv

wine <- read.csv("winequality-red.csv")

#Se realiza la revision de la correlacion entre columnas para verificar 
#cuales nos ayudaran a determinar de mejor manera la calidad de vino

#Empezamos el algoritmo observando las variables con un matrixplot:

cor(wine$quality, wine$density)
cor(wine$quality, wine$alcohol)
cor(wine$quality, wine$pH)
cor(wine$quality, wine$sulphates)

# grafica de correlacion
correlations <- cor(wine) # correlation matrix
corrplot(correlations, method="circle") # corrplot


#Segun se observa en la grafica, exite una mayor correlacion para
#determinar la mayor calidad del vino con los datos de la columna 
#alcohol y sulphates

plot(x=wine$alcohol, y=wine$sulphates)

#Seleccion de las columnas alcohol y sulphates

wine_s_a <- wine[,10:11]


#Para determinar el valor optimo de k usaremos dos metodos:

# #determinar el numero ideal de cluster grafica con el metodo  Metodo WSS


wss<- 0 
for (i in 1:12) {
  out <- kmeans(wine_s_a, centers = i, nstart = 20)
  wss[i] <- out$tot.withinss
}

plot(1:12, wss,type = "b", xlab = "# Cluster", ylab = "WSS")

#determinar el numero ideal de cluster grafica con el metodo "gap_stat"

fviz_nbclust(wine_s_a, kmeans, method = "gap_stat")

#Verificacion del numero de k con la funcion fviz_nbclust de la libreria library(factoextra)

fviz_nbclust(wine_s_a, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#Por medio de la grafica se verifica que el numero optimo para K es 4

# k-means
wine_clus <- kmeans(wine_s_a, center= 4, nstart = 20) 
plot(wine_s_a, col=wine_clus$cluster)
points(wine_clus$centers, cex=2, col=12, pch=19)
clusplot(wine_s_a, wine_clus$cluster, color=TRUE, shade=TRUE)
eclust(wine_s_a, "kmeans", k = 4, nstart = 20, graph = TRUE)

# GMM
gmm_wine = Mclust(wine_s_a, G = 4)
summary(gmm_wine, parameters = TRUE)
plot(gmm_wine, what = "classification")



# Bibliografía

#[1] Dataset   =  https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009

