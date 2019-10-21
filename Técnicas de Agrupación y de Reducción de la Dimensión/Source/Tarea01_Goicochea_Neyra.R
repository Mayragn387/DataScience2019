###-------------------------Header------------------------
#     Elaborador por Mayra Goicochea
#     Fecha:  15/10/2019
#     Source: Iris (package Datasets)
###-----------------------Librerias-----------------------
library(datasets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
###-------------------------------------------------------
# Carga Dataset
data(iris)

# Carga Resumen
summary(iris)

#Valores Ausentes (NA)
#No existen en el dataset
cat("NA values:",sum(is.na(iris)),"\n") 

#Frecuencia de las caracteristicas segun especie

iris %>% gather("Char","Value",-Species) %>% 
  ggplot(aes(x = Species, y = Value, fill = Char)) +
  geom_col(position = "dodge") +
  theme_light()

#Histogramas por cada variable cuantitativa
ggplot(iris, aes(x = Sepal.Width)) + geom_histogram(binwidth = 0.2, aes(fill = Species)) + 
  xlab("Sepal Width") +  ylab("Frecuencia") + ggtitle("Sepal Width") + theme_light() + 
  geom_vline(aes(xintercept = mean(Sepal.Width)), linetype = "dashed")

ggplot(iris, aes(x = Sepal.Length)) + geom_histogram(binwidth = 0.2, aes(fill = Species)) + 
  xlab("Sepal Length") +  ylab("Frecuencia") + ggtitle("Sepal Length") + theme_light() + 
  geom_vline(aes(xintercept = mean(Sepal.Length)), linetype = "dashed")

ggplot(iris, aes(x = Petal.Width)) + geom_histogram(binwidth = 0.2, aes(fill = Species)) + 
  xlab("Petal Width") +  ylab("Frecuencia") + ggtitle("Petal Width") + theme_light() + 
  geom_vline(aes(xintercept = mean(Petal.Width)), linetype = "dashed")

ggplot(iris, aes(x = Petal.Length)) + geom_histogram(binwidth = 0.2, aes(fill = Species)) + 
  xlab("Petal Length") +  ylab("Frecuencia") + ggtitle("Petal Length") + theme_light() + 
  geom_vline(aes(xintercept = mean(Petal.Length)), linetype = "dashed")

#Boxplots por cada variable
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot(aes(fill = Species)) + 
  ylab("Sepal Width") + 
  ggtitle("Boxplot por Sepal Width") +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) 

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(fill = Species)) + 
  ylab("Sepal Length") + 
  ggtitle("Boxplot por Sepal Length") +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) 

ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_boxplot(aes(fill = Species)) + 
  ylab("Petal Width") + 
  ggtitle("Boxplot por Petal Width") +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) 

ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(aes(fill = Species)) + 
  ylab("Petal Length") + 
  ggtitle("Boxplot por Petal Length") +
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) 

#Correlacion entre variables
corrplot::corrplot.mixed(cor(iris[1:4]))
pairs.panels(iris[1:4],bg = c("pink","purple","cyan")[iris$Species], pch = 21 + as.numeric(iris$Species),main = "Correlacion de Variables en Iris Data",hist.col = "gray",stars = TRUE) 

