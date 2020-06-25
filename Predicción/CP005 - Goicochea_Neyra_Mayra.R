#---------------------------------------Header---------------------------------------
#               Caso Mapfre Seguros y Modelos ETS y ARIMA - Tarea 5
#   Fecha:  13/11/2019
#   Nombre: Mayra Johana Goicochea Neyra
#
#--------------------------------------Libraries--------------------------------------

library(skimr)
library(readr)

library(kknn)
library(dplyr)
library(tidyverse)
library(ggplot2)

library(forecast)
library(xts)
library(zoo)
library(ggfortify)

#--------------------------------------Data Load--------------------------------------

rawData <- read.csv("../Data/Primas_mapfre.csv", sep = ';', dec = ',')

#-----------------------------------Wrangling Data-----------------------------------
data.primas <- rawData %>% mutate(Fecha = as.character(as.numeric(substring(as.character(Fecha),7,11))*10000 +
                                                         as.numeric(substring(as.character(Fecha),1,2))*100 +
                                                         as.numeric(substring(as.character(Fecha),4,5))),
                                  Total_Primas = Primas_vida + Primas_no_vida) %>% select(Fecha, Total_Primas)
summary(data.primas)

####Creacion de Objetos Series de Tiempo
#Se crea el objeto xts de Primas Vida
xPrimas <- xts((data.primas$Total_Primas),
               order.by = as.POSIXct(strptime(data.primas$Fecha,"%Y%m%d")),
               frequency = 4)

ts_data <- ts(as.numeric(xPrimas), start = c(2008,1) , frequency = 4)
#Se crea el objeto trimestral debido a que la informacion se ha guardado por el ultimo dia de cada trimestre.
xPrimas <- to.quarterly(xPrimas) 

zPrimas <- as.zoo(xPrimas$xPrimas.Close)
names(zPrimas) <- "Primas"

#--------------------------Analisis de Variable Total Primas--------------------------

####Grafico de Primas en el tiempo
data.primas %>% mutate(Fecha = as.Date(Fecha,"%Y%m%d")) %>%
  ggplot(aes(Fecha, Total_Primas)) + 
  geom_line(color = "#00AFBB", size = 1) +
  #geom_smooth() +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07", method = "loess")
  theme_bw
  
#Irregular (E): Tiene variaciones aleatorias con respecto a periodos anteriores.  
#Tendencia (T): tiene tendencia creciente hasta el 2013 y a partir de ese año parece que baja la pendiente de la tendencia.
#Estacionalidad (S): Parece Estacional, con los siguientes graficos podremos observar a detalle.

####Revision de Estacionalidad de la Variable 
dec <- decompose(ts_data)
plot(dec)
ggseasonplot(ts_data, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Numero de Primas") +
  ggtitle("Seasonal plot: Primas Vida Mapfre")

ggseasonplot(ts_data, polar = TRUE) +
  ylab("Numero de Primas") +
  ggtitle("Polar seasonal plot: Primas Vida Mapfre")
#No tiene Estacionalidad.

####Revision Trimestral


##Grafico de serie
autoplot(zPrimas) + ggtitle("Cantidad Trimestral de Primas (Vida y No Vida) de Mapfre") + 
  xlab("Trimestres") + ylab("Primas")
#Grafico Trimestral - Estacionalidad
ggfreqplot(as.ts(zPrimas),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + 
  ggtitle("Primas Trimestrales")
#No tiene Estacionalidad

#--------------------------------------Modelling--------------------------------------

#Se va estimar la prediccion de los 4 ultimos trimestres 
cOmit <- 4
nObs <- length(zPrimas) 

#Train
oPrimas <- window(zPrimas, start = index(zPrimas[1]),
                      end = index(zPrimas[nObs - cOmit]))

#### Modelo ETS : Se utiliza el subset de train (2008 Q1 - 2016 Q4) para obtener
#automaticamente el modelo ETS que se ajusta al comportamiento del numero total de primas
fit.ets <- ets(oPrimas)
fets <- forecast(fit.ets)  #Prediccion
#Resultados
summary(fets) #AIC:561.7218
#Se tiene con este modelo un 5.59% de porcentaje de error absoluto (MAPE) sobre la muestra

#### Modelo ARIMA : De forma similar al modelo ETS, se utiliza el subset de train 
#(2008 Q1 - 2016 Q4) para obtener automaticamente el modelo ARIMA.

fit.arima = auto.arima(oPrimas,lambda = "auto") 
#Resultados
summary(fit.arima) #AIC=179.54
#Se tiene con este modelo un 5.64% de porcentaje de error absoluto (MAPE) sobre la muestra. El modelo ETS
#genera menor error

#Analisis Residual
#Análisis de la Autocorrelación: 
ggtsdisplay(fit1$residuals) 

#Prueba box-Ljung
Box.test(fit.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(fit.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(fit.arima$residuals,lag = 12, fitdf = 3, type = "Lj")

farima <- forecast(fit.arima) #Prediccion

df_new <- data.frame(value = as.vector(zPrimas),time = time(zPrimas))

#Grafico de ambas predicciones
plot <- ggplot(df_new)
parima <-  geom_forecast(farima, alpha = 0.4, col = "green")
pets <-  geom_forecast(fets, alpha = 0.2, col = "red")

plot + parima + pets +
  ggtitle("Predicción Primas Mapfre: Modelo ETS(A,A,A) vs ARIMA(0,1,1)(0,1,1)[4]") +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  theme_bw()

#Grafico con estimacion de Arima
plot + parima +
  ggtitle("Predicción Primas Mapfre: ARIMA(0,1,1)(0,1,1)[4]") +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  theme_bw()
#Predice bien los dos primeros puntos(2017 Q1 y 2017 Q2), pero los siguientes no es por ello que no es eficiente
#para la prediccion a mediano y largo plazo.

#Grafico con estimacion de ETS
plot + pets +
  ggtitle("Predicción Primas Mapfre: Modelo ETS(A,A,A)") +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  theme_bw()
#A pesar de que no predice bien los tres primeros puntos(2017 Q1, 2017 Q2 y 2017 Q3), pero para el 2017 Q4 se ajusta mejor que el modelo ARIMA.

##### KFOLd Cross Validation
k <- 10 # 60 is minimum data length for fitting a model, but dataset length is 40
n <- length(ts_data)
mape1 <- mape2 <- matrix(NA,4,4) 
st <- tsp(ts_data)[1] + (k - 1)/4
for(i in 1:4)
{
  xshort <- window(ts_data, end = st + (i-1))
  xnext <- window(ts_data, start = st + (i-1) + 1/4, end=st + i)
  #Automatic ETS Model
  fit1 <- ets(xshort, model = "AAA")
  fcast1 <- forecast(fit1, h = 4)
  #ARIMA 
  fit2 <- Arima(xshort, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 4), lambda = 0.4588303)
  fcast2 <- forecast(fit2, h = 4)
  #MAPE
  mape1[i,] <- abs((xnext- fcast1[['mean']])/xnext)*100 
  mape2[i,] <- abs((xnext- fcast2[['mean']])/xnext)*100 
}
plot(1:4, rowMeans(mape1), type = "l", col = 2, xlab = "horizon", ylab = "MAPE",
     ylim = c(0,20))
lines(1:4, rowMeans(mape2), type = "l",col = 3)
legend("topleft",legend = c("ETS","ARIMA"),col = 2:3,lty = 1)

#Conclusion: El modelo ARIMA tiene menor margen de error (segun el AIC)a comparacion de ETS.
#Aunque el modelo ETS predice mejor en el ultimo dato del test (2017 Q4), es mas eficiente utilizar
#el modelo ARIMA para la prediccion del 2018 y 2019.

#--------------------------Prediccion del 2018--------------------------
#A continuacion se estima el siguiente año (2018) con el modelo ETS.
modelo <- Arima(zPrimas, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 4), lambda = 0.4588303)
pred.2018 <- forecast(modelo, h = 8)
plot(pred.2018)

ggplot(df_new)+
  geom_forecast(pred.2018, alpha = 0.4, col = "red") +
  ggtitle("Predicción Primas Mapfre 2018: Modelo ETS(A,A,A)") +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  theme_bw()
#Los datos estimados para el año 2018 y año 2019
pred.2018
