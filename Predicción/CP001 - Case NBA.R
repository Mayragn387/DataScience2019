#-------------------------Header----------------------------------
#                   Caso NBA - Tarea 1
#   Fecha:  10/10/2019
#   Nombre: Mayra Johana Goicochea Neyra
#
#------------------------------------------------------------------
setwd("C:/Users/Goicochea/Documents/GitHub/MasterDS2019/Data")
library(here)
library(readr)
library(kknn)
mNba <- read.csv("./nba.csv")
colnames(mNba) <- c("Player","Salary","NBA_Country","NBA_DraftNumber","Age","Tm","G","MP","PER","TS","TPAr","FTr","ORB","DRB","TRB","AST","STL","BLK","TOV","USG","OWS","DWS","WS","WS48","OBPM","DBPM","BPM","VORP")
summary(mNba)
cat("NA values:",sum(is.na(mNba)),"\n")

#Valores Ausentes en los datos
#Se realiza una revision de que metodo utilizar para ajustar los datos con valores ausentes.
#1. Omitirlos
#2. Reemplazar su valor por la Mediana
#3. Hotdeck
#3. Hotdeck
library(rminer)
library(ggplot2)

medianTOV <- median(mNba$TOV,na.rm = TRUE)
medianFTr <- median(mNba$FTr,na.rm = TRUE)
medianTPAr <- median(mNba$TPAr,na.rm = TRUE)
medianTS <- median(mNba$TS,na.rm = TRUE)

mNba1 <- na.omit(mNba)
mNba2 <- imputation("value",mNba,"TOV",Value = medianTOV)
mNba3 <- imputation("hotdeck",mNba,"TOV")

meth0 <- data.frame(length = mNba$TOV)
meth1 <- data.frame(length = mNba1$TOV)
meth2 <- data.frame(length = mNba2$TOV)
meth3 <- data.frame(length = mNba3$TOV)
meth0$method <- "Orig"
meth1$method <- "Omit"
meth2$method <- "Median"
meth3$method <- "hotdeck"
all <- rbind(meth2,meth0,meth1, meth3)
ggplot(all,aes(length,fill = method)) + geom_density(alpha = 0.2)

mNba2 <- imputation("value",mNba,"FTr",Value = medianFTr)
mNba3 <- imputation("hotdeck",mNba,"FTr")
meth0 <- data.frame(length = mNba$FTr)
meth1 <- data.frame(length = mNba1$FTr)
meth2 <- data.frame(length = mNba2$FTr)
meth3 <- data.frame(length = mNba3$FTr)
meth0$method <- "Orig"
meth1$method <- "Omit"
meth2$method <- "Median"
meth3$method <- "hotdeck"
all <- rbind(meth2,meth0,meth1, meth3)
ggplot(all,aes(length,fill = method)) + geom_density(alpha = 0.2)

mNba2 <- imputation("value",mNba,"TPAr",Value = medianTPAr)
mNba3 <- imputation("hotdeck",mNba,"TPAr")
meth0 <- data.frame(length = mNba$TPAr)
meth1 <- data.frame(length = mNba1$TPAr)
meth2 <- data.frame(length = mNba2$TPAr)
meth3 <- data.frame(length = mNba3$TPAr)
meth0$method <- "Orig"
meth1$method <- "Omit"
meth2$method <- "Median"
meth3$method <- "hotdeck"
all <- rbind(meth2,meth0,meth1, meth3)
ggplot(all,aes(length,fill = method)) + geom_density(alpha = 0.2)

mNba1 <- na.omit(mNba)
mNba2 <- imputation("value",mNba,"TS",Value = medianTS)
mNba3 <- imputation("hotdeck",mNba,"TS")
meth0 <- data.frame(length = mNba$TS)
meth1 <- data.frame(length = mNba1$TS)
meth2 <- data.frame(length = mNba2$TS)
meth3 <- data.frame(length = mNba3$TS)
meth0$method <- "Orig"
meth1$method <- "Omit"
meth2$method <- "Median"
meth3$method <- "hotdeck"
all <- rbind(meth2,meth0,meth1, meth3)
ggplot(all,aes(length,fill = method)) + geom_density(alpha = 0.2)

#Se selecciona el metodo de hotdeck por ser mas cercano a los valores originales para que no distorsione el modelo predictivo.
mNba <- imputation("hotdeck",mNba,"TOV")
mNba <- imputation("hotdeck",mNba,"FTr")
mNba <- imputation("hotdeck",mNba,"TPAr")
mNba <- imputation("hotdeck",mNba,"TS")

#Revisamos que ya no haya valores ausentes.
cat("NA values:",sum(is.na(mNba)),"\n")

#Se formula el modelo basico de regresion para analizar el comportamiento de las variables exogenas.
#Se quita la variable Nombre del Jugador al no ser representativa para la prediccion del salario del jugador.


reg0 <- lm(Salary~ .-Player, data = mNba)

summary(reg0)
#De igual forma se observa en el modelo inicial que el Pais crea muchas variables dicotomicas no representativas para el modelo predictivo.

reg01 <- lm(Salary~ .-Player-NBA_Country, data = mNba)
summary(reg01)

#========= ANALISIS DE LAS VARIABLES EXPLICATIVAS============= 
#1. Pruebas de Normalidad
#   a. QQPLOT: Contrastamos graficamente la distribucion de densidad de la muestra con respecto a la distribucion normal.

library(carData)
library(car)

qqPlot(reg01, labels = row.names(mNbaF), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")  
#La linea continua se encuentra dentro del intervalo de confianza en el centro, pero en los extremos se aleja.
#No se puede afirmar que el comportamiento de las variables siga un distribucion normal.

#   b.Histograma + densidad + normal + rug
#Esta tecnica nos permite calcular el error residual del analisis de normalidad de la muestra.
residplot <- function(fit, nbreaks = 10) {
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE,
       xlab = "Studentized Residual",
       main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x, mean = mean(z), sd = sd(z)),
        add = TRUE, col = "blue", lwd = 2)
  lines(density(z)$x, density(z)$y,
        col = "red", lwd = 2, lty = 2)
  legend("topright",
         legend =  c( "Normal Curve", "Kernel Density Curve"),
         lty = 1:2, col = c("blue","red"), cex = .7)
}

residplot(reg01)  

#Jarque - Bera
vResid = resid(reg01)
library(fBasics)
library(akima)
jbTest(vResid)

#Shapiro Wilk
shapiro.test(vResid)

reg01 <- lm(Salary~ .-Player-NBA_Country-Tm, data = mNba)
#Validacion General
library(gvlma)
gvmodel <- gvlma(reg01) 
summary(gvmodel)

#Revision de Multicolinealidad
reg0 <- lm(Salary~ .-Player, data = mNba)
vif(reg0) 
sqrt(vif(reg0)) > 2

reg0 <- lm(Salary~ .-Player-NBA_Country, data = mNba)
vif(reg0) 
sqrt(vif(reg0)) > 2

reg01 <- lm(Salary~ .-Player-NBA_Country-Tm, data = mNba)
vif(reg01) 
sqrt(vif(reg01)) > 2

#Modelo de regresion segun las variables significativas reconocidas con summary
reg02 <- lm(Salary~ NBA_DraftNumber+Age+G+MP, data = mNba)
summary(reg02)
vif(reg02) 
sqrt(vif(reg02)) > 2

#Modelo de regresion segun los errores de multicolinealidad (menores a 2)
reg03 <- lm(Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV, data = mNba)
summary(reg03)
vif(reg03) 
sqrt(vif(reg03)) > 2

#Revision de Valores Anomalos

outlierTest(reg01)
#Muestra los dos valores anomalos
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2,3)*p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(reg01)

#Valores Influyentes

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(mNba) - length(reg01$coefficients) - 2)
plot(reg01, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

# Added variable plots
# add id.method = "identify" to interactively identify points
avPlots(reg01, ask = FALSE, id.method = "identify")

# Influence Plot
influencePlot(reg01, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )
mNba <- mNba[-c(114,143,166,328),]



#=================SELECCION DE VARIABLES================ 
library(MASS)
library(leaps)

reg0 <- lm(Salary~ .-Player, data = mNba)
#1. Forward Stepwise
stepAIC(reg0, direction = "forward")

#2. Backward Stepwise
stepAIC(reg0, direction = "backward")

#3. Both Direction Stepwise
stepAIC(reg0, direction = "both")

#=======CROSS VALIDATION========= 
#1.Caso: reg0 =  Salary~.-Player-NBA_Country-Tm
#2.Caso (Summary): Salary~ NBA_DraftNumber+Age+G+MP
#3.Caso (Colinealidad): Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV
#4.Caso (Stepwise): Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM

#Validation Set
library(ISLR)
set.seed(400)
mNbaF <- mNba[,c(2,4:5,7:28)] #Se eliminan las columnas Player, NBA_Country y Tm por que afectan a las pruebas (problema de multicolinealidad)
numData = nrow(mNbaF)
train = sample(numData ,numData/2)

#Caso 1: reg0 =  Salary~.-Player-NBA_Country-Tm
regres.train = lm(Salary~.,mNbaF ,subset = train ) 
attach(mNbaF)
mean((Salary - predict(regres.train ,Auto))[-train ]^2)
#error:2.837077e+13

#2.Caso (Summary): Salary~ NBA_DraftNumber+Age+G+MP
regres.train2 = lm(Salary~ NBA_DraftNumber+Age+G+MP,mNbaF ,subset = train )
mean((Salary - predict(regres.train2 ,Auto))[-train ]^2)
#error:2.870914e+13

#3.Caso (Colinealidad): Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV
regres.train3 = lm(Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV,mNbaF ,subset = train )
mean((Salary - predict(regres.train3 ,Auto))[-train ]^2)
#error:3.934361e+13

#4.Caso (Stepwise): Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM
regres.train4 = lm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM,mNbaF ,subset = train )
mean((Salary - predict(regres.train4 ,Auto))[-train ]^2)
#error:2.669294e+13 #El mejor

#Leave One Out Cross Validation
library(boot)
#1.Caso: reg0 =  Salary~.-Player-NBA_Country-Tm
glm.fit1 = glm(Salary~.,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit1)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error: 2.545618e+13

#2.Caso (Summary): Salary~ NBA_DraftNumber+Age+G+MP
glm.fit2 = glm(Salary~NBA_DraftNumber+Age+G+MP,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit2)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error:2.706614e+13

#3.Caso (Colinealidad): Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV
glm.fit3 = glm(Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit3)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error:3.710536e+13

#4.Caso (Stepwise): Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM
glm.fit4 = glm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit4)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error:2.429988e+13 #El mejor

#K Fold Cross Validation
library(boot)
#1.Caso: reg0 =  Salary~.-Player-NBA_Country-Tm
glm.fit1 = glm(Salary~.,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit1,K = 10)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error:2.517223e+13

#2.Caso (Summary): Salary~ NBA_DraftNumber+Age+G+MP
glm.fit2 = glm(Salary~ NBA_DraftNumber+Age+G+MP,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit2,K = 10)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error:2.71919e+13

#3.Caso (Colinealidad): Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV
glm.fit3 = glm(Salary~ NBA_DraftNumber + Age + FTr + AST + STL + TOV,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit3,K = 10)
cv.err$delta[1]  #raw cross-validation estimate of prediction error
#Error:3.714488e+13

#4.Caso (Stepwise): Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM
glm.fit4 = glm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM,mNbaF ,family =  gaussian())
cv.err = cv.glm(mNbaF,glm.fit4)
cv.err$delta[1] #raw cross-validation estimate of prediction error
#Error:2.429988e+13 #El mejor