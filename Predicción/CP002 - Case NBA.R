#-------------------------Header----------------------------------
#                   Caso NBA - Tarea 2
#   Fecha:  16/10/2019
#   Nombre: Mayra Johana Goicochea Neyra
#
#------------------------Libraries--------------------------------
setwd("C:/Users/Goicochea/Documents/GitHub/MasterDS2019/Data")
library(dplyr)    
library(readr)
library(kknn)
library(rsample) 
library(glmnet)
library(ggplot2)
library(car)
library(boot)
#------------------------Data Load--------------------------------

mNba <- read.csv("./nba.csv")

#--------------------Data Exploration-----------------------------
colnames(mNba) <- c("Player","Salary","NBA_Country","NBA_DraftNumber","Age","Tm","G","MP","PER","TS","TPAr","FTr","ORB","DRB","TRB","AST","STL","BLK","TOV","USG","OWS","DWS","WS","WS48","OBPM","DBPM","BPM","VORP")
summary(mNba)
dim(mNba)

###Data Cleaning
##1.Looking for:
#Missing Values
cat("NA values:",sum(is.na(mNba)),"\n")
mNba <- rminer::imputation("hotdeck",mNba,"TOV")
mNba <- rminer::imputation("hotdeck",mNba,"FTr")
mNba <- rminer::imputation("hotdeck",mNba,"TPAr")
mNba <- rminer::imputation("hotdeck",mNba,"TS")
cat("NA values:",sum(is.na(mNba)),"\n")

mNba %>% mutate(salarioM = (Salary/1000000)) %>% 
  ggplot(aes(x = salarioM)) +
  geom_histogram(fill = "#f7ad36", colour="black") +
  theme_light()

# Modelo Inicial
# Se quita la variable nombre del jugador debido a que no es una variable explicativa influyente al modelo predictivo del salario.
# No es una variable categorica para este caso.
reg0 <- lm(Salary~ .-Player, data = mNba)

#Influents Values
# Cooks Distance D
cutoff <- 4/(nrow(mNba) - length(reg0$coefficients) - 2)
plot(reg0, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "orange")

# Added variable plots
car::avPlots(reg0, ask = FALSE, id.method = "identify", col.lines = "orange")
# Influence Plot
car::influencePlot(reg0, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance", col = "#f7ad36" )

##2.Extracting
#Se eliminan los valores influyentes para que no distorsione el modelo
mNba <- mNba[-c(2,49,114,143,328),] 
#mNba1 <- mNba[,-c(1,3,6)]

###Data Exploration Analysis
##1.Summary
summary(reg0)
#Que un modelo con un R cuadrado de 0.5236 y con 8 variables significativas de 99 (incluidas las variables categoricas Team y Country)
#Es un caso extraño, podria haber multicolinealidad.

##2.Matriz de Correlación
# Se genera la matriz de correlación de todas las variables cuantitativas del modelo:
cor(mNba[,-c(1,3,6)])
corrplot::corrplot.mixed(cor(mNba[,-c(1,3,6)]))

#Se identifica que las variables 'PER','WS48','OBPM','DRB','TRB','OWS','WS','VORP','BPM' estan altamente correlacionadas
#Se genera un grafico de estas variables.
corrplot::corrplot.mixed(cor(mNba[,c('PER','WS48','OBPM','DRB','TRB','OWS','WS','VORP','BPM')]))

#Se revisa con la funcion vif (variance inflation factor), para revisar cuanta inestabilidad aporta al modelo.
reg1 <- lm(Salary~ .-Player-NBA_Country-Tm, data = mNba) 
#Se eliminan las variables aliadas (Country y Tm)
vif(reg1)

#A continuación comprobamos si hay problemas de multicolinealidad
rev.mc <- sqrt(vif(reg1)) > 2
sum(rev.mc) 
#Efectivamente hay problemas de multicolinealidad, son 18 variables que lo causan.

##2.Validacion General
#Por ultimo revisamos todas las pruebas con el metodo de validacion general
#Se observa que solo se cumple la hip?tesis de homocedasticidad.
gvmodel <- gvlma::gvlma(reg1) 
summary(gvmodel)


###Seleccion Variables
#1. Stepwise: Both Direction Stepwise
MASS::stepAIC(reg0, direction = "both")

#2.Cross Validation (del modelo Stepwise)
glm.fit4 = glm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + TPAr + ORB + TRB + USG + WS + OBPM,mNba[,-c(1,3,6)] ,family =  gaussian())
cv.err = cv.glm(mNba[,-c(1,3,6)],glm.fit4)
cv.err$delta[1]
#Es un valor de error delta grande: 2.609707e+13 (recordamos que es el mejor entre los evaluados en CP001)

###Regularizacion
#Se divide la muestra en dos subsets Train y Test.
set.seed(123)
nba_split <- initial_split(mNba, prop = .7, strata = "Salary")
nba_train <- training(nba_split)
nba_test  <- testing(nba_split)

#Se establece los modelos segun los subsets Train y Test.
nba_train_x <- model.matrix(Salary ~ ., nba_train)[, -1]
nba_train_y <- log(nba_train$Salary)

nba_test_x <- model.matrix(Salary ~ ., nba_test)[, -1]
nba_test_y <- log(nba_test$Salary)

dim(nba_train_x)
#Elastic Net
#Se establecen 4 modelos: lasso, ridge y dos de elastic net (con alpha 0.25 y 0.75)
lasso    <- glmnet(nba_train_x, nba_train_y, alpha = 1.0, standardize = TRUE) 
elastic1 <- glmnet(nba_train_x, nba_train_y, alpha = 0.25, standardize = TRUE) 
elastic2 <- glmnet(nba_train_x, nba_train_y, alpha = 0.75, standardize = TRUE) 
ridge    <- glmnet(nba_train_x, nba_train_y, alpha = 0.0, standardize = TRUE)

plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1)")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0)")

#Se realiza la evaluación mediante K Fold Cross Validation para obtener el mejor modelo
#Determinamos 10 de K Fold
fold_id <- sample(1:10, size = length(nba_train_y), replace = TRUE)

# Se crea un rango de alphas con incremento de 0.1 (desde 0 a 1, para evaluar también si conviene lasso o ridge)
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)

#Mediante un bucle, se evalua el rango de alpha para calcular el mse
for (i in seq_along(tuning_grid$alpha)) {
  # Se crea el modelo evaluado por cross validation
  fit <- cv.glmnet(nba_train_x, nba_train_y, alpha = tuning_grid$alpha[i], foldid = fold_id)
  
  # se guarda los valores mse y lambda mse en la tabla
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

#Se visualiza la tabla de mse
tuning_grid
#observamos que el menor mse es el caso de lasso.

#Graficamos los valores de alpha y se visualiza que se obtiene el menor valor mse cuando el alpha es 1 (lasso)
g <- tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2, colour = "#f7ad36") +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se),fill = "#ffedb8", alpha = .5) +
  ggtitle("MSE: one standard error") +
  theme_minimal()

plotly::ggplotly(g)

# El mejor modelo: Lasso
cv_lasso   <- cv.glmnet(nba_train_x, nba_train_y, alpha = 1.0)
min(cv_lasso$cvm)
## [1] 1.133798

# predict
pred <- predict(cv_lasso, s = cv_lasso$lambda.min, nba_test_x)
mean((nba_test_y - pred)^2)
## [1] 1.166295
