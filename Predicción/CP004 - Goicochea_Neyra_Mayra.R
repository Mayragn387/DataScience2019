#----------------------------------Header----------------------------------
#               Caso loans y Modelo GLM - Tarea 4
#   Fecha:  04/11/2019
#   Nombre: Mayra Johana Goicochea Neyra
#
#---------------------------------Libraries-------------------------------

library(skimr)
library(RSQLite)
library(kknn)
library(dplyr)
library(tidyverse)
library(rminer)
library(ggplot2)
library(faraway)
library(verification)

#-------------------------------Data Load-------------------------------
# Limpiar el workspace: 
rm(list = ls())

con <- dbConnect(SQLite(),"../Data/database.sqlite")
dbListTables(con)
raw_data <- dbGetQuery(con,"SELECT * FROM loan")

#-----------------------------Data Wrangling-----------------------------
# Observaciones ausentes (NA): 
na_rate <- function(x) {x %>% is.na() %>% sum() / length(x)}
sapply(raw_data, na_rate) %>% round(2) 
#Con este resultado y la descripción de las variables se quitaran variables con ratios altos y que no son significativas para el modelo.

loan <- raw_data[,c(4,7,8,10,11,13:18,22,25:27,29:36,38,54,60)]

#Excepción: las variables mths_since_last_delinq y mths_since_last_record pueden apoyar al modelo es así que se realiza una conversión de los valores ausentes por 0.
#y se convertirá en factores las variables categóricas.
loan <- loan %>% mutate(term = as.factor(term),
                        grade = as.factor(ifelse(is.na(grade),"(Other)",grade)),
                        sub_grade = as.factor(ifelse(is.na(sub_grade),"(Other)",sub_grade)),
                        emp_length = as.factor(ifelse(is.na(emp_length),"(Other)",emp_length)),
                        home_ownership = as.factor(ifelse(is.na(home_ownership),"OTHER",home_ownership)),
                        verification_status = as.factor(verification_status),
                        loan_status = as.factor(ifelse(is.na(loan_status),"(Other)",loan_status)),
                        purpose = as.factor(ifelse(is.na(purpose),"(Other)",purpose)),
                        int_rate = as.numeric(trimws(gsub("%","",int_rate)))/100,
                        revol_util = as.numeric(trimws(gsub("%","",revol_util)))/100,
                        #Los valores ausentes se convierten en 0.
                        mths_since_last_delinq = ifelse(is.na(mths_since_last_delinq),0,mths_since_last_delinq),
                        mths_since_last_record = ifelse(is.na(mths_since_last_record),0,mths_since_last_record),
                        )

cat("NA values:",sum(is.na(loan)),"\n") #solo quedan el 8% de la información que son valores ausentes.
loan <- na.omit(loan) #Se quitan las observaciones ausentes

#---------------------Summarize Data------------------------------
set.seed(789)

#Resumen de la información.
str(loan)
skim(loan)

#Histograma de los montos de préstamos
ggplot(loan,aes(loan_amnt)) +
  geom_histogram(breaks = seq(0 , 35000, by = 1000),col="black", aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "#adeae2", high = "#1796a1")+
  labs(title = "Monto del Préstamo", x = "Monto", y = "Numero de préstamos")+
  theme_bw()

# Variable Target o Default: Loan Status
#Gráfico de cajas para revisar el monto del préstamo por el estado del préstamo.
ggplot(loan, aes(loan_status, loan_amnt)) +
  geom_boxplot(aes(fill = loan_status)) +
  labs(title = "Monto del Préstamo por Estado del Préstamo", x = "Préstamo", y = "Monto") +
  theme_bw() +
  theme(axis.text.x = element_blank())
# Para hacer credit scoring se considera la información histórica
loan.df <- as.data.frame(loan[loan$loan_status != "Current", ])

xtabs(~loan_status, data = loan.df)

#El caso default sera cualquiera de los siguientes casos:
#Charged off: credito impago
#Default: caso deuda
loan.df$loan_status <- as.character(loan.df$loan_status)
loan.df$loan_status[loan.df$loan_status == "Charged Off" | loan.df$loan_status == "Default" ] <- "Yes"
loan.df$loan_status[loan.df$loan_status != "Yes"] <- "No"
loan.df$loan_status <- as.factor(loan.df$loan_status)
xtabs(~loan_status, data = loan.df)

#Annual Income
#Histograma del ingreso anual
ggplot(loan.df,aes(annual_inc)) +
  geom_histogram(breaks = seq(0 , 35000, by = 1000),col = "black", aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "#adeae2", high = "#1796a1")+
  labs(title = "Ingreso Anual", x = "Ingreso anual", y = "Frecuencia")+
  theme_bw()
#Box plot Annual Income vs loan_status
ggplot(loan.df, aes(x=loan_status, y=annual_inc, fill=loan_status))+
  geom_boxplot()+
  theme_bw()

# Home_ownership
#Esta variable tambien se dividira en dos grupos: 
#1: Casa propia o Hipotecada
#0: Renta u otros
ggplot(loan.df,aes(home_ownership)) +
  geom_bar(col="black", aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "#adeae2", high = "#1796a1")+
  labs(title = "Vivienda", x = "Propiedad o No", y = "Cantidad")+
  theme_bw()
#Box plot Home_ownership vs loan_status
ggplot(loan.df, aes(x=loan_status, y=home_ownership, fill=loan_status))+
  geom_boxplot()+
  theme_bw()

xtabs(~home_ownership, data = loan.df)
loan.df$home_ownership <- as.character(loan.df$home_ownership)
loan.df$home_ownership[loan.df$home_ownership == "OWN" | loan.df$home_ownership == "MORTGAGE"] <- "Yes"       
loan.df$home_ownership[loan.df$home_ownership != "Yes"] <- "No"
loan.df$home_ownership <- as.factor(loan.df$home_ownership)
xtabs(~home_ownership, data = loan.df)

# Delinq_2yrs
#Esta variable tambien se dividira en dos grupos: 
#1: Hay incidencias de morosidad
#0: No hay incidencias
ggplot(loan.df,aes(delinq_2yrs)) +
  geom_bar(col="black", aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "#adeae2", high = "#1796a1")+
  labs(title = "Incidencias de morosidad", x = "Dias de incidencias", y = "Cantidad")+
  theme_bw()

xtabs(~delinq_2yrs, data = loan.df)
loan.df$delinq_2yrs <- as.character(loan.df$delinq_2yrs)
loan.df$delinq_2yrs[loan.df$delinq_2yrs=="0"] <- "Yes"
loan.df$delinq_2yrs[loan.df$delinq_2yrs!= "Yes"] <- "No"
loan.df$delinq_2yrs <- as.factor(loan.df$delinq_2yrs)
xtabs(~delinq_2yrs, data = loan.df)

# Verification status: 
#Se dividira en dos grupos: 
#1: Verificado
#0: Otros
ggplot(loan.df,aes(verification_status)) +
  geom_bar(col="black", aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "#adeae2", high = "#1796a1")+
  labs(title = "Estado de Verificacion", x = "Estado Verificacion", y = "Cantidad")+
  theme_bw()

loan.df$verification_status <- as.character(loan.df$verification_status)
loan.df$verification_status[loan.df$verification_status == "Verified" | loan.df$verification_status == "Source Verified"] = "Yes"
loan.df$verification_status[loan.df$verification_status != "Yes"] = "No"
loan.df$verification_status <- as.factor(loan.df$verification_status)
xtabs(~verification_status, data = loan.df)

# Grouping variables
#Se clasificara en tres niveles: 
#0: Traslado, Negocio pequeño, y energia renovable (Negocios)
#1: Creditos personales y comerciales
#2: Otros
ggplot(loan.df,aes(purpose)) +
  geom_bar(col="black", aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "#adeae2", high = "#1796a1")+
  labs(title = "Objetivo del prestamo", x = "Objetivo", y = "Cantidad")+
  theme_bw()
xtabs(~purpose, data = loan.df)
loan.df$purpose <- as.character(loan.df$purpose)
loan.df$purpose[loan.df$purpose == "car" | loan.df$purpose == "major_purchase" | 
                  loan.df$purpose == "home_improvement"| loan.df$purpose == "credit_card" ] <- 1
loan.df$purpose[loan.df$purpose == "moving" | loan.df$purpose == "small_business" | 
                  loan.df$purpose == "renewable_energy" ] <- 0
loan.df$purpose[loan.df$purpose!= 0 & loan.df$purpose != 1 ] <- 2
loan.df$purpose <- as.factor(loan.df$purpose)
xtabs(~purpose, data = loan.df)

rm("loan","raw_data","labels","limits_inc","na_rate")
#Asignar valores numericos a la variable loan_status
loan.df$loan_status <- as.character(loan.df$loan_status)
loan.df$loan_status[loan.df$loan_status == "Yes"]<-1
loan.df$loan_status[loan.df$loan_status == "No"] <-0
loan.df$loan_status <- as.numeric(loan.df$loan_status)
xtabs(~loan_status, data = loan.df)

#---------------------------Train & Test Subsets---------------------------
set.seed(789)
n <- nrow(loan.df)
id_train <- sample(1:n , 0.8*n)
loan.train = loan.df[id_train,]
loan.test = loan.df[-id_train,]
dim(loan.train)
dim(loan.test)

#Modelo de Regresion Logistica 0 (con todas las variables)
loan.glm0 <- glm(loan_status~.,family=binomial,loan.train)
summary(loan.glm0)
sumary(loan.glm0)
AIC(loan.glm0) #138910.7
BIC(loan.glm0) #140482

#Modelo de Regresion Logistica 1 (con las variables loan_amnt, term, grade, home_ownership, annual_inc, purpose, dti, delinq_2yrs, inq_last_6mths, open_acc, revol_util, total_acc, out_prncp, tot_cur_bal)
loan.glm1 <- glm(loan_status ~ loan_amnt + home_ownership + annual_inc + verification_status + 
                   purpose + dti + delinq_2yrs + int_rate + inq_last_6mths + mths_since_last_delinq +
                   revol_bal + revol_util + total_acc,
              family = binomial(link= "logit"),loan.train)
summary(loan.glm1)
sumary(loan.glm1)
AIC(loan.glm1) #147484
BIC(loan.glm1) #147635.1

#Modelo de Regresion Logistica 2 (con las variables significativas según summary) 
loan.glm2 <- glm(loan_status~loan_amnt + term + grade + home_ownership + annual_inc + purpose + dti +
                   delinq_2yrs + inq_last_6mths + open_acc + revol_util + total_acc + out_prncp + tot_cur_bal,
                 family = binomial,loan.train)
AIC(loan.glm2) #142941.2
BIC(loan.glm2) #143152.7

#---------------------------Pruebas inSample y OutSample, Curva ROC---------------------------
#Pruebas dentro de la muestra train
prob.glm0.insample <- predict(loan.glm0,loan.train,type = "response")
prob.glm1.insample <- predict(loan.glm1,loan.train,type = "response")
prob.glm2.insample <- predict(loan.glm2,loan.train,type = "response")
roc.plot(x= loan.train$loan_status == 1, pred=cbind(prob.glm0.insample,prob.glm1.insample,prob.glm2.insample), legend=TRUE, leg.text=c("Full Model","Model 1", "Model 2"))$roc.vol

#Pruebas fuera de la muestra (test)
prob.glm0.outsample <- predict(loan.glm0,loan.test,type = "response")
prob.glm1.outsample <- predict(loan.glm1,loan.test,type = "response")
prob.glm2.outsample <- predict(loan.glm2,loan.test,type = "response")

roc.plot(x= loan.test$loan_status == 1, pred=cbind(prob.glm0.outsample,prob.glm1.outsample,prob.glm2.outsample), legend=TRUE, leg.text=c("Full Model","Model 1", "Model 2"))$roc.vol
#El modelo con todos las variables es el mejor segun la curva ROC.

#---------------------------Cross Validation---------------------------
#Vector de resultados de error
cv.error <- rep(0,3)

# Guardar los resultados para cada K  validation set. K= {3,5,10} 
cv.error[1] <- cv.glm(loan.test, loan.glm0, K=3)$delta[1]
cv.error[2] <- cv.glm(loan.test, loan.glm0, K=5)$delta[1]
cv.error[3] <- cv.glm(loan.test, loan.glm0, K=10)$delta[1]
mean(cv.error) #Error medio 0.1544534

cv.error[1] <- cv.glm(loan.test, loan.glm1, K=3)$delta[1]
cv.error[2] <- cv.glm(loan.test, loan.glm1, K=5)$delta[1]
cv.error[3] <- cv.glm(loan.test, loan.glm1, K=10)$delta[1]
mean(cv.error) #Error medio 0.1473709

cv.error[1] <- cv.glm(loan.test, loan.glm2, K=3)$delta[1]
cv.error[2] <- cv.glm(loan.test, loan.glm2, K=5)$delta[1]
cv.error[3] <- cv.glm(loan.test, loan.glm2, K=10)$delta[1]
mean(cv.error) #Error medio 0.150922

#--------------------------------Cutoff--------------------------------

#Funcion para calcular el mejor cutoff segun pesos de penalidad (mayor costo a los casos de predecir que no sea default pero que si es)
searchgrid = seq(0.01, 0.99, 0.01)
result = cbind(searchgrid, NA)

cost1 <- function(r, pi){
  weight1 = 10
  weight0 = 1
  c1 = (r==1)&(pi<pcut)
  c0 = (r==0)&(pi>pcut)
  return(mean(weight1*c1+weight0*c0))
}
model <- glm(loan_status~.,family = binomial,loan.train) ;
prob <- predict(model,loan.train,type = "response")

for(i in 1:length(searchgrid)){
  pcut <- result[i,1]
  result[i,2] <- cost1(loan.train$loan_status, prob)
}
plot(result, ylab="Cost in Training Set")

result[which.min(result[,2]),] #El mejor cutoff es 0.1

#Libreria Information Value - funcion optimalCutoff
library(InformationValue)
model <- glm(loan_status~., data=loan.train, family=binomial) #
predicted <- predict(loan.glm0, loan.test, type="response")  # predicted scores
optCutOff <- optimalCutoff(loan.test$loan_status, predicted)[1] 
optCutOff #El mejor cutoff es 0.4663192

#----------------------------Matriz de Confusion----------------------------
predicted.glm0.outsample <-  prob.glm0.outsample> 0.4663192
predicted.glm0.outsample <- as.numeric(predicted.glm0.outsample)
table(loan.test$loan_status, predicted.glm0.outsample, dnn=c("Truth","Predicted"))
#El cutoff de la libreria Information value es mejor, por que tiene menos taza de error que el 0.1


