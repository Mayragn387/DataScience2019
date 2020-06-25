#-------------------------Header----------------------------------
#                   Caso GAM Bikes - Tarea 3
#   Fecha:  23/10/2019
#   Nombre: Mayra Johana Goicochea Neyra
#
#------------------------Libraries--------------------------------
library(dplyr)    
library(readr)
library(kknn)
library(rsample) 
library(tidyverse)
library(ggplot2)
library(skimr)
library(gam)
library(splines)
#------------------------Data Load--------------------------------
hour_rawdata <- read.csv("../Data/hour.csv")
day_rawdata <- read.csv("../Data/day.csv")

#---------------------Data Wrangling------------------------------
#Se quitan los campos instant y date
bday <- day_rawdata[,-c(1,2)]
bhour <- hour_rawdata[,-c(1,2)]
#Se asignan los factores correspondientes a las variables: yr, season, holiday, weathersit
bday$yr <- factor(format(bday$yr, format = "%A"), levels = c("0", "1") , labels = c("2011","2012"))
bhour$yr <- factor(format(bhour$yr, format = "%A"), levels = c("0", "1") , labels = c("2011","2012"))

bday$season <- factor(format(bday$season, format = "%A"),levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
bhour$season <- factor(format(bhour$season, format = "%A"),levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))

bday$holiday <- factor(format(bday$holiday, format = "%A"),levels = c("0", "1") , labels = c("Working Day","Holiday"))
bhour$holiday <- factor(format(bhour$holiday, format = "%A"),levels = c("0", "1") , labels = c("Working Day","Holiday"))

bday$weathersit <- factor(format(bday$weathersit, format = "%A"),levels = c("1", "2","3","4") ,labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist","Bad: Rain/Snow/Fog","Worse: Heavy Rain/Snow/Fog"))
bhour$weathersit <- factor(format(bhour$weathersit, format = "%A"),levels = c("1", "2","3","4") ,labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist","Bad: Rain/Snow/Fog","Worse: Heavy Rain/Snow/Fog"))

#---------------------Summarize Data------------------------------
skim(bday)
skim(bhour)

ggplot(bday, aes(x = cnt)) + 
  geom_histogram(colour = "blue", fill = 'skyblue') +
  ggtitle("Frecuencia de prestamos de bicicletas")
  
ggplot(bhour, aes(x = cnt)) + 
  geom_histogram(colour = "blue", fill = 'skyblue') +
  ggtitle("Frecuencia de prestamos de bicicletas")

#Distribucion de las variables categoricas
#Dataset por fecha
ggplot(bday, aes(x = season, y = cnt)) +
  geom_boxplot( aes(colour=season)) +
  ggtitle("Total prestamos (Cnt) vs Season") +
  theme_bw()

ggplot(bday, aes(x = weathersit, y = cnt)) +
  geom_boxplot( aes(colour=weathersit)) +
  ggtitle("Total prestamos (Cnt) vs Weather") +
  theme_bw()

ggplot(bday, aes(x = holiday, y = cnt)) +
  geom_boxplot( aes(colour=holiday)) +
  ggtitle("Total prestamos (Cnt) vs Holiday/Working Day") +
  theme_bw()

#Matriz de Correlacion
mday_cor <- bday %>% mutate(temp = 41*temp, atemp = atemp*50, hum = hum*100, windspeed = windspeed*67 )
colnames(mday_cor) <- c("Season", "Year","Month", "Holiday","Weekday","Working day","Weather","Temperature","Feeling Temperature","Humidity","Windspeed","Casual","Registered","Count")

corrplot::corrplot.mixed(cor(mday_cor[,c("Count","Temperature","Feeling Temperature","Humidity","Windspeed")]))
PerformanceAnalytics::chart.Correlation(mday_cor[,c("Count","Temperature","Feeling Temperature","Humidity","Windspeed")],histogram = TRUE, pch = 19)

#Relacion de Bike Share Count vs Temperature
bday %>% mutate(temperature = temp*41) %>%
  ggplot( aes(temperature,cnt)) +
  geom_point(alpha = 0.5, aes(color = temperature)) +
  geom_smooth(colour = "red") +
  ggtitle("Count by Temperature") +
  scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') +
  theme_bw()

#Relacion de Bike Share Count vs Feeling Temperature
bday %>% mutate(feel_temperature = atemp*50) %>%
  ggplot( aes(feel_temperature,cnt)) +
  geom_point(alpha = 0.5, aes(color = feel_temperature)) +
  geom_smooth(colour ="red") +
  ggtitle("Count by Feeling Temperature") +
  scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') +
  theme_bw()

#Relacion de Bike Share Count vs Windspeed
bday %>% mutate(windspeed = windspeed*67) %>%  
  ggplot(aes(windspeed,cnt)) +
  geom_point(alpha = 0.5, aes(color = windspeed)) +
  geom_smooth(colour = "red") +
  ggtitle("Count by Windspeed") +
  scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') +
  theme_bw()

#Relacion de Bike Share Count vs Humidity
bday %>% mutate(hum = hum*100) %>%  
  ggplot(aes(hum,cnt)) +
  geom_point(alpha = 0.5, aes(color = hum)) +
  geom_smooth(colour="red") +
  ggtitle("Count by Humidity") +
  scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') +
  theme_bw()

#Relacion de Total prestamos (Cnt) vs Working Day
ggplot(bhour, aes(hr,cnt)) +
  geom_point() +
  ggtitle("Count by Working Day") +
  geom_point(aes(color = temp), position = "jitter") +
  scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))+
  theme_bw() +
  facet_grid(~workingday)

#---------------------Model Selection------------------------------

#CNT vs Temperature
plot(bday$temp, bday$cnt, col='gray')
fit <- smooth.spline(bday$temp, bday$cnt, df=16)
fit2 <- smooth.spline(bday$temp, bday$cnt, cv=TRUE)
lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=1)
fit2$df #9.1

#CNT vs Feeling Temperature
plot(bday$atemp, bday$cnt, col='gray')
fit <- smooth.spline(bday$atemp, bday$cnt, df=16)
fit2 <- smooth.spline(bday$atemp, bday$cnt, cv=TRUE)
lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=1)
fit2$df #8.8

#CNT vs Humidity
plot(bday$hum, bday$cnt, col='gray')
fit <- smooth.spline(bday$hum, bday$cnt, df=16)
fit2 <- smooth.spline(bday$hum, bday$cnt, cv=TRUE)
lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=1)
fit2$df #4.55

plot(bday$windspeed, bday$cnt, col='gray')
fit <- smooth.spline(bday$windspeed, bday$cnt, df=16)
fit2 <- smooth.spline(bday$windspeed, bday$cnt, cv=TRUE)
lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=1)
fit2$df #6.01


gam1 <- gam(cnt ~ s(temp,df=9.1)+s(atemp,df=8.8)+s(hum,df=4.55)+s(windspeed,df=6.01)+season+holiday+weekday+workingday+weathersit, data=bday)
summary(gam1)
plot(gam1, se=TRUE, col='blue')


m_1 <- gam(cnt ~ s(temp,df=9.1)+s(atemp,df=8.8)+s(hum,df=4.55)+season+holiday+weekday+workingday+weathersit, data=bday)
m_2 <- gam(cnt ~ s(temp,df=9.1)+s(atemp,df=8.8)+s(hum,df=4.55)+windspeed+season+holiday+weekday+workingday+weathersit, data=bday)
m_3 <- gam(cnt ~ s(temp,df=9.1)+s(atemp,df=8.8)+s(hum,df=4.55)+s(windspeed,df=6.01)+season+holiday+weekday+workingday+weathersit, data=bday)
m_4 <- gam(cnt ~ s(temp,df=9.1)+s(atemp,df=8.8)+s(hum,df=4.55)+season+holiday+weekday+workingday, data=bday)
m_5 <- gam(cnt ~ s(atemp,df=8.8)+windspeed+season+holiday+weekday+weathersit, data=bday)

anova(m_1,m_2,m_3,m_4,m_5,test='F')

