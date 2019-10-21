#--------------PRIMERA PARTE  2 PUNTOS ------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(dbplyr)

#1. Crear un nuevo proyecto denominado practica 4.

# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer el dataset activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado

#Se cargan los datos en un dataframe llamado activities
activities <- read_csv("activities.csv")


# 3.Comprobar el contenido con View y contar cuantos NAs hay en la columna GPS del dataset activities
head(activities,5)
#Se cuentan las filas que tienen datos NA en la variable GPS
nNA<-sum(is.na(activities$GPS))
nNA  #Son 305 registros

# 4. Crear un objeto R denominado act_new que contenga solo las variables
# siguientes: 1,2,5-6
#Se crea un dataframe donde se almacenan las variables 1,2,5,6
act_new<-  select(activities,1,2,5:6)
#Limpiamos el environment.
rm("activities")

# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'
#Se cambia el nombre de las variables Activity type por tipo y Timezone por ciudad con la funcion rename
act_new<- act_new %>% rename(tipo="Activity type",ciudad="Timezone")
head(act_new,5)

# 6. Realizar un recuento de tipo de actividad con summary. Para ello
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable par visualizar las frecuencias.
# Haz lo mismo para la variable ciudad
#Se cambian las variables tipo y ciudad por factores, se utiliza la funcion as.factor.
act_new$tipo<-as.factor(act_new$tipo)
act_new$ciudad<-as.factor(act_new$ciudad)

#Se genera un resumen en base a las variables tipo y ciudad
summary(act_new$tipo)
summary(act_new$ciudad)

#Grafico geom_bar basico
#Se genera dos graficos con las frecuencias de los datos por tipo y ciudad
ggplot(act_new,aes(x=tipo,fill=tipo))+geom_bar()
ggplot(act_new,aes(x=ciudad,fill=ciudad))+geom_bar()

#Grafico con orden descendente
#Se genera dos graficos con las frecuencias de los datos por tipo y ciudad ordenados en forma descendente.
act_new %>% group_by(tipo) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(tipo,-n),y=n,fill=tipo))+
  geom_bar(stat="identity", show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("Tipo de actividad")+
  ylab("n")+
  labs(title="Frecuencias segun tipo de actividad")

act_new %>% group_by(ciudad) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(ciudad,-n),y=n,fill=ciudad))+
  geom_bar(stat="identity", show.legend=FALSE) +
  xlab("Ciudad")+
  ylab("n")+
  labs(title="Frecuencias segun Ciudad")


#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que
# no se practican en Amsterdam y si en Madrid y viceversa. Genera graficos para visualizar los resultados

#Se crean dos dataframes para las ciudades de Amsterdam y Madrid respectivamente.
act_new_ams<-filter(act_new,grepl("Amsterdam",ciudad))
act_new_mad<-filter(act_new,grepl("Madrid",ciudad))

#Deportes que se practican en Amsterdam pero no en Madrid
#con el uso de %in% se puede filtrar si el valor de amsterdam existe en madrid y solo se traen los datos con valor False a esta condicion.
act_new_ams %>% select(tipo) %>% distinct() %>%  filter((tipo %in% act_new_mad$tipo)==FALSE)
#Grafico
#Se genera un grafico para ver los deportes que no se practican en madrid pero si en amsterdam
#para ello se usa filter y se sumariza la ocurrencia de cada actividad
act_new_ams %>% group_by(tipo) %>% 
  summarise(n=n()) %>% 
  filter((tipo %in% act_new_mad$tipo)==FALSE) %>%
  ggplot(aes(x=reorder(tipo,-n),y=n,fill=tipo))+
  geom_bar(stat="identity", show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("Deporte")+
  ylab("n")+
  labs(title="Deportes que se practican en Amsterdam pero no en Madrid")

#Deportes que se practican en Madrid pero no en Amsterdam
#De forma similar al caso anterior, se utiliza %in% para revisar si cada deporte del dataframe de madrid existe en los datos de amsterdam
#y se considera ese valor para los casos que no se cumplen esa condicion.
act_new_mad %>% select(tipo) %>% distinct() %>% filter((tipo %in% act_new_ams$tipo)==FALSE)
#Grafico
#Se genera un grafico para ver los deportes que no se practican en amsterdam pero si en madrid
#para ello se usa filter y se sumariza la ocurrencia de cada actividad
act_new_mad %>%group_by(tipo) %>% 
  summarise(n=n()) %>%
  filter((tipo %in% act_new_ams$tipo)==FALSE) %>%
  ggplot(aes(x=reorder(tipo,-n),y=n,fill=tipo))+
  geom_bar(stat="identity", show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("Deporte")+
  ylab("n")+
  labs(title="Deportes que se practican en Madrid pero no en Amsterdam")


#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el ano 2019
#Se filtra la informacion del dataframe act_new con las siguientes condiciones: actividades realizadas en Amsterdam, en el ano 2019
#y que sean "Pilates" o "Cycling" (para ello se utiliza el %in%)
#Se modifica el formato del campo de a fecha (YYYY/MM/DD) y se muestra los valores unicos (con distinct)
act_new %>% filter(grepl("Amsterdam",ciudad),format(de,"%Y")=="2019",tipo %in% c("Cycling","Pilates")) %>%
  mutate(fecha=format(de,"%Y/%m/%d")) %>%
  select(fecha) %>% distinct 


#9. Crear una nueva variable dif con los minutos de realizacion de cada actividad en Amsterdam
# y realizar una representacion grafica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas
#Con el uso de mutate se calcula la diferencia en minutos de las fechas de y a. La funcion difftime calcula 
#la diferencia de fechas en la unidad indicada (en este caso mins)
act_new<- act_new %>% mutate(dif= difftime(a,de,units="mins")) 

head(act_new,5)

#Se suma las diferencias de minutos por tipo de actividad (con group by y summarise)
act_new_1<- act_new %>% group_by(tipo) %>%
  summarise(tiempo=sum(as.integer(dif))) 

# Actividades en Amsterdam que se han practicado durante dos horas o mas
#Se grafica las actividades que se han realizado por 120 min a mas en la ciudad Amsterdam
#Se usa filter para condicionar la informacion, se usa summarise y group by para calcular los minutos totales.
act_new %>% filter(grepl("Amsterdam",ciudad)) %>%
  group_by(tipo) %>%
  summarise(tiempo=sum(as.integer(dif))) %>%
  filter(tiempo >=120) %>%
  ggplot(aes(x=reorder(tipo,-tiempo),y=tiempo, fill=tipo))+
  geom_bar(stat="identity", show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("Actividad")+
  ylab("Tiempo Diff")+
  labs(title="Actividades se han practicado en Amsterdam durante 2 o mas horas")

#Actividades en Madrid que se han practicado durante dos horas o mas
#Se grafica las actividades que se han realizado por 120 min a mas en la ciudad Madrid
#Se usa filter para condicionar la informacion, se usa summarise y group by para calcular los minutos totales.
act_new %>% filter(grepl("Madrid",ciudad)) %>%
  group_by(tipo) %>%
  summarise(tiempo=sum(as.integer(dif))) %>%
  filter(tiempo >=120) %>%
  ggplot(aes(x=reorder(tipo,-tiempo),y=tiempo, fill=tipo))+
  geom_bar(stat="identity", show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  xlab("Actividad")+
  ylab("Tiempo Diff")+
  labs(title="Actividades se han practicado en Madrid durante 2 o mas horas")

#Actividades que se han practicado durante dos o mas horas
#A continuacion se muestra un grafico con color diferente por ciudad para ver el total de ocurrencia de los deportes.
act_new %>% filter(grepl("Madrid",ciudad)|grepl("Amsterdam",ciudad)) %>%
  group_by(tipo,ciudad) %>%
  summarise(tiempo=sum(as.integer(dif))) %>%
  filter(tiempo >=120) %>%
  ggplot(aes(x=reorder(tipo,-tiempo),y=tiempo, fill=ciudad))+
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position="bottom", legend.box = "horizontal")+
  xlab("Actividad")+
  ylab("Tiempo Diff")+
  labs(title="Actividades se han practicado durante 2 o mas horas")

#Actividades que se han practicado durante dos o mas horas(en facetas)
#Se visualiza a continuacion dos graficos en paralelo (Amsterdam a la izquierda y Madrid a la derecha)
#con la opcion facet_grid
act_new %>% filter(grepl("Madrid",ciudad)|grepl("Amsterdam",ciudad)) %>%
  group_by(tipo,ciudad) %>%
  summarise(tiempo=sum(as.integer(dif))) %>%
  filter(tiempo >=120) %>%
  ggplot(aes(x=reorder(tipo,-tiempo),y=tiempo, fill=tipo))+
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position="bottom", legend.box = "horizontal")+
  xlab("Actividad")+
  ylab("Tiempo Diff")+
  facet_grid(.~ciudad)+
  labs(title="Actividades se han practicado durante 2 o mas horas")

#Limpiamos el environment
rm("act_new_mad","act_new_ams")

#10. Guardar el nuevo dataset en un archivo llamado "act_new.csv"
#Se usa write.csv para crear el archivo act_new.csv y asi guardar los datos.
write.csv(act_new,file="act_new.csv")


#----------------SEGUNDA PARTE   3 PUNTOS-------------


# 11. Cargar el dataset sleep en un objeto llamado sleep
#Se cargan los datos en un dataframe llamado sleep
sleep <- read_csv("sleep.csv")

#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan informacion, que no sean todo cero.
#Se filtra el dataframe segun las columnas, y se usa all(sleep==0) para determinar las variables que no 
#cumplen con la condicion que tiene todos sus valores 0.
sleep_new<-sleep[,apply(sleep,2,function(sleep) !all(sleep==0))]
head(sleep_new,5)
rm("sleep")

#13. Renombrar las variables de sleep_new a nombres cortos:
#Se cambia el nombre de las variables ligero (s), profundo (s), despierto (s), Duration to sleep (s), Duration to wake up (s) y Snoring (s) con la funcion rename
sleep_new<- sleep_new %>% rename(Ligero="ligero (s)",Profundo="profundo (s)",Despierto="despierto (s)",To_sleep="Duration to sleep (s)",To_Wakeup="Duration to wake up (s)",Snoring="Snoring (s)")
head(sleep_new,5)

#14. Eliminar todas las filas que contengan alg√∫n NA
#Se utilizo complete.cases, una funcion que identifica que no existan valores ausentes en las filas.
sleep_new<-sleep_new %>% filter(complete.cases(sleep_new))
head(sleep_new,5)


# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo
#Se utiliza mutate para calcular el tiempo total de sueno = sueno ligero + sueno profundo
#y se divide entre 60 para calcularlo en minutos.
sleep_new <- mutate(sleep_new,total=(Ligero+Profundo)/60)
head(sleep_new,5)

# 16. Visualizacion de la relacion ligero-profundo-total
#Representacion grafica en minutos o horas
#Se utiliza ggplot para graficar la relacion entre las variables.
#Para observar las relaciones en forma grafica, me parecio conveniente el uso del geom_point y geom_smooth

#Grafico Ligero vs Profundo (en minutos)
sleep_new %>% mutate(ligero=Ligero/60,profundo=Profundo/60) %>%
  ggplot(sleep_new,mapping=aes(x=ligero,y=profundo))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  labs(title="Analisis sueno ligero y sueno profundo(en min.)")

#Grafico Ligero vs Total (en minutos)
sleep_new %>% mutate(ligero=Ligero/60) %>%
  ggplot(sleep_new,mapping=aes(x=ligero,y=total))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  labs(title="Analisis sueno ligero y sueno total(en min.)")

#Grafico Profundo vs Total (en minutos)
sleep_new %>% mutate(profundo=Profundo/60) %>%
  ggplot(mapping=aes(x=profundo,y=total))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  labs(title="Analisis sueno profundo y sueno total (en min.)")


# A la vista de los resultados, que tipo de sueno es mas relevante?
#El sueno profundo parece mas significativo por que el sueno total tiene una correlacion aparentemente mas fuerte que la de sueno ligero debido a que los puntos se ubican mas cercanos a la linea.
#Segun el grafico, aparentemente el sueno ligero y el sueno profundo no tienen correlacion. 

# 17. Realizar un analisis de diferencias entre los dos tipos de sueno e interpretar los resultados
# usar la funcion ICalpha o el 'One sample t-test' de TeachingDemos: t.test()
#Se utilizo la funcion ICalpha para calcular la diferencia de las medias muestrales del sueno ligero y sueno profundo
#a nivel de confianza del 95%.
ICalpha<-function(ModeloA, ModeloB, alfa=0.05)
{
  n<-length(ModeloA)
  diferencias<-ModeloA-ModeloB
  mediad<-mean(diferencias)
  s<-sqrt(var(diferencias))
  valort<-qt(alfa/2,n-1,lower.tail = F)
  valor<-valort*s/sqrt(n)
  cotaInf<-mediad-valor
  cotaSup<-mediad+valor
  df<-data.frame(cotaInf, cotaSup)
  return(df)
}

mligero<-sleep_new$Ligero
mprofundo<- sleep_new$Profundo

#Se grafica ambas muestras sueno ligero y sueno profundo
plot(mligero, mprofundo)

#Prueba a niveles de confianza: : alfa=0.05, alfa=0.01 y alfa = 0.1
IC<-ICalpha(mligero, mprofundo, 0.1)
print('Intervalo de confianza al 90%')
IC  #Segun el intervalo de confianza al 90%: las diferencias son negativas y no interceptan el valor 0.

IC<-ICalpha(mligero, mprofundo, 0.05)
print('Intervalo de confianza al 95%')
IC  #Segun el intervalo de confianza al 95%: las diferencias siguen siendo negativas y no interceptan el valor 0.

IC<-ICalpha(mligero, mprofundo, 0.01)
print('Intervalo de confianza al 99%')
IC  #Segun el intervalo de confianza al 99%: las diferencias son negativas y no interceptan el valor 0.

rm(mligero,mprofundo)

#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.
#Primero se crea una variable fecha para guardar el valor en formato YYYY-MM-DD del campo DE en ambos dataframes
act_new <- mutate(act_new, fecha=as.Date(de,tz="UTC"))
sleep_new <- mutate(sleep_new,fecha=as.Date(de,tz="UTC"))

#Se seleccionan las variables fecha y ciudad del dataframe act_new.
#DespuÈs se selecciona los valores ˙nicos, para establecer dia y ciudad en la que estuvo en esa fecha.
#Se encontro que el 19/03/2019, estuvo en Madrid primero y despuÈs Amsterdam.
#Como el dataframe de sleep_new se trata del sueno obtenido ese dÌa, considere que tomara el ˙ltimo valor de la fecha.
#AsÌ que en este caso serÌa Amsterdam. Para hacerlo utilice top_n que trae los primeros n registros y en el caso de traer los ultimos n
#se coloca valor n en negativo, en este caso n=-1

act_new_1<-act_new %>% select(fecha,ciudad) %>% distinct %>% group_by(fecha) 

act_new_1<-act_new_1%>% top_n(-1) %>% group_by(fecha) 

#Se realiza el merge en base a la variable fecha.
sleep_new<-merge(x=sleep_new, y=act_new_1, by="fecha") 

#19. Representar la relacion totalsleep y profundo usando como facetas el factor ciudad
#Se usa facet_grid para mostrar los graficos en paralelo de las ciudades.
sleep_new %>% mutate(profundo=Profundo/60) %>%
  rename(totalsleep=total) %>%
  ggplot(aes(x=profundo,y=totalsleep)) +
  geom_point(aes(color=ciudad))+
  geom_smooth(se=FALSE)+
  theme_bw()+
  facet_grid(.~ciudad)+
  labs(title="Analisis sueno profundo y sueno total (en min.)")

#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"
#Se usa write.csv para crear el archivo sleep_new.csv y asi guardar los datos.
write.csv(sleep_new,file="sleep_new.csv")
#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.