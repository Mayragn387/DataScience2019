#----------EJERCICIO PRACTICO 3.2----------
#Cargar dataset
calif.eco<-read.csv("./calificaciones ECO 2019.csv",sep=";",dec=",",header=TRUE)

#a.Cúantos alumnos hay en total y la nota media en el examen final sin considerarar los NP
calif.examen<-calif.eco[is.na(calif.eco$Ex..JUNIO.12P )==FALSE,]
print(paste("Cantidad Total de Alumnos (Sin NP):",nrow(calif.examen)))
print(paste("Nota media en el Examen:",round(mean(calif.examen$Ex..JUNIO.12P))))

#b.Nombre de los que han aprobado el examen final a pesar de haber asistido menos 
#de un 50% a las clases.
calif.ap50<-subset(calif.eco,(calif.eco$Ex..JUNIO.12P>=6)&(calif.eco$Asistencia..1P<0.50))
calif.ap50[]<-lapply(calif.ap50,function(x) if(is.factor(x))factor(x) else x )
calif.ap50<-calif.ap50[,"NOMBRE.ALUMNO"]
calif.ap50<-factor(calif.ap50)

print("Alumnos aprobados en el examen final pero que han asistido menos de un 50%")
calif.ap50

#c.Completa la columna de calificacion con: Aprobado (5-6.9), Notable (7-8.9), Sobresaliente (9-10). Ten encuenta que las calificaciones así descritas deben darse sobre 10
#Para determinar el valor Calificacion se utilizo de base la Nota Final
nro<-nrow(calif.eco)
nt<-c()
for(i in 1:nro){
  v<-calif.eco[i,"Nota_FINAL"]
  nuevaC<-as.character(calif.eco[i,"CALIFICACION"])
  if(nuevaC!= "SUSPENSO"){
    if(v>=9) nuevaC<-"SOBRESALIENTE"
    else if(v>=7) nuevaC<-"NOTABLE"
    else if(v>=5) nuevaC<-"APROBADO"
    else nuevaC<-"SUSPENSO"
    nt[[i]]<-c(nuevaC)
  }
  else nt[[i]]<-c("SUSPENSO")
  #return(calif.eco[i,"CALIFICACION"])
}
nuevo<-data.frame(calif.eco,nt)
nuevo<-nuevo[,-10] #Se elimina la columna anterior de Calificacion
colnames(nuevo)<-c("NOMBRE_ALUMNO","Parcialito_1-6P","Parcialito _1-10P","Parcialito_2-10P","Media_parcialitos","Nota_Labs-2P","Asistencia-1P","Ex.JUNIO-12P","Nota_FINAL","CALIFICACION")

#d.Guarda la tabla modificada en el proyecto Practica2 con el nombre Calificaciones2019.csv
write.csv(nuevo,file="Calificaciones2019.csv",row.names = FALSE,na="")

