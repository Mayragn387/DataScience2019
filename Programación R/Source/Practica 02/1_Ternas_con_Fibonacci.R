#----------EJERCICIO PRACTICO 3.1----------
#Funcion Fibonacci
#Se usa una funcion con recursion con simple complejidad, donde se envia n(numero de cuaternas fibonacci generadas)
#f1 y f2 los primeros dos valores de la secuencia fibonacci
cuaternaf<- function(n, f1, f2){
  if(n==0) return(f1)
  else if (n==1) return(f2)
  else return(cuaternaf(n-1,f2, f1+f2))
}

#Calculo de las ternas
#La funcion terna calcula las ternas pitagoricas en base a 4 numeros consecutivos de la secuencia fibonacci
#donde n es el nro de ternas pitagoricas que se generaran.
v<-c()
nt<-c()
nternas<-function(n){
  for(i in 1:n){
    for(j in 1:4){
      s<-(i+j-1)
      v[[j]]<-c(cuaternaf(s,0,1))
    }
    
    a<-(v[1]*v[4])
    b<-(2*v[2]*v[3])
    h<-(v[2]*v[2]+v[3]*v[3])

    nt[[i]]<-c(a,b,h)
    print(paste("Terna ",i,":",nt[i]))
  }
  return(nt)
}
#Ejecucion de la funcion
#Ejecucion de ejemplos de 3 y 5 ternas pitagoricas
nternas(3)

nternas(5)

