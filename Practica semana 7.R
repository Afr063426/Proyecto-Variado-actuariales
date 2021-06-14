#### Ejercicio 1 de la practica de la semana 7


#### Se programa la simulaci?n entonces se tiene
probabilidadPerdida<-function(tiempo,precio,menor,media,desviacio){
niter<-1e5 ### numero de repeticiones
below<-rep(0,niter)# numero de datos
for(i in 1:niter){
  r<-rnorm(tiempo,mean=media,sd=desviacio) ### numeros aleatorios
  logPrice<-log(precio)+sum(r)### calculo de precio after
  below[i]<-as.numeric(logPrice<log(menor))## se determinan cuantos est?n por debajo
}
return(mean(below)) ### Se retorna la media
}
### Probabilidad de que al final del dia valga menos de 990
probabilidadPerdida(1,1000,990,0.001,0.015)
### Probabilidad de que al final de 5 dias valga menos de 990
probabilidadPerdida(5,1000,990,0.001,0.015)
### Probabilidad de que dentro de un a?o valga en mas de 110
### Duda caso anual
1-probabilidadPerdida(1,100,110,0.1,0.2)
### Duda caso diario
1-probabilidadPerdida(253,100,110,0.1/253,0.2/sqrt(253))
### Probabilidad de que se venda por 90 o mas dentro de 2 a?os valga menos de 90 o m?s
### Duda caso anual
1-probabilidadPerdida(2,80,90,0.08,0.15)
### Duda caso diario
1-probabilidadPerdida(2*253,80,90,0.08/253,0.15/sqrt(253))



#### Ejercicio 4 
### Primero se crea la tabla de datos
datosEjercicio4<-cbind("t"=1:4,"P_t"=c(82,85,83,87),"D_t"=c(0.1,0.1,0.1,0.125))
###
datosEjercicio4<-as.data.frame(datosEjercicio4)
### Calculo de retorno 
R_3_2<-prod((datosEjercicio4$P_t[3-c(0:(2-1))]+datosEjercicio4$D_t[3-c(0:(2-1))])/datosEjercicio4$P_t[3-c(1:2)])-1
r_3_3<-log(prod((datosEjercicio4$P_t[4-c(0:(3-1))]+datosEjercicio4$D_t[4-c(0:(3-1))])/datosEjercicio4$P_t[4-c(1:3)]))
