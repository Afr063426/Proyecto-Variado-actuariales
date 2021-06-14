#### Ejercicio 1 de la practica de la semana 7


#### Se programa la simulación entonces se tiene
probabilidadPerdida<-function(tiempo,precio,menor,media,desviacio){
niter<-1e5 ### numero de repeticiones
below<-rep(0,niter)# numero de datos
for(i in 1:niter){
  r<-rnorm(tiempo,mean=media,sd=desviacio) ### numeros aleatorios
  logPrice<-log(precio)+sum(r)### calculo de precio after
  below[i]<-as.numeric(logPrice<log(menor))## se determinan cuantos están por debajo
}
return(mean(below)) ### Se retorna la media
}
### Probabilidad de que al final del dia valga menos de 990
probabilidadPerdida(1,1000,990,0.001,0.015)
### Probabilidad de que al final de 5 dias valga menos de 990
probabilidadPerdida(5,1000,990,0.001,0.015)
### Probabilidad de que dentro de un año valga en mas de 110
### Duda caso anual
1-probabilidadPerdida(1,100,110,0.1,0.2)
### Duda caso diario
1-probabilidadPerdida(253,100,110,0.1/253,0.2/sqrt(253))
### Probabilidad de que se venda por 90 o mas dentro de 2 años valga menos de 90 o más
### Duda caso anual
1-probabilidadPerdida(2,80,90,0.08,0.15)
### Duda caso diario
1-probabilidadPerdida(2*253,80,90,0.08/253,0.15/sqrt(253))
