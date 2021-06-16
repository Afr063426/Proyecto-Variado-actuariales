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
### Funcion para ver si es superior a un precio
probabilidadGanancia<-function(tiempo,precio,mayor,media,desviacio){
  niter<-1e5 ### numero de repeticiones
  up<-rep(0,niter)# numero de datos
  for(i in 1:niter){
    r<-rnorm(tiempo,mean=media,sd=desviacio) ### numeros aleatorios
    logPrice<-log(precio)+sum(r)### calculo de precio after
    up[i]<-as.numeric(logPrice>log(mayor))## se determinan cuantos est?n por debajo
  }
  return(mean(up)) ### Se retorna la media
}
probabilidadGanancia(20,97,100,0.0002,0.03)


#### Ejercicio 6 la suma de normales es normal entonces veamos cu?nto es el m?nimo para que la probabilidad
### de que valga el doble en una probabilidad mayor a 0.9
### Entonces se programa con un while y entonces esto coincide 1 menos la probabilidad de que sea menor al doble
##### Da un numero muy grande si se cambia a 0.05 queda en 21 dias
i=1
mayor=F
while(!mayor){
  mayor=(1-pnorm(log(2),i*0.05,i*0.012))>=0.9
  i=i+1
}
i-1


i=1
mayor=F
while (!mayor) {
  mayor=(1-probabilidadPerdida(i,1,2,0.0005,0.012))>=0.9
  i=i+1
}




### Esto no se hace pero aquí se va poner la parte 9

### Ejercicio 1
100*exp(0.1*sqrt(1/252)*252)
100*(1+0.1*sqrt(1/252)*252+0.1*sqrt(1/252)*252/2)


### Ejercicio 2

###100 periodos
u_n=exp(0.15*1/sqrt(4)*1/sqrt(100))
d_n=exp(-0.15*1/sqrt(4)*1/sqrt(100))
m=0.08/4
p_n=(exp(m*1/100)-d_n)/(u_n-d_n)
45*(u_n*p_n+(1-p_n)*d_n)^(100)

### 80 periodos
u_n=exp(0.15*1/sqrt(4)*1/sqrt(80))
d_n=exp(-0.15*1/sqrt(4)*1/sqrt(80))
m=0.08/4
p_n=(exp(m*1/80)-d_n)/(u_n-d_n)
45*(u_n*p_n+(1-p_n)*d_n)^(80)


### Ejercicio 5.14 

### Si es posible obtener el mismo precio ya que se divide en un numero par de tal forma que basta que suba y baje la misma cantidadd veces


### La probabilidad de que ocurra este precio es
choose(80,40)*p_n^(40)*(1-p_n)^(40)


###La probabilidad de que sea mayor es
1-sum(choose(80,0:40)*p_n^(0:40)*(1-p_n)^(80-0:40))

