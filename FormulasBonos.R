# Scipt con las fórmulas
library(lifecontingencies)
install.packages('lubridate')

#Casos de periodos completos
precio_sucio <- function(C, m, r, F, M, n) {
  M * ((1 + r / m) ^ (-(n * m))) + F * C / m * annuity(r, n, 0, m)
}
#Caso falta más de cupón
precio_limpio <- function(C, m, r, F, M, n, i, d) {
  if (n > 1) {
    precio_sucio(C, m, r, F, M, n) * (1 + r) ^ (n - i / d) - C / m * F(d - i) /
      d
    
  } else{
    (M + C * F / m) / (1 + r / m * (d - i) / d) -  C * F / m * (d - i) / d
  }
}

##Running yield, esta falta revisarla, en el
rc <- function(C, P) {
  C / P * 100
  
}


##Función duración de Macaulay

d_Macaulay <- function(P, C, r) {
  (sum(c(1:length(C)) * C * (1 + r) ^ (-c(1:length(
    C
  ))))) / P
}
d_Macaulay(94.845806,c(rep(6,3),106),0.08)
d_Macaulay(101.886,c(5,105),0.05)/(1.05)*0.01
d_Macaulay(100,c(6,106),0.06)/(1.06)*0.01
d_Macaulay(97.327,c(5,105),0.05)/(1+0.05)*0.01
#Función duración modificada, hay una alternativa en este caso se pasa por Macaulay
#la alternativa pasa por derivar el precioy dividirlo entre el precio
d_Modificada <- function(P, C, r) {
  -1 / (1 + r) * d_Macaulay(P, C, r)
}



## Tasas Spot 
t_spot<-function(P,M){
  (M/(P))^(1/c(1:length(P)))-1
}


##Tasas Forward
t_forward<-function(P,M){
  t_s<-t_spot(P,M)
  t_s[-1]^c(2:length(t_s))/(t_s[-length(t_s)]^c(1:length(t_s[-length(t_s)])))-1
}

<<<<<<< HEAD


precio_sucio(0.055,1,0.05,100,100,3)
=======
## Balance en el período l

b_l<-function(A,r,k,n,l){
  ((1+r/k)^n-(1+r/k)^l)*A/((1+r/k)^n-1)
  
}

I_l<-function(A,r,k,n,l){
  r/k*b_l(A,r,k,n,l)
}
>>>>>>> f1d2f459d81b6ab9abf4feaaf1cc912c52d27a19
