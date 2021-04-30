# Scipt con las fórmulas
library(lifecontingencies)


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
  
}