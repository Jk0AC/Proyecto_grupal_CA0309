
#Funcion calcula At y devuelve su valor acumulado por el rendimiento al final del
#año:

calcular.At.acum <- function(V.ini, prima, gasto, comision, rendimiento){
  
  At <- V.ini + prima - gasto - comision
  
  At.acum <- At * (1 + rendimiento)
  
  return(At.acum)
}

