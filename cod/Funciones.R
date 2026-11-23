

# Función crea Matriz de transición:

matriz.transicion <- function(p, qr, qd) {
  matrix(c(p, qr, qd,
           0, 1,  0,
           0, 0,  1), nrow = 3, byrow = TRUE,
         dimnames = list(c("Activo", "Renuncia", "Fallecimiento"),
                         c("Activo", "Renuncia", "Fallecimiento")))
}


# Flujos, profit testing y Escenarios adversos ----
#Funcion calcula At y devuelve su valor acumulado por el rendimiento al final del
#año:

calcular.At.acum <- function(V.ini, prima, gasto, comision, rendimiento){
  
  At <- V.ini + prima - gasto - comision
  
  At.acum <- At * (1 + rendimiento)
  
  return(At.acum)
}



#Función con párametros escenario base por defecto, devuelve una tabla
#con los resultados: flujos, Reservas y Resultados de toda la cartera:

calcular.resultados <- function(Tabla = tabla.anual, Reservas = reservas.vt, 
                                Prima = prima , Comision = comision.prima, 
                                Gasto = gasto.anual.vigente,
                                Beneficio = beneficio, Rendimiento = j){
  
  Tabla %>%
    mutate(
      Reserva.t.1 = Reservas[1:5] * activos.inicio,
      
      Primas.anual = Prima * activos.inicio,
      
      Gastos.anual = Gasto * activos.inicio,
      
      Comisiones.anual = Comision * Primas.anual,
      
      Fondos.acumulados = calcular.At.acum(
        Reserva.t.1,
        Primas.anual,
        Gastos.anual,
        Comisiones.anual,
        Rendimiento
      ),
      
      Beneficios.anual = Beneficio * fallecimientos.anuales,
      
      Reserva.t = Reservas[2:6] * activos.fin,
      
      Resultado.cartera =
        Fondos.acumulados -Beneficios.anual - Reserva.t
    ) %>%
    select(-names(Tabla)[-1])
  
}



#Función recibe los recursos iniciales, la tabla con resultados y el 
#rendimiento anual:

calcular.recursos <- function(F0, tabla, j){
  
  n <- nrow(tabla)
  
  eFt <- numeric(n)
  aporte <- numeric(n)
  Ft <- numeric(n)
  
  saldo.previo <- F0
  
  for(i in seq_len(n)){
    
    eFt[i] <-
      (
        saldo.previo +
          tabla$Primas.anual[i] -
          tabla$Gastos.anual[i] -
          tabla$Comisiones.anual[i]
      ) * (1 + j) -
      tabla$Beneficios.anual[i]
    
    aporte[i] <- max(0, -eFt[i])
    
    Ft[i] <- eFt[i] + aporte[i]
    
    saldo.previo <- Ft[i]
  }
  
  list(
    tabla.recursos = tibble(
      anio = 1:5,
      eFt = eFt,
      aporte = aporte,
      Ft = Ft
    ),
    saldo.minimo = min(eFt),
    aportes.totales = sum(aporte),
    respaldo.anio.1 = eFt[1] - tabla$Reserva.t[1]
  )
}



