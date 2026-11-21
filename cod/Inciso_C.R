
#Separamos tabla.anual en dos:

#Una con saldos acumulados:
salidas.acum <- tabla.anual %>% 
  select(anio , contains("acum"))


#Otra con proyecciones activos:
tabla.anual <- tabla.anual %>% 
  relocate(salidas.anuales.total, .before = activos.fin) %>% 
  select(-contains("acum"))



#Tabla anual de Flujos, Reservas y Resultados: ----

tabla.resultados <- tabla.anual %>%
  mutate(
    Reserva.t_1 = reservas.vt[1:5] * activos.inicio,
    Primas.anual = prima * activos.inicio,
    Gastos.anual = gasto.anual.vigente * activos.inicio,
    Comisiones.anual = comision.prima * Primas.anual,
    Fondos.acumulados = calcular.At.acum(Reserva.t_1, Primas.anual, Gastos.anual,  
                                    Comisiones.anual, j),
    Beneficios.anual = beneficio * fallecimientos.anuales,
    Reserva.t = reservas.vt[2:6] * activos.fin,
    Resultado.cartera = Fondos.acumulados - Beneficios.anual - Reserva.t
  ) %>%
  select(-names(tabla.anual)[-1])



#Tabla de Recursos y Aportes ----

F0 <- (100000 - gasto.adquisicion) * N0


eFt <- numeric(5)
aporte <- numeric(5)
Ft <- numeric(5)

saldo.previo <- F0

for(i in 1:5){
  
  eFt[i] <-
    (
      saldo.previo +
        tabla.resultados$Primas.anual[i] -
        tabla.resultados$Gastos.anual[i] -
        tabla.resultados$Comisiones.anual[i]
    ) * (1 + j) -
    tabla.resultados$Beneficios.anual[i]
  
  aporte[i] <- max(0, - eFt[i])
  
  Ft[i] <- eFt[i] + aporte[i]
  
  saldo.previo <- Ft[i]
}

#Saldo minimo antes de aporte:
min(eFt)

#Suma de aportes:
sum(aporte)


# Respaldo año 1:

Respaldo.anio.1 <- Ft[1] - tabla.resultados$Reserva.t[1]

##Tabla final recursos: ----

tabla.recursos <- tibble(anio = 1:5, 
                         Saldo.cierre.ant = eFt,
                         Aportes = aporte,
                         Saldo.cierre.post = Ft)



# Tabla NPV y margen: ----


tabla.descuento.saldos <- tabla.resultados %>% 
  select(anio, Resultado.cartera, Primas.anual) %>% 
  mutate(Descuento = sapply(anio, function(x)(1+interes)^(-x)),
         VP.flujos = Descuento*Resultado.cartera,
         VP.primas = Descuento*(1+interes)*Primas.anual)

tabla.NPV <- tabla.descuento.saldos  %>% 
 summarise(NPV = - gasto.adquisicion*N0 + sum(VP.flujos),
           Margen = NPV/sum(VP.primas))






