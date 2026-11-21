# C. Flujos, reservas suministradas y profit testing ----


#Separamos tabla.anual en dos:

#Una con saldos acumulados:
salidas.acum <- tabla.anual %>% 
  select(anio , contains("acum"))


#Otra con proyecciones activos:
tabla.anual <- tabla.anual %>% 
  relocate(salidas.anuales.total, .before = activos.fin) %>% 
  select(-contains("acum"))



## Tabla anual de Flujos, Reservas y Resultados: ----

tabla.resultados <- calcular.resultados()


## Recursos y Aportes ----

F0 <- (100000 - gasto.adquisicion) * N0

# Lista con: [1] tabla.recursos, [2] Saldo min, [3] Aportes tot y [4] Respaldo:

recursos <- calcular.recursos(F0, tabla.resultados, j)

recursos[1]

recursos[2]

recursos[3]

recursos[4]

## Tabla NPV y margen: ----


tabla.descuento.saldos <- tabla.resultados %>% 
  select(anio, Resultado.cartera, Primas.anual) %>% 
  mutate(Descuento = sapply(anio, function(x)(1+interes)^(-x)),
         VP.flujos = Descuento*Resultado.cartera,
         VP.primas = Descuento*(1+interes)*Primas.anual)

tabla.NPV <- tabla.descuento.saldos  %>% 
 summarise(NPV = - gasto.adquisicion*N0 + sum(VP.flujos),
           Margen = NPV/sum(VP.primas))






