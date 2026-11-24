# D. Escenario adverso y respaldo de las reservas ----
# Ejecutar desde la raiz del proyecto:
# source("cod/Inciso_D.R")
#Utiliza funciones de dplyr, R base y creaciones propias del script Funciones.R 
#Usa archivos y tablas Inciso C, no modifica archivos de otros incisos.



## Tabla Resultados profit testing escenario adverso: ----

tabla.resultados.adv <- calcular.resultados(Gasto = gasto.anual.alterno, 
                                            Rendimiento = j.alterno)


## Recursos y Aportes escenario adverso: ----

recursos.adv <- calcular.recursos(F0, tabla.resultados.adv, j.alterno)


#Matriz conparativa de aportes:

matrix(c(recursos[4], recursos.adv[4], 
         recursos[2], recursos.adv[2], 
         recursos[3], recursos.adv[3]), 
         ncol = 2, nrow = 3, byrow = T, 
       dimnames = list(c("Respaldo", "Mínimo", "Total"), 
                       c("Base", "Adverso")))


## Tabla NPV y Margen escenario adverso: ----

tabla.NPV.adv <- tabla.resultados.adv %>% 
  select(anio, Resultado.cartera, Primas.anual) %>% 
  mutate(Descuento = sapply(anio, function(x)(1+interes)^(-x)),
         VP.flujos = Descuento*Resultado.cartera,
         VP.primas = Descuento*(1+interes)*Primas.anual) %>% 
  summarise(NPV = - gasto.adquisicion*N0 + sum(VP.flujos),
            Margen = NPV/sum(VP.primas))


# Matriz NPV base contra adverso:


matriz.comp.NPV <- matrix(c(resumen.NPV$NPV, tabla.NPV.adv$NPV,
                             resumen.NPV$Margen, tabla.NPV.adv$Margen), 
                          nrow = 2, ncol = 2,
                          byrow = T, 
                          dimnames = list(c("NPV", "Margen"), 
                                          c("Base", "Adverso")))


# Tabla comparativa base contra adverso:

tabla.comp.escenarios <- tabla.resultados %>% 
  left_join(tabla.resultados.adv, by = "anio") %>%
  select(anio, contains("Gastos"), contains("Fondos"), contains("Resultado"))
  
  
names(tabla.comp.escenarios) <- c("anio","Gasto.Base","Gasto.Adverso", 
                                  "Acumulado.Base", "Acumulado.Adverso",
                                  "Resultado.Base", "Resultado.Adverso") 


# Tablas imprimibles Reporte ----

tabla.comp.adv <- tibble(
  Escenario = c("Base", "Adverso"),
  NPV = c(resumen.NPV$NPV, tabla.NPV.adv$NPV) / 1e6,
  "Saldo mínimo $\\tilde{F}_t$" = c(recursos$saldo.minimo, recursos.adv$saldo.minimo) /
    1e6 ,
  "Total de aportes" = c(recursos$aportes.totales, recursos.adv$aportes.totales) /
    1e6,
  "$\\tilde{F}_1 - V_1$" =  c(recursos$respaldo.anio.1, recursos.adv$respaldo.anio.1) /
    1e6
) %>%
  mutate(across(-Escenario, \(x) format(
    round(x, 2), big.mark = ".", decimal.mark = ","
  )))






