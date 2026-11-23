
# D. Escenario adverso y respaldo de las reservas ----

## Tabla Resultados escenario adverso: ----

tabla.resultados.adv <- calcular.resultados(Gasto = gasto.anual.alterno, 
                                            Rendimiento = j.alterno)


## Recursos y Aportes escenario adverso ----

recursos.adv <- calcular.recursos(F0, tabla.resultados.adv, j.alterno)


#Matriz conparativa de aportes:

matrix(c(recursos[4], recursos.adv[4], 
         recursos[2], recursos.adv[2], 
         recursos[3], recursos.adv[3]), 
         ncol = 2, nrow = 3, byrow = T, 
       dimnames = list(c("Respaldo", "Mínimo", "Total"), 
                       c("Base", "Adverso")))


### Tabla final recursos escenario adverso: ----

recursos.adv[1]

## Tabla NPV y margen escenario adverso: ----

tabla.NPV.adv <- tabla.resultados.adv %>% 
  select(anio, Resultado.cartera, Primas.anual) %>% 
  mutate(Descuento = sapply(anio, function(x)(1+interes)^(-x)),
         VP.flujos = Descuento*Resultado.cartera,
         VP.primas = Descuento*(1+interes)*Primas.anual) %>% 
  summarise(NPV = - gasto.adquisicion*N0 + sum(VP.flujos),
            Margen = NPV/sum(VP.primas))


#Matriz NPV base contra adverso:


matriz.comp.NPV <- matrix(c(tabla.NPV$NPV, tabla.NPV.adv$NPV,
                             tabla.NPV$Margen, tabla.NPV.adv$Margen), 
                          nrow = 2, ncol = 2,
                          byrow = T, 
                          dimnames = list(c("NPV", "Margen"), 
                                          c("Base", "Adverso")))


#Tabla comparativa base contra adverso:

tabla.comp.escenarios <- tabla.resultados %>% 
  left_join(tabla.resultados.adv, by = "anio") %>%
  select(anio, contains("Gastos"), contains("Fondos"), contains("Resultado"))
  
  
names(tabla.comp.escenarios) <- c("anio","Gasto.Base","Gasto.Adverso", 
                                  "Acumulado.Base", "Acumulado.Adverso",
                                  "Resultado.Base", "Resultado.Adverso") 






