# C. Flujos, reservas suministradas y profit testing ----
#Ejecutar desde la raiz del proyecto: source("cod/Inciso_C.R")
#Utiliza funciones de dplyr, R base y creaciones propias del script Funciones.R 
#Usa archivos y tablas Inciso A, modifica archivos de otros incisos: tabla.anual.


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

## Tabla NPV y margen: ----


tabla.NPV <- tabla.resultados %>% 
  select(anio, Resultado.cartera, Primas.anual) %>% 
  mutate(Descuento = sapply(anio, function(x)(1+interes)^(-x)),
         VP.flujos = Descuento*Resultado.cartera,
         VP.primas = Descuento*(1+interes)*Primas.anual)

resumen.NPV <- tabla.NPV %>% 
 summarise(NPV = - gasto.adquisicion*N0 + sum(VP.flujos),
           Margen = NPV/sum(VP.primas))


# Tablas imprimibles Reporte: ----

### Tabla flujos descuento NPV:----
tabla.flujos.descontados <- tabla.NPV %>%
  transmute(
    Año = anio,
    Descuento = round(Descuento, 4),
    `VP Resultado` = VP.flujos / 1e6,
    `VP Primas` = VP.primas / 1e6
  ) %>%  
  mutate(
    across(
      -Año,
      \(x) format(round(x, 2),
                  big.mark = ".",
                  decimal.mark = ",")
    ))

### Tabla flujos indicadores NPV y Margen:----
tabla.indicador <- tibble(
  Métrica = c("NPV", "Margen"),
  Valor = c(
    format(
      round(resumen.NPV$NPV / 1e6, 2),
      big.mark = ".",
      decimal.mark = ","
    ),
    scales::percent(
      resumen.NPV$Margen,
      accuracy = 0.01
    )
  )
)

### Tabla Recursos y Aportes:----

tabla.recursos <-  recursos[[1]] %>%
  transmute(
    Año = anio,
    eFt = eFt / 1e6,
    Aporte = aporte / 1e6,
    Ft = Ft / 1e6
  ) %>%  
  mutate(
    across(
      -Año,
      \(x) format(round(x, 2),
                  big.mark = ".",
                  decimal.mark = ",")
    ))


#Exportación: ----


write.csv(tabla.resultados, "cod/Inciso_C_resultados/ C1_tabla_resultados.csv", row.names = FALSE)
write.csv(recursos[[1]], "cod/Inciso_C_resultados/ C2_tabla_recursos.csv", row.names = FALSE)
write.csv(tabla.NPV, "cod/Inciso_C_resultados/ C3_tabla_flujos_NPV.csv", row.names = FALSE)


