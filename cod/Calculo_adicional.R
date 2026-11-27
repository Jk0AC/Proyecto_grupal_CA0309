# Cálculo adicional defensa ronda 1: aumento de prima -----
# Ejecutar desde la raiz del proyecto:
# source("cod/Calculo_adicional_Ronda.R")
# Utiliza funciones de dplyr, ggplot2 gráficos y creaciones 
# propias del script Funciones.R 
# Usa archivos y tablas Inciso C, no modifica archivos de otros incisos.


## Tabla Resultados profit testing escenario adicional: ----

tabla.resultados.adicional <- calcular.resultados(Prima = prima.esc.adcional)

# Recursos y Aportes escenario adicional ----

recursos.adicional <- calcular.recursos(F0, tabla.resultados.adicional, j)


## Tabla final recursos escenario adverso: ----

recursos.adicional[[1]]

# Matriz conparativa de aportes: ----

matriz.comp.aportes.ad <- matrix(c(recursos[4], recursos.adicional[4], 
         recursos[2], recursos.adicional[2], 
         recursos[3], recursos.adicional[3]), 
       ncol = 2, nrow = 3, byrow = T, 
       dimnames = list(c("Respaldo", "Mínimo", "Total"), 
                       c("Base", "Adicional")))


## Tabla NPV y margen escenario adverso: ----

tabla.NPV.adicional <- tabla.resultados.adicional %>% 
  select(anio, Resultado.cartera, Primas.anual) %>% 
  mutate(Descuento = sapply(anio, function(x)(1+interes)^(-x)),
         VP.flujos = Descuento*Resultado.cartera,
         VP.primas = Descuento*(1+interes)*Primas.anual) %>% 
  summarise(NPV = - gasto.adquisicion*N0 + sum(VP.flujos),
            Margen = NPV/sum(VP.primas))

#Matriz NPV base contra adicional:

  
matriz.comp.NPV.ad <- matrix(c(resumen.NPV$NPV, tabla.NPV.adicional$NPV,
                            resumen.NPV$Margen, tabla.NPV.adicional$Margen), 
                          nrow = 2, ncol = 2,
                          byrow = T, 
                          dimnames = list(c("NPV", "Margen"), 
                                          c("Base", "Mayor prima")))


# Tabla comparativa base contra adicional:

tabla.comp.adicional <- tabla.resultados %>% 
  left_join(tabla.resultados.adicional, by = "anio") %>%
  select(anio,  contains("Primas"), contains("Gastos"), 
         contains("Fondos"), contains("Resultado"))


names(tabla.comp.adicional) <- c("anio","Primas.Base", "Primas.Adicional",
                                 "Gasto.Base","Gasto.Adicional", 
                                  "Acumulado.Base", "Acumulado.Adicional",
                                  "Resultado.Base", "Resultado.Adicional") 




# Tabla imprimible: ----
tibble(
  Escenario = c("Base", "Aumento Prima"),
  NPV = c(resumen.NPV$NPV, tabla.NPV.adicional$NPV) / 1e6,
  Margen = c(resumen.NPV$Margen, tabla.NPV.adicional$Margen)*100,
  "Saldo mínimo $\\tilde{F}_t$" = c(recursos$saldo.minimo, recursos.adicional$saldo.minimo) /
    1e6 ,
  "Total de aportes" = c(recursos$aportes.totales, recursos.adicional$aportes.totales) /
    1e6,
  "$\\tilde{F}_1 - V_1$" =  c(recursos$respaldo.anio.1, recursos.adicional$respaldo.anio.1) /
    1e6
) %>%
  mutate(across(-Escenario, \(x) format(
    round(x, 2), big.mark = ".", decimal.mark = ","
  )))





# Gráficos escenario adicional ----


## Gráfico comparación resultados profit testing: ----

datos.comp.resultados <- tabla.comp.adicional %>%
  select(anio, Resultado.Base, Resultado.Adicional) %>%
  tidyr::pivot_longer(
    cols = c(Resultado.Base, Resultado.Adicional),
    names_to = "Escenario",
    values_to = "Resultado"
  ) %>%
  mutate(
    Escenario = recode(
      Escenario,
      "Resultado.Base" = "Prima de 50 000 u.m.",
      "Resultado.Adicional" = "Prima de 55 000 u.m."
    ),
    Resultado = Resultado / 1e6
  )

grafico.comp.resultados <- ggplot(datos.comp.resultados,
       aes(x = anio, y = Resultado,
           color = Escenario, group = Escenario)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:5) +
  scale_color_manual(
    values = c("Prima de 50 000 u.m." = "#2563EB",
               "Prima de 55 000 u.m." = "#E76F51")
  ) +
  labs(
    title = "Comparación de resultados anuales",
    subtitle = "Escenario base vs. incremento de prima",
    x = "Año",
    y = "Resultado (millones de u.m.)",
    color = "Escenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


## Gráfico comparación de flujos: ----

grafico.comp.flujos.ad <- tabla.comp.adicional %>%
  select(
    anio, Primas.Base, Primas.Adicional,
    Acumulado.Base, Acumulado.Adicional
  ) %>%
  pivot_longer(
    cols = -anio,
    names_to = c("Flujo", "Escenario"),
    names_pattern = "(.*)\\.(Base|Adicional)",
    values_to = "Valor"
  ) %>%
  mutate(
    Valor = Valor / 1e6,
    Escenario = recode(
      Escenario,
      Base = "Prima 50 000 u.m.",
      Adicional = "Prima 55 000 u.m."
    )
  ) %>%
  ggplot(aes(anio, Valor, color = Escenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Flujo, scales = "free_y") +
  scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Comparación de flujos por escenario",
    x = "Año",
    y = "Millones de u.m.",
    color = "Escenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold", size = 10),
        strip.text = element_text(face = "bold", size = 10))



## Gráficos Recursos: ----

datos.recursos.ad <- bind_rows(
  recursos[[1]] %>%
    mutate(Escenario = "Prima 50 000"),
  recursos.adicional[[1]] %>%
    mutate(Escenario = "Prima 55 000")
) %>%
  select(anio, Escenario, eFt, Ft) %>%
  tidyr::pivot_longer(
    cols = c(eFt, Ft),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  mutate(
    Valor = Valor / 1e6,
    Tipo = recode(
      Tipo,
      eFt = "Antes del aporte",
      Ft = "Después del aporte"
    )
  )

grafico.recursos.ad <- ggplot(datos.recursos.ad,
       aes(x = anio, y = Valor, color = Escenario)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Tipo, scales = "free_y") +
  scale_x_continuous(breaks = 1:5) +
  scale_color_manual(
    values = c(
      "Prima 50 000" = "#2563EB",
      "Prima 55 000" = "#E76F51"
    )
  ) +
  labs(
    title = "Comparación de recursos por escenario",
    subtitle = "Primas anuales de 50 000 y 55 000 u.m.",
    x = "Año",
    y = "Millones de u.m.",
    color = "Escenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = 10)
        )



## Gráfico aportes: ----

datos.aportes <- bind_rows(
  recursos[[1]] %>%
    mutate(Escenario = "Prima 50 000"),
  recursos.adicional[[1]] %>%
    mutate(Escenario = "Prima 55 000")
) %>%
  mutate(aporte = aporte / 1e6)


graf.comp.ad.aportes <- ggplot(datos.aportes,
       aes(x = factor(anio), y = aporte, fill = Escenario)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c(
      "Prima 50 000" = "#2563EB",
      "Prima 55 000" = "#E76F51"
    )
  ) +
  labs(
    title = "Comparación de aportes requeridos",
    subtitle = "Financiamiento para evitar recursos deficitarios",
    x = "Año",
    y = "Millones de u.m.",
    fill = "Escenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



# Exportación: ----

write.csv(tabla.resultados.adicional, "cod/Adicional_resultados/ AD1_tabla_resultados_ad", row.names = FALSE)
write.csv(recursos.adicional[[1]], "cod/Adicional_resultados/ AD2_tabla_recursos_ad.csv", row.names = FALSE)
write.csv(tabla.NPV.adicional, "cod/Adicional_resultados/ AD3_tabla_flujos_NPV_ad.csv", row.names = FALSE)
write.csv(tabla.comp.adicional, "cod/Adicional_resultados/ AD4_tabla_comp_adicional.csv", row.names = FALSE)
write.csv(matriz.comp.aportes.ad, "cod/Adicional_resultados/ AD5_matriz_comp_aportes_ad.csv", row.names = FALSE)
write.csv(matriz.comp.NPV.ad, "cod/Adicional_resultados/ AD6_matriz_comp_NPV_ad.csv", row.names = FALSE)
