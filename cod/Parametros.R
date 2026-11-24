# Parámetros generales ----
# Ejecutar desde la raiz del proyecto:
# source("cod/Parametros.R")
# Define variables generales usadas para la resolución de ejercicios 
# No modifica archivos de otros incisos.

## Base: ----
fecha.ref <- ymd("2025-12-31")

r <- 0.03 # renuncia anual (decremento único)

horizonte <- 5

prima <- 50000

gasto.adquisicion <- 75000  #Por prima

beneficio <- 5000000

gasto.anual.vigente <- 10000

comision.prima <- 0.05

j <- 0.05   # rendimiento

interes <- 0.04   # descuento

reservas.vt <- c(0, 40000, 35000, 25000, 15000, 0)

## Escenarios adversos: ----
gasto.anual.alterno <- 11000
j.alterno <- 0.04

## Escenario adicional: ----
prima.esc.adcional <- 55000


