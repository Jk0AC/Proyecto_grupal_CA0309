# E. Equity-linked y rentabilidad estocastica ----
# CA-0309 | Grupo 1
# Ejecutar desde la raiz del proyecto:
# source("cod/Inciso_E.R")
# O: Rscript cod/Inciso_E.R
# Solo utiliza funciones de R base. No modifica archivos de otros incisos.

# Parametros del enunciado ----
E_semilla <- 2026L
E_n <- 1000L
E_h <- 5L
E_edad <- 45L
E_A0 <- 100000
E_cargo <- 0.02
E_m <- 0.04
E_sigma <- 0.15
E_r <- 0.03
E_descuento <- 0.04
E_garantias <- c(100000, 110000)

# Ubicacion del proyecto ----
# Acepta: ejecucion desde la raiz del repositorio o con Rscript desde
# cualquier carpeta; si se usa source(), desde la raiz o desde cod/.
E_argumentos <- commandArgs(trailingOnly = FALSE)
E_file_arg <- grep("^--file=", E_argumentos, value = TRUE)
E_candidatos <- c(getwd(), dirname(getwd()))
if (length(E_file_arg) > 0L) {
  E_script <- normalizePath(sub("^--file=", "", E_file_arg[1]),
                            mustWork = FALSE)
  E_candidatos <- c(dirname(dirname(E_script)), E_candidatos)
}
E_candidatos <- unique(normalizePath(E_candidatos, mustWork = FALSE))
E_existe <- vapply(
  E_candidatos,
  function(p) file.exists(file.path(p, "data", "CA0309_Mortalidad_CR2023.csv")),
  logical(1)
)
if (!any(E_existe)) {
  stop("No se encontro data/CA0309_Mortalidad_CR2023.csv. ",
       "Ejecute desde la raiz del repositorio.")
}
E_raiz <- E_candidatos[which(E_existe)[1]]
E_salida <- file.path(E_raiz, "cod", "Inciso_E_resultados")
dir.create(E_salida, recursive = TRUE, showWarnings = FALSE)

# Mortalidad por edad y sexo ----
E_mort <- read.csv(
  file.path(E_raiz, "data", "CA0309_Mortalidad_CR2023.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
E_columnas <- c("edad", "qx_masculino")
if (!all(E_columnas %in% names(E_mort))) {
  stop("El CSV debe tener las columnas edad y qx_masculino.")
}
E_edades <- E_edad + seq_len(E_h) - 1L
if (anyDuplicated(E_mort$edad)) stop("Hay edades repetidas en mortalidad.")
E_filas <- match(E_edades, E_mort$edad)
if (anyNA(E_filas)) stop("No estan todas las edades 45 a 49 en mortalidad.")
E_q <- as.numeric(E_mort$qx_masculino[E_filas])
if (anyNA(E_q) || any(E_q < 0 | E_q >= 1)) {
  stop("Probabilidades qx invalidas.")
}

# Supuesto: fuerzas constantes y causas competidoras (muerte, renuncia).
# mu = -log(1-qx) y nu = -log(1-r).
# La permanencia contractual anual es exp(-mu-nu)=(1-qx)*(1-r).
E_p_anual <- (1 - E_q) * (1 - E_r)
E_p_cierre <- cumprod(E_p_anual)
E_p_inicio <- c(1, head(E_p_cierre, -1))
stopifnot(length(E_p_inicio) == E_h,
          all(E_p_inicio >= 0 & E_p_inicio <= 1),
          all(E_p_cierre >= 0 & E_p_cierre <= 1),
          all(diff(E_p_cierre) <= 0))

E_tabla_perm <- data.frame(
  anio = seq_len(E_h),
  edad_inicio = E_edades,
  qx_masculino = E_q,
  r_unico = rep(E_r, E_h),
  p_permanencia_anual = E_p_anual,
  p_inicio = E_p_inicio,
  p_cierre = E_p_cierre
)

# Simulacion financiera ----
# Z independientes entre anios y trayectorias; R reproduce con set.seed.
# IMPORTANTE: con la misma semilla, R y Python generan distintas muestras.
set.seed(E_semilla)
E_Z <- matrix(rnorm(E_n * E_h), nrow = E_n, ncol = E_h)
E_logrend <- E_m + E_sigma * E_Z
E_A_inicio <- matrix(NA_real_, nrow = E_n, ncol = E_h)
E_cargos <- matrix(NA_real_, nrow = E_n, ncol = E_h)
E_A_postcargo <- matrix(NA_real_, nrow = E_n, ncol = E_h)
E_A_cierre <- matrix(NA_real_, nrow = E_n, ncol = E_h)
E_VP_cargos <- matrix(NA_real_, nrow = E_n, ncol = E_h)
E_saldo <- rep(E_A0, E_n)

for (t in seq_len(E_h)) {
  E_A_inicio[, t] <- E_saldo
  E_cargos[, t] <- E_cargo * E_saldo
  E_A_postcargo[, t] <- E_saldo - E_cargos[, t]
  E_saldo <- E_A_postcargo[, t] * exp(E_logrend[, t])
  E_A_cierre[, t] <- E_saldo

  # Cargo al INICIO de t, en tiempo t-1.
  E_VP_cargos[, t] <- E_cargos[, t] * E_p_inicio[t] /
    ((1 + E_descuento)^(t - 1L))
}
E_VP_cargos_total <- rowSums(E_VP_cargos)
E_saldo_final <- E_A_cierre[, E_h]

# Complemento del proveedor al cierre de 5, solo si persiste el contrato.
E_factor_garantia <- E_p_cierre[E_h] / (1 + E_descuento)^E_h
E_complemento <- sapply(E_garantias, function(G) pmax(G - E_saldo_final, 0))
E_VP_complemento <- E_complemento * E_factor_garantia
E_NPV <- matrix(E_VP_cargos_total, nrow = E_n, ncol = length(E_garantias)) -
  E_VP_complemento
colnames(E_complemento) <- paste0("G", E_garantias)
colnames(E_VP_complemento) <- paste0("G", E_garantias)
colnames(E_NPV) <- paste0("G", E_garantias)

# Riesgo: L = -NPV, y CTE95 es la media de las 50 MAYORES perdidas
# en las 1000 trayectorias, incluso cuando esas "perdidas" sean negativas.
E_CTE95 <- function(npv) {
  L <- -npv
  mean(sort(L, decreasing = TRUE)[seq_len(ceiling(length(L) * 0.05))])
}
E_resumen <- data.frame(
  garantia = E_garantias,
  VP_medio_complemento = colMeans(E_VP_complemento),
  NPV_medio_proveedor = colMeans(E_NPV),
  Frecuencia_perdida = colMeans(E_NPV < 0),
  CTE95_perdida = apply(E_NPV, 2, E_CTE95),
  Frecuencia_activacion_garantia = colMeans(E_complemento > 0),
  row.names = NULL
)

# Comprobaciones cuantitativas ----
E_tol <- 1e-8
E_pruebas <- c(
  "1000 trayectorias por 5 anios" = identical(dim(E_Z), c(E_n, E_h)),
  "Cargos igual en ambas garantias" =
    all(is.finite(E_VP_cargos_total)),
  "Saldo inicio del anio siguiente coincide con cierre anterior" =
    isTRUE(all.equal(E_A_inicio[, -1, drop = FALSE],
                     E_A_cierre[, -E_h, drop = FALSE], tolerance = 1e-12)),
  "Cargos deducidos antes de rendimiento" =
    isTRUE(all.equal(E_A_postcargo, E_A_inicio - E_cargos, tolerance = 1e-12)),
  "Evolucion anual de la cuenta" =
    isTRUE(all.equal(E_A_cierre,
                     E_A_postcargo * exp(E_logrend), tolerance = 1e-12)),
  "Garantia mayor nunca reduce complemento" =
    all(E_complemento[, 2] + E_tol >= E_complemento[, 1]),
  "Garantia mayor nunca aumenta NPV" =
    all(E_NPV[, 2] <= E_NPV[, 1] + E_tol),
  "NPV igual VP cargos menos VP complemento" =
    isTRUE(all.equal(E_NPV,
                     matrix(E_VP_cargos_total, nrow = E_n, ncol = 2) -
                       E_VP_complemento, tolerance = 1e-12)),
  "Probabilidades de permanencia validas" =
    all(E_p_inicio >= 0 & E_p_inicio <= 1) &&
    all(E_p_cierre >= 0 & E_p_cierre <= 1),
  "Frecuencias en [0,1]" =
    all(E_resumen$Frecuencia_perdida >= 0 &
          E_resumen$Frecuencia_perdida <= 1) &&
    all(E_resumen$Frecuencia_activacion_garantia >= 0 &
          E_resumen$Frecuencia_activacion_garantia <= 1),
  "CTE95 promedia exactamente las 50 mayores perdidas" =
    ceiling(E_n * 0.05) == 50L
)
E_verificaciones <- data.frame(
  verificacion = names(E_pruebas),
  cumple = ifelse(E_pruebas, "SI", "NO"),
  row.names = NULL
)
if (!all(E_pruebas)) {
  stop("Fallo comprobacion del inciso E: ",
       paste(names(E_pruebas)[!E_pruebas], collapse = "; "))
}

# Tabla por trayectoria: permite rehacer todo el resumen.
E_trayectorias <- data.frame(
  trayectoria = seq_len(E_n),
  VP_cargos = E_VP_cargos_total,
  saldo_final_cuenta = E_saldo_final,
  complemento_G100000 = E_complemento[, 1],
  VP_complemento_G100000 = E_VP_complemento[, 1],
  NPV_G100000 = E_NPV[, 1],
  complemento_G110000 = E_complemento[, 2],
  VP_complemento_G110000 = E_VP_complemento[, 2],
  NPV_G110000 = E_NPV[, 2]
)
# Una trayectoria de ejemplo, con comprobacion del pago final.
E_detalle <- data.frame(
  trayectoria = rep(1L, E_h),
  anio = seq_len(E_h),
  edad = E_edades,
  normal_Z = E_Z[1, ],
  logrendimiento = E_logrend[1, ],
  saldo_inicio = E_A_inicio[1, ],
  cargo_inicio = E_cargos[1, ],
  saldo_tras_cargo = E_A_postcargo[1, ],
  saldo_cierre = E_A_cierre[1, ],
  p_inicio = E_p_inicio,
  VP_cargo = E_VP_cargos[1, ],
  complemento_G100000_final = c(rep(0, E_h - 1), E_complemento[1, 1]),
  VP_complemento_G100000_final = c(rep(0, E_h - 1), E_VP_complemento[1, 1]),
  NPV_G100000_final = c(rep(NA_real_, E_h - 1), E_NPV[1, 1]),
  complemento_G110000_final = c(rep(0, E_h - 1), E_complemento[1, 2]),
  VP_complemento_G110000_final = c(rep(0, E_h - 1), E_VP_complemento[1, 2]),
  NPV_G110000_final = c(rep(NA_real_, E_h - 1), E_NPV[1, 2])
)

# Archivos de resultados ----
E_exportar <- function(x, nombre) {
  write.csv(x, file.path(E_salida, nombre), row.names = FALSE, na = "")
}
E_exportar(E_tabla_perm, "E1_permanencia.csv")
E_exportar(E_resumen, "E2_comparacion_garantias.csv")
E_exportar(E_trayectorias, "E3_NPV_trayectorias.csv")
E_exportar(E_detalle, "E4_detalle_trayectoria_1.csv")
E_exportar(E_verificaciones, "E6_verificaciones.csv")

# Histogramas: mismos breaks, limites y eje vertical para ambas garantias.
E_todos_npv <- as.vector(E_NPV)
E_breaks <- pretty(range(E_todos_npv), n = 30)
E_h1 <- hist(E_NPV[, 1], breaks = E_breaks, plot = FALSE)
E_h2 <- hist(E_NPV[, 2], breaks = E_breaks, plot = FALSE)
E_ymax <- max(c(E_h1$counts, E_h2$counts)) * 1.10
png(file.path(E_salida, "E5_histogramas_NPV.png"),
    width = 1500, height = 680, res = 140)
E_par_previo <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 3.8, 1.3))
hist(E_NPV[, 1], breaks = E_breaks, xlim = range(E_breaks),
     ylim = c(0, E_ymax), main = "Garantia = 100 000",
     xlab = "NPV del proveedor (u.m.)",
     ylab = "Numero de trayectorias", col = "gray75", border = "white")
abline(v = 0, lty = 2, lwd = 2)
hist(E_NPV[, 2], breaks = E_breaks, xlim = range(E_breaks),
     ylim = c(0, E_ymax), main = "Garantia = 110 000",
     xlab = "NPV del proveedor (u.m.)",
     ylab = "Numero de trayectorias", col = "gray75", border = "white")
abline(v = 0, lty = 2, lwd = 2)
par(E_par_previo)
dev.off()

cat("\nInciso E - Simulacion en R (semilla 2026)\n")
print(E_resumen, row.names = FALSE)
cat("\nVerificaciones:\n")
print(E_verificaciones, row.names = FALSE)
cat("\nArchivos exportados a:", E_salida, "\n")
