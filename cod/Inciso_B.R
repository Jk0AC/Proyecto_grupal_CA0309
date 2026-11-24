# ---- 0. Parámetros -----------------------------------------------------------
horizonte <- 5
edad.max.conyuge <- 106      # si y > 106, se excluye el registro solo de B
tasa.desc <- 0.04            # tasa de descuento anual efectivo del proyecto (d = 4 %)
v <- 1 / (1 + tasa.desc)

dir.create("cod/Inciso_B_resultados", showWarnings = FALSE)

# ---- 1. Insumos --------------------------------------------------------------
tabla.mortalidad <- read.delim("data/CA0309_Mortalidad_CR2023.csv", sep = ",")
names(tabla.mortalidad) <- c("edad", "M", "F")
mort.larga <- tabla.mortalidad %>%
  pivot_longer(c(M, F), names_to = "sexo", values_to = "q")

cohortes <- read.csv("cod/Inciso_A_resultados/A3_poblacion_inicial_edad_sexo.csv")
N0 <- sum(cohortes$N0)

# ---- 2. Pareja hipotética y edad asignada del cónyuge -------------------------
delta <- function(x) case_when(x < 35 ~ 0, x < 55 ~ 3, TRUE ~ 5)  # válido para x >= 30

cohortes.b <- cohortes %>%
  mutate(pareja = edad0 >= 30,                        # I_pareja(x)
         delta  = if_else(pareja, delta(edad0), NA_real_),
         sexo.conyuge = if_else(sexo == "M", "F", "M"),
         edad.conyuge = case_when(!pareja ~ NA_real_,
                                  sexo == "M" ~ edad0 - delta,
                                  TRUE        ~ edad0 + delta),
         # Admisibilidad: y <= 106 y probabilidades disponibles hasta x+4 y y+4
         excl.b = pareja & (edad.conyuge > edad.max.conyuge |
                            edad.conyuge < 0 |
                            edad.conyuge + horizonte - 1 > max(tabla.mortalidad$edad) |
                            edad0 + horizonte - 1 > max(tabla.mortalidad$edad)),
         motivo.b = case_when(!pareja ~ "Sin pareja (x < 30): no genera pagos en B",
                              excl.b  ~ "Excluido de B: edad del cónyuge > 106 o faltan probabilidades",
                              TRUE    ~ "Con pareja admisible"))

# ---- 3. Cuadro de activos con y sin pareja, exclusiones propias de B ----------
cuadro.b <- cohortes.b %>%
  group_by(sexo) %>%
  summarise(activos = sum(N0),
            sin.pareja = sum(N0[!pareja]),
            con.pareja = sum(N0[pareja]),
            excluidos.b = sum(N0[excl.b]),
            con.pareja.admisible = sum(N0[pareja & !excl.b]), .groups = "drop") %>%
  bind_rows(summarise(., across(where(is.numeric), sum)) %>% mutate(sexo = "Total"))
cuadro.b
stopifnot(with(cuadro.b[cuadro.b$sexo == "Total", ],
               activos == N0 & sin.pareja + con.pareja == N0 &
               con.pareja - excluidos.b == con.pareja.admisible))

# Edades asignadas (una fila por sexo y edad inicial)
edades.asignadas <- cohortes.b %>%
  filter(pareja) %>%
  select(sexo, edad.activo = edad0, delta, sexo.conyuge, edad.conyuge, excl.b, N0)

# ---- 4. Probabilidades de supervivencia por estatus ---------------------------
# Año t (cierre): edades al inicio del año x + t - 1 y y + t - 1; solo mortalidad
adm <- cohortes.b %>% filter(pareja, !excl.b)

prob <- adm %>%
  crossing(anio = 1:horizonte) %>%
  mutate(edad.x = edad0 + anio - 1, edad.y = edad.conyuge + anio - 1) %>%
  left_join(mort.larga %>% rename(qx = q), by = c("sexo" = "sexo", "edad.x" = "edad")) %>%
  left_join(mort.larga %>% rename(qy = q),
            by = c("sexo.conyuge" = "sexo", "edad.y" = "edad")) %>%
  arrange(sexo, edad0, anio) %>%
  group_by(sexo, edad0) %>%
  mutate(px.t  = cumprod(1 - qx),                 # activo vivo al cierre del año t
         py.t  = cumprod(1 - qy),                 # cónyuge vivo
         p.ambos      = px.t * py.t,              # ambos vivos (independencia)
         p.alguno     = px.t + py.t - p.ambos,    # al menos uno vivo (último sobreviviente)
         p.conyuge.sob = py.t * (1 - px.t),       # cónyuge vivo y activo fallecido
         p.activo.sob  = px.t * (1 - py.t),       # (auxiliar para verificar)
         vt = v^anio) %>%
  ungroup()

stopifnot(!anyNA(prob$qx), !anyNA(prob$qy))

# Verificaciones de coherencia probabilística
verif.prob <- tibble(
  verificacion = c("alguno = ambos + cónyuge sobreviviente + activo sobreviviente",
                   "alguno = P(x) + P(y) - P(ambos)",
                   "ambos <= cónyuge sobreviviente + ambos = P(y) <= alguno",
                   "todas las probabilidades en [0,1]"),
  resultado = c(
    all(abs(prob$p.alguno - (prob$p.ambos + prob$p.conyuge.sob + prob$p.activo.sob)) < 1e-12),
    all(abs(prob$p.alguno - (prob$px.t + prob$py.t - prob$p.ambos)) < 1e-12),
    all(prob$p.ambos <= prob$py.t + 1e-12 & prob$py.t <= prob$p.alguno + 1e-12),
    all(prob[c("p.ambos", "p.alguno", "p.conyuge.sob")] >= 0 &
        prob[c("p.ambos", "p.alguno", "p.conyuge.sob")] <= 1)))
stopifnot(all(verif.prob$resultado))

# ---- 5. Rentas temporales unitarias (pago 1 al cierre de cada año) ------------
# a = sum_{t=1}^{5} v^t * P(estatus al cierre del año t)
renta <- prob %>%
  group_by(sexo, edad.activo = edad0, edad.conyuge, N0) %>%
  summarise(a.ambos    = sum(vt * p.ambos),
            a.alguno   = sum(vt * p.alguno),
            a.conyuge  = sum(vt * p.conyuge.sob), .groups = "drop")

# ---- 6. VP agregados: cantidades iniciales de activos con pareja admisible ----
# (sin aplicar la proyección con renuncias; son tres coberturas alternativas)
vp.agregado <- renta %>%
  summarise(activos.con.pareja.admisible = sum(N0),
            VP.ambos.vivos = sum(N0 * a.ambos),
            VP.al.menos.uno = sum(N0 * a.alguno),
            VP.conyuge.sobreviviente = sum(N0 * a.conyuge)) %>%
  pivot_longer(starts_with("VP"), names_to = "cobertura", values_to = "VP.agregado") %>%
  mutate(VP.por.activo.con.pareja = VP.agregado / activos.con.pareja.admisible)
vp.agregado

vp.por.sexo <- renta %>%
  group_by(sexo) %>%
  summarise(activos = sum(N0),
            VP.ambos.vivos = sum(N0 * a.ambos),
            VP.al.menos.uno = sum(N0 * a.alguno),
            VP.conyuge.sobreviviente = sum(N0 * a.conyuge), .groups = "drop")
vp.por.sexo

# Conteos esperados agregados por año (informativo; NO suma de coberturas)
esperados.anio <- prob %>%
  group_by(anio) %>%
  summarise(ambos.vivos = sum(N0 * p.ambos),
            al.menos.uno = sum(N0 * p.alguno),
            conyuge.sobreviviente = sum(N0 * p.conyuge.sob), .groups = "drop")
esperados.anio

# ---- 7. Exportación -----------------------------------------------------------
write.csv(cuadro.b,          "cod/Inciso_B_resultados/B1_activos_con_sin_pareja.csv", row.names = FALSE)
write.csv(edades.asignadas,  "cod/Inciso_B_resultados/B2_edades_asignadas.csv", row.names = FALSE)
write.csv(prob %>% select(sexo, edad.activo = edad0, sexo.conyuge, edad.conyuge, anio,
                          edad.x, edad.y, qx, qy, px.t, py.t,
                          p.ambos, p.alguno, p.conyuge.sob),
          "cod/Inciso_B_resultados/B3_probabilidades_por_edad_sexo.csv", row.names = FALSE)
write.csv(renta,             "cod/Inciso_B_resultados/B4_rentas_unitarias.csv", row.names = FALSE)
write.csv(vp.agregado,       "cod/Inciso_B_resultados/B5_VP_agregados.csv", row.names = FALSE)
write.csv(vp.por.sexo,       "cod/Inciso_B_resultados/B6_VP_por_sexo.csv", row.names = FALSE)
write.csv(esperados.anio,    "cod/Inciso_B_resultados/B7_esperados_por_anio.csv", row.names = FALSE)
write.csv(verif.prob,        "cod/Inciso_B_resultados/B8_verificaciones.csv", row.names = FALSE)