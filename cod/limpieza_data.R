

tabla.mortalidad <- read.delim("data/CA0309_Mortalidad_CR2023.csv", sep = ",")

names(tabla.mortalidad) <- c("edad", "M", "F")

activos <- read_excel("data/Población CA-0309.xlsx",
                      sheet = "Activos",
                      col_types = c("text", "date", "date", "date",
                                    "numeric", "text", "numeric"))

# Tipos de fecha y edad recalculada a la fecha de referencia
activos.dep <- activos %>%
  mutate(`Fecha Nacimiento` = as_date(`Fecha Nacimiento`),
         `Fecha Ingreso`    = as_date(`Fecha Ingreso`),
         `Fecha Defuncion`  = as_date(`Fecha Defuncion`),
         Edad = floor(time_length(interval(`Fecha Nacimiento`, fecha.ref), "years")))

str(activos.dep)
activos.dep %>% summarise(across(everything(), \(x) sum(is.na(x))))

# Duplicados
(n.duplicados <- sum(duplicated(activos.dep$`Id Asociado`)))


# Criterios de exclusión
fechas.nac.invalidas <- activos.dep %>% filter(is.na(`Fecha Nacimiento`))
activos.dep %>% count(Genero)
sexo.invalido     <- activos.dep %>% filter(!Genero %in% c("Masculino", "Femenino") | is.na(Genero))
edad.fuera.rango  <- activos.dep %>% filter(Edad < 0 | Edad > 106)
defuncion.activos <- activos.dep %>% filter(!is.na(`Fecha Defuncion`))

# ---- Cuadro sin duplicar conteos: un único motivo por registro (por prioridad)
activos.dep <- activos.dep %>%
  mutate(motivo = case_when(
    duplicated(`Id Asociado`)                              ~ "1. Id duplicado",
    is.na(`Fecha Nacimiento`)                              ~ "2. Sin nacimiento válido",
    is.na(Genero) | !Genero %in% c("Masculino", "Femenino") ~ "3. Sin sexo utilizable",
    Edad < 0 | Edad > 106                                  ~ "4. Edad fuera de 0 a 106",
    !is.na(`Fecha Defuncion`)                              ~ "5. Defunción informada",
    TRUE                                                   ~ NA_character_))

cuadro.exclusiones <- activos.dep %>%
  filter(!is.na(motivo)) %>% count(motivo, name = "registros")

# Traslape entre criterios (informativo)
traslapes <- tibble(
  criterio = c("Sin nacimiento", "Sin sexo", "Edad fuera de rango", "Defunción informada"),
  cumplen  = c(nrow(fechas.nac.invalidas), nrow(sexo.invalido),
               nrow(edad.fuera.rango), nrow(defuncion.activos)))

n.total <- nrow(activos.dep)
n.excl  <- sum(cuadro.exclusiones$registros)

cuadro.registros <- bind_rows(
  tibble(concepto = "Registros en hoja Activos", registros = n.total),
  cuadro.exclusiones %>% transmute(concepto = paste("(-)", motivo), registros),
  tibble(concepto = "Total excluidos", registros = n.excl),
  tibble(concepto = "Población inicial N0", registros = n.total - n.excl))
cuadro.registros

# Revisión de cruces con otras hojas (sin eliminar automáticamente)
fallecidos <- read_excel("data/Población CA-0309.xlsx", sheet = "Fallecidos",
                         col_types = c("text", "date", "date", "date",
                                       "date", "text", "numeric"))
inactivos  <- read_excel("data/Población CA-0309.xlsx", sheet = "Inactivos",
                         col_types = c("text", "date", "date", "date",
                                       "date", "text", "numeric"))
sum(fallecidos$`Id Asociado` %in% activos.dep$`Id Asociado`)   # 0
sum(inactivos$`Id Asociado`  %in% activos.dep$`Id Asociado`)   # 0

# Base final (una observación por persona, sin exclusiones) ----
base <- activos.dep %>% filter(is.na(motivo))

N0 <- nrow(base)