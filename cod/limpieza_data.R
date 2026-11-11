library(readxl)
library(dplyr)
library(magrittr)
library(lubridate)

activos <- read_excel("data/Población CA-0309.xlsx", 
                                sheet = "Activos", 
                                col_types = c("text", "date", "date", "date", "numeric", "text", "numeric"))



#Guradamos la data sin columnas cambiando tipos fechas y recalculando la edad respecto
#fecha de referencia:
activos.dep <- activos  %>% 
  mutate(`Fecha Nacimiento` = as_date(`Fecha Nacimiento`),
         `Fecha Ingreso` = as_date(`Fecha Ingreso`),
         `Fecha Defuncion` = as_date(`Fecha Defuncion`),
         Edad = floor(time_length(interval(`Fecha Nacimiento`, ymd("2025-12-31")), "years"))
  )

#Revisión rápida de contenidos:
str(activos.dep)


#Na's por columna:

activos.dep %>% 
  summarise(across(everything(), \(x) sum(is.na(x))))

# Verificamos si hay duplicados:

sum(duplicated(activos.dep$`Id Asociado`))


# Registros sin nacimiento válido                
fechas.nac.invalidas <- activos.dep %>% 
  filter(is.na(`Fecha Nacimiento`)) 

activos.dep %>% count(Genero)

# Sin sexo utilizable
sexo.invalido <-  activos.dep %>% filter(!Genero %in% c("Masculino", "Femenino"))

# Con edad fuera de 0 a 106
edad.fuera.rango <- activos.dep %>% filter(Edad < 0 | Edad > 106)

# Defunción informada entre activos
defuncion.activos <- activos.dep %>% filter(!is.na(`Fecha Defuncion`))


#Se guardan los ids con formato inválido:
ids.invalidos <- c(fechas.nac.invalidas$`Id Asociado`, 
                   sexo.invalido$`Id Asociado`, 
                   edad.fuera.rango$`Id Asociado`, 
                   defuncion.activos$`Id Asociado`)

length(unique(ids.invalidos)) #Se eliminan 138 id's
length(activos$`Id Asociado`)

#Se eliminan de la data los ids inválidos:
activos.dep <- activos.dep %>% filter(!`Id Asociado` %in% ids.invalidos)

#Verificamos si existen coincidencias de Ids en hoja fallecidos:

fallecidos <- read_excel("data/Población CA-0309.xlsx", 
                                sheet = "Fallecidos", 
                                col_types = c("text", "date", "date", "date", 
                                              "date", "text", "numeric"))

fallecidos %>% 
  filter(`Id Asociado` %in% activos.dep$`Id Asociado`) %>%
  nrow() #No hay coincidencias


#Verificamos si existen coincidencias de Ids en hoja fallecidos:
inactivos <- read_excel("data/Población CA-0309.xlsx", 
                         sheet = "Inactivos", 
                         col_types = c("text", "date", "date", "date", 
                                       "date", "text", "numeric"))

inactivos %>% 
  filter(`Id Asociado` %in% activos.dep$`Id Asociado`) %>%
  nrow() #No hay coincidencias

