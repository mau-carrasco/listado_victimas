########################################################################
#### Consulta de datos para la subsecretaría de Redes Asistenciales ####
########################################################################

# Carga de paquetes
library(tidyverse)
library(readxl)

# Carga de datos
revision <- read_excel("datos/revisión_ssddhh.xlsx")
indh <- read_excel("datos/BBDD Acciones judiciales y víctimas.xlsx")
pvi <- read_excel("datos/CATASTRO PARA MUESTREO V. 13.12.23.xlsx", sheet = 3)

# Consulta en base del INDH
consulta_indh <- indh %>%
  filter(RUT %in% c(revision$Rut)) %>%
  select(Rut = RUT, 
         Nombre,
         Sexo,
         Ruc_querella = `RUC QUERELLA`,
         Consecuencia
         )

# Consulta en base del PVI
consulta_pvi <- pvi %>%
  filter(`TIPO DE DAÑO 1` == "TRAUMATISMO OCULAR") %>%
  select(Rut = `RUN/ N PASAPORTE`,
         Nombre = `NOMBRE COMPLETO`,
         Sexo = `IDENTIDAD DE GÉNERO`,
         Ruc_querella = `RUC 1`,
         Daño = `TIPO DE DAÑO 1`
         )

# Guardado del resultado de las consultas en Excel
openxlsx::write.xlsx(consulta_indh, "resultados/consulta_indh.xlsx")
openxlsx::write.xlsx(consulta_pvi, "resultados/consulta_pvi.xlsx")
