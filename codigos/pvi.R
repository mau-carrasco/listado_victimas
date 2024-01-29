############################################
#### Procesamiento de los datos del PVI ####
############################################

#### Preámbulo  ####

# Directorio de trabajo
setwd("C:/Users/mauricio.carrasco/Desktop/estallido_social/")

# carga de paquetes
library(tidyverse)
library(flextable)
library(readxl)

# Carga de datos
pvi <- read_excel("datos/CATASTRO PARA MUESTREO V. 13.12.23.xlsx", sheet = 3)


#### Listado de víctimas de trauma ocular ####
to_pvi <- pvi %>%
  select(Nombre = `NOMBRE COMPLETO`,
         Rut = `RUN/ N PASAPORTE`,
         Sexo = `IDENTIDAD DE GÉNERO`,
         contains("TIPO DE DAÑO")) %>%
  pivot_longer(
    cols = contains("TIPO DE DAÑO"),
    names_to = "Daño",
    values_to = "Consecuencia"
    ) %>%
  drop_na(Consecuencia) %>%
  filter(grepl("OCULAR", Consecuencia)) %>%
  select(-4) %>%
  separate(Rut, into = c("Rut", "DV"), sep = "-") %>%
  select(Rut) %>%
  filter(Rut != "SIN DATO") %>%
  drop_na(Rut) %>% unique()


#### Guardado de bases de datos ####

# Excel con listado de víctimas de trauma ocular
openxlsx::write.xlsx(to_pvi, "resultados/to_pvi.xlsx")
