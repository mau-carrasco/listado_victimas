#############################################
#### Procesamiento de los datos de PACTO ####
#############################################

#### Preámbulo  ####

# Directorio de trabajo
setwd("~/GitHub/listado_victimas/")

# carga de paquetes
library(tidyverse)
library(flextable)
library(readxl)

# Carga de datos
pacto <- read_excel("datos/F REGISTRO PACTO AL 31.12.2023 pend dg LG y LP.xlsx")

#### Listado de víctimas de trauma ocular ####
to_pacto <- pacto %>%
  filter(`ESTADO 2023` %in% c("Activo", "Inactivo", "Derivado a Concepción")) %>%
  select(Rut = RUT) %>%
  drop_na() %>%
  unique() %>%
  separate(Rut, into = c("Rut", "DV"), sep = "-") %>%
  select(!DV)

#### Guardado de bases de datos ####

# Excel con listado de víctimas de trauma ocular
openxlsx::write.xlsx(to_pacto, "resultados/to_pacto.xlsx")
