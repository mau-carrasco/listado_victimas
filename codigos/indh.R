#############################################
#### Procesamiento de los datos del INDH ####
#############################################

#### Preámbulo  ####

# Directorio de trabajo
setwd("C:/Users/mauricio.carrasco/Desktop/estallido_social/")

# carga de paquetes
library(tidyverse)
library(flextable)
library(readxl)

# Carga de datos
indh <- read_excel("datos/BBDD Acciones judiciales y víctimas.xlsx")


#### Construcción de listados de interés ####

# Listado de víctimas de homicidio y homicidio frustrado
homicidio_indh <- indh %>%
  filter(
    `Figura juridica invocada` %in% c(
      "Homicidio",
      "Homicidio frustrado",
      "Apremios ilegítimos con homicidio",
      "Violencia innecesaria con resultado de muerte")
    ) %>%
  select(Region,
         Nombre,
         Rut = RUT,
         Sexo, 
         Edad, 
         Delito = `Figura juridica invocada`, 
         Consecuencia) %>%
  filter(!grepl("ocular", Consecuencia))

# Listado de víctimas de trauma ocular
to_indh <- unique(indh %>%
         filter(
           Consecuencia %in% c(
             "Lesión causada por trauma ocular",
             "Estallido de globo ocular",
             "Pérdida de visión por trauma ocular irreversible")
         ) %>%
         select(Rut = RUT) %>%
         drop_na()) %>%
  separate(Rut, into = c("Rut", "DV"), sep = "-") %>%
  select(!DV)


#### Guardado de bases de datos ####

# Excel con listado de víctimas de homicidio
openxlsx::write.xlsx(homicidio_indh, "resultados/homicidio_indh.xlsx")

# Excel con listado de víctimas de trauma ocular
openxlsx::write.xlsx(to_indh, "resultados/to_indh.xlsx")

