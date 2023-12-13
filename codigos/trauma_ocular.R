##########################################################
#### Listado consolidado de víctimas de trauma ocular ####
##########################################################

#### Preámbulo  ####

# Directorio de trabajo
setwd("C:/Users/mauricio.carrasco/Desktop/estallido_social/")

# carga de paquetes
library(tidyverse)
library(flextable)
library(readxl)
library(ggvenn)

# carga de datos
to_indh <- read_excel("resultados/to_indh.xlsx")
to_pvi <- read_excel("resultados/to_pvi.xlsx")


#### Consolidado ####

# Listado de ruts de víctimas de trauma ocular registradas en querellas del INDH y atenciones del PVI
to_consolidado <- to_indh %>%
  select(Rut) %>%
  full_join(to_pvi %>% select(Rut))

# Diagrama de Venn con el registro de casos de trauma ocular
ggvenn(
  list(`INDH` = unique(to_indh$Rut),
       `PVI` = unique(to_pvi$Rut)),
  set_name_size = 4
) +
  labs(title = "Diagrama de Venn del registro de víctimas de trauma ocular",
       subtitle = " Por institución de Derechos Humanos",
       caption = "Fuente: Elaboración propia en base a datos del INDH y el PVI") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggsave("resultados/diagrama_to.png")
