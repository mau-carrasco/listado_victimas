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
library(FSA)

# carga de datos
to_indh <- read_excel("resultados/to_indh.xlsx")
to_pvi <- read_excel("resultados/to_pvi.xlsx")
to_pacto <- read_excel("resultados/to_pacto.xlsx")

#### Consolidado ####

# Listado de ruts de víctimas de trauma ocular registradas en querellas del INDH, atenciones del PVI y atenciones de PACTO
to_consolidado <- to_indh %>%
  select(Rut) %>%
  full_join(to_pvi %>% select(Rut)) %>%
  full_join(to_pacto %>% select(Rut))

to_pvi$pvi <- 1
to_indh$indh <- 1
to_pacto$pacto <- 1

to_consolidado <- to_consolidado %>%
  left_join(to_pacto) %>%
  left_join(to_pvi) %>%
  left_join(to_indh) %>%
  replace(is.na(.), 0)

to_consolidado <- to_consolidado[-110,]

to_consolidado <- to_consolidado %>%
  group_by(Rut) %>%
  summarise(
    pacto = max(pacto),
    indh = max(indh),
    pvi = max(pvi)
    ) %>%
  ungroup()

to_consolidado$Rut <- as.numeric(to_consolidado$Rut)
to_consolidado <- as.data.frame(to_consolidado)

#### Análisis descriptivo de los listados ###

# Diagrama de Venn con el registro de casos de trauma ocular
ggvenn(
  list(`INDH` = unique(to_indh$Rut),
       `PVI` = unique(to_pvi$Rut),
       `PACTO` = unique(to_pacto$Rut)),
  set_name_size = 4
) +
  labs(title = "Diagrama de Venn del registro de víctimas de trauma ocular",
       subtitle = " Por institución de Derechos Humanos",
       caption = "Fuente: Elaboración propia en base a datos del INDH, el PVI y el PACTO") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggsave("resultados/diagrama_to.png", width = 6, height = 6)

#### Estimación poblacional ####

estimacion_1 <- capHistSum(to_consolidado[,-1])
estimacion_1$sum

summary(mrClosed(estimacion_1))
confint(mrClosed(estimacion_1))

tabla_1 <- data.frame(summary(mrClosed(estimacion_1)),
           confint(mrClosed(estimacion_1)))

tabla_1[-4,] %>%
  kableExtra::kbl()
