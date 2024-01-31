##########################################################
#### Listado consolidado de víctimas de trauma ocular ####
##########################################################

#### Preámbulo  ####

# Directorio de trabajo
setwd("~/GitHub/listado_victimas/")

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
    pvi = max(pvi),
    pacto = max(pacto),
    indh = max(indh),
    ) %>%
  ungroup()

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

estimacion_1 <- capHistSum(to_consolidado[,2:4])
estimacion_1$sum

mrClosed(estimacion_1, "Schnabel")

summary(mrClosed(estimacion_1, "Schnabel"))
confint(mrClosed(estimacion_1))

tabla_1 <- data.frame(summary(mrClosed(estimacion_1)),
           confint(mrClosed(estimacion_1)))

tabla_1[-4,] %>%
  kableExtra::kbl()


nula_observacion <- to_consolidado %>%
  summarise(INDH = 0, PVI = 0, PACTO = 0, Freq = NA)
primera_observacion <- to_consolidado %>%
  summarise(INDH = 1, PVI = 0, PACTO = 0, Freq = sum(indh[pvi == 0 & pacto == 0]))
segunda_observacion <- to_consolidado %>%
  summarise(INDH = 0, PVI = 1, PACTO = 0, Freq = sum(pvi[indh == 0 & pacto == 0]))
tercera_observacion <- to_consolidado %>%
  summarise(INDH = 1, PVI = 1, PACTO = 0, Freq = sum(pvi[indh == 1 & pacto == 0]))
cuarta_observacion <- to_consolidado %>%
  summarise(INDH = 0, PVI = 0, PACTO = 1, Freq = sum(pacto[indh == 0 & pvi == 0]))
quinta_observacion <- to_consolidado %>%
  summarise(INDH = 1, PVI = 0, PACTO = 1, Freq = sum(pacto[indh == 1 & pvi == 0]))
sexta_observacion <- to_consolidado %>%
  summarise(INDH = 0, PVI = 1, PACTO = 1, Freq = sum(pacto[indh == 0 & pvi == 1]))
septima_observacion <- to_consolidado %>%
  summarise(INDH = 1, PVI = 1, PACTO = 1, Freq = sum(pacto[indh == 1 & pvi == 1]))

historial_capturas <- rbind(primera_observacion,
      segunda_observacion,
      tercera_observacion,
      cuarta_observacion,
      quinta_observacion,
      sexta_observacion,
      septima_observacion)

library(Rcapture)
result <- closedpMS.t(historial_capturas, dfreq=TRUE)
print(result)


library(SparseMSE)
estimatepopulation(historial_capturas, nboot = 1000, pthresh = 0.02,
                   alpha = c(0.01, 0.05, 0.1))
checkident(historial_capturas, mX = NULL, verbose = T)

descriptive(to_consolidado[,-1], dfreq = T, dtype=c("hist","nbcap"))

modelos <- Rcapture::closedp(
  to_consolidado[,-1]
)

modelos <- as.data.frame(modelos$results)

historial_capturas

closedp.bc(to_consolidado[,-1], m=c("M0","Mbh"))
