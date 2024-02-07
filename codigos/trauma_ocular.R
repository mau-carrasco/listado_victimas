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
    pacto = max(pacto),
    indh = max(indh),
    pvi = max(pvi)
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

library(Rcapture)
descriptivos <- descriptive(to_consolidado[,-1], dfreq = F, dtype = "hist")
plot(descriptivos)
indicadores <- capHistSum(to_consolidado[,-1])$sum %>%
  select(1:2,4)
m1 <- mrClosed(n = indicadores$n, m = indicadores$m, M = indicadores$M,
         method="Chapman")

m2 <- mrClosed(n = indicadores$n, m = indicadores$m, M = indicadores$M,
               method="Schnabel")

cbind(summary(m2,incl.SE=TRUE),confint(m2))

chapman_comparacion <- cbind(summary(m1,incl.SE=TRUE),confint(m1))
chapman_comparacion <- chapman_comparacion %>%
  data.frame()
chapman_comparacion$Comparacion <- c("PACTO", "PACTO + INDH", "PACTO + INDH + PVI", NA)
chapman_comparacion <- chapman_comparacion %>% select(5, 1:4)
rempsyc::nice_table(chapman_comparacion %>% drop_na(),
                    title = "Tabla 1. Estimación población de víctima de violaciones de derechos humanos con resultado de trauma ocular por método de comparación múltiple de Chapman",
                    note = c("",
                             "Los límites superiores e inferiores del intervalo de cada estimación fueron calculados para un 95% de confianza."))

modelos <- closedp.t(to_consolidado[,-1], dfreq = F, neg = T)

mat <- histpos.t(3)
mX1 <- mat
mc1 <- closedp.mX(to_consolidado[,-1], dfreq = F, mX = mX1, mname = "Sin interación")
summary(mc1$glm)

mX2 <- cbind(mat,mat[, 1] * mat[, 2])
mc2 <- closedp.mX(to_consolidado[,-1], dfreq = F, mX = mX2, mname = "Mt interacción PACTO * INDH")
summary(mc2$glm)

resultados <- rbind(mc1$results, mc2$results)
resultados <- resultados %>% as.tibble() %>%
  mutate(IC_inf = abundance - 1.96 * stderr,
         IC_sup = abundance + 1.96 * stderr,
         Modelo = c(1, 2),
         Modelo = factor(Modelo, labels = c("Sin interación", "Interacción PACTO * INDH"))) %>%
  select(Modelo, "Estimación" = abundance, SE = stderr, "IC lim. inf." = IC_inf, "IC. lim. sup" = IC_sup)
resultados %>%
  rempsyc::nice_table(
    title = "Tabla 1: Estimación poblacional de las víctimas de violaciones de derechos humanos con resultado de trauma ocular a partir de modelos loglineáles",
    note = c("", 
             "El tamaño poblacional fue estimado a través una regresión de Poisson que considera la presencia y ausencia de víctimas en los tres listados: PVI, PACTO e INDH.",
             "El modelo de interacción controla la dependencia de los listados PACTO e INDH, ya que en ambos registros existen víctimas catastradas en el mismo espacio-tiempo.",
             "Los límites superiores e inferiores del intervalo de cada estimación fueron calculados para un 95% de confianza.")
  )

anova(modelos$glm$M0,
      mc1$glm,
      mc2$glm, test = "Chisq") %>%
  broom::tidy() %>%
  mutate(term = c("Modelo nulo", "Modelo simple", "Modelo interacción")) %>%
  rempsyc::nice_table(title = "Significancia de los modelos poblacionales por Análisis de Varianza (ANOVA)",
                      note = c("", "El contraste de varianza entre los modelos se hizo a través de la prueba Chi-cuadrado de una cola"))

modelsummary::modelsummary(
  list("Modelo simple" = mc1$glm,
       "Modelo interacción" = mc2$glm),
  stars = T, output = "flextable", title = "Coeficientes (en log odds) de los modelos poblacionales de víctimas de violaciones de derechos humanos con resultado de trauma ocular") %>%
  flextable::theme_apa()

ggplot(resultados, aes(x = Modelo, y = Estimación)) +
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = `IC lim. inf.`, ymax = `IC. lim. sup`), width = 0.2, color = "darkblue") +
  labs(x = "Modelo", y = "Población estimada", caption = "Nota: Intervalo estimado en un 95% de confianza") +
  theme_minimal()


modelos$parameters
