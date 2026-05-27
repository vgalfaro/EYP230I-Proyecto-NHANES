
library(foreign)
library(dplyr)
library(ggplot2)

# 1. CARGA Y UNIÓN DE DATOS 

demo    <- read.xport("DEMO_L.xpt")
body    <- read.xport("BMX_L.xpt")
chol    <- read.xport("TCHOL_L.xpt")
pres    <- read.xport("BPXO_L.xpt")
# Unión secuencial indexada por "SEQN"



# 2. LIMPIEZA INICIAL 

# calcular promedio de mediciones de presiones diarias.

pres$promedio_sistolica <- rowMeans(pres[, c("BPXOSY1", "BPXOSY2", "BPXOSY3")])

pres$promedio_diastolica<- rowMeans(pres[,c("BPXODI1","BPXODI2","BPXODI3")])

# Unión secuencial indexada por "SEQN"
datos_crudos <- demo %>%
  inner_join(chol, by = "SEQN") %>%
  inner_join(body, by = "SEQN") %>%
  inner_join(pres, by = "SEQN")

# restringimos  edad a RIDAGEYR >= 18 años 

# - Seleccionamos únicamente las variables del modelo conceptual.

datos_adultos <- datos_crudos %>%
  filter(RIDAGEYR >= 18) %>%
  select(SEQN, 
         Colesterol = LBXTC, 
         Edad = RIDAGEYR, 
         Sexo = RIAGENDR, 
         NSE_PIR = INDFMPIR,
         IMC = BMXBMI,
         Presion_sis = promedio_sistolica,
         Presion_dia = promedio_diastolica)

print("Dimensiones tras filtro de adultos:")
dim(datos_adultos)


# 3. REVISIÓN DE FALTANTES  Y CODIFICACIÓN

# A. Diagnóstico de NAs por variable
nas_resumen <- data.frame(
  Total_NA = colSums(is.na(datos_adultos)),
  Porcentaje_NA = round(colMeans(is.na(datos_adultos)) * 100, 2)
)
print("Resumen de datos faltantes:")
print(nas_resumen)

datos_limpios <- datos_adultos %>% na.omit()


# B. Codificación de Variables
# - El Sexo viene codificado como 1 y 2. Debemos transformarlo a Factor para que R no lo lea como continuo.
# - Para el Nivel Socioeconómico (NSE_PIR), creamos una variable categórica.
datos_limpios <- datos_limpios %>%
  mutate(Sexo = Sexo - 1)

datos_limpios <- datos_limpios %>%
  mutate(
    Sexo = factor(Sexo, levels = c(0, 1), labels = c("Hombre", "Mujer")),
    
  )

print("Datos finales listos para el EDA:")
summary(datos_limpios)

# 4. EDA

#  Distribución Univariada de colesterol y circunferencia de cintura

p1 <- ggplot(datos_limpios, aes(x = Colesterol)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(x = "Colesterol (mg/dL)",
       y = "Frecuencia")
  )

ggplot(datos_limpios, aes(x = Edad, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", aes(color = "Promedio"), se = TRUE) +
  scale_color_manual(values = c("Promedio" = "red")) +
  theme_minimal() +
  labs(x = "Edad", y = "Colesterol (mg/dL)",
       color = "Leyenda") +
  theme(
    legend.position = c(0.80, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Calculamos la matriz de covarianza

matriz_covariables <- data.frame(
  datos_limpios$Sexo,
  datos_limpios$IMC,
  datos_limpios$NSE_PIR,
  datos_limpios$Presion_sis,
  datos_limpios$Presion_dia,
  datos_limpios$Edad,
  (datos_limpios$Edad)^2
)

cor(matriz_covariables)

ggplot(datos_limpios, aes(x = Edad, y = Colesterol)) +
  geom_point(
    aes(color = factor(Sexo)),
    alpha = 0.12,
    size = 1.6,
    show.legend = FALSE
  ) +
  geom_smooth(
    aes(color = factor(Sexo), fill = factor(Sexo)),
    method = "loess",
    se = TRUE,
    linewidth = 1.4,
    alpha = 0.12,
    key_glyph = "path"
  ) +
  scale_color_manual(
    name = "Sexo",
    values = c("0" = "orange", "1" = "blue"),
    labels = c("0" = "Hombre", "1" = "Mujer")
  ) +
  scale_fill_manual(
    values = c("0" = "orange", "1" = "blue"),
    guide = "none"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        alpha = 1,
        linewidth = 1.6
      )
    )
  ) +
  theme_minimal() +
  labs(x = "Edad",
    y = "Colesterol (mg/dL)"
  ) +
  theme(
    legend.position = c(0.80, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

