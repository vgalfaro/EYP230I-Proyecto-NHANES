
library(foreign)
library(dplyr)
library(ggplot2)

# 1. CARGA Y UNIÓN DE DATOS 

demo    <- read.xport("DEMO_L.xpt")
body    <- read.xport("BMX_L.xpt")
chol    <- read.xport("TCHOL_L.xpt")

# Unión secuencial indexada por "SEQN"
datos_crudos <- demo %>%
  inner_join(chol, by = "SEQN") %>%
  inner_join(body, by = "SEQN")


# 2. LIMPIEZA INICIAL 


# restringimos  edad a RIDAGEYR >= 20 años 
# - Seleccionamos únicamente las variables del modelo conceptual.

datos_adultos <- datos_crudos %>%
  filter(RIDAGEYR >= 20) %>%
  select(SEQN, 
         Colesterol = LBXTC, 
         Cintura = BMXWAIST, 
         Edad = RIDAGEYR, 
         Sexo = RIAGENDR, 
         NSE_PIR = INDFMPIR)

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
  mutate(
    Sexo = factor(Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    NSE_Grupo = case_when(
      NSE_PIR < 1.3  ~ "Bajo",
      NSE_PIR <= 3.5 ~ "Medio",
      NSE_PIR > 3.5  ~ "Alto"
    ),
    NSE_Grupo = factor(NSE_Grupo, levels = c("Bajo", "Medio", "Alto"))
  )

print("Datos finales listos para el EDA:")
summary(datos_limpios)

# 4. EDA

#  Distribución Univariada de colesterol y circunferencia de cintura
p1 <- ggplot(datos_limpios, aes(x = Colesterol)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() + labs(title = "Distribución del Colesterol Total ($mg/dL$)")
ggsave("eda_dist_colesterol.png", plot = p1)

p2 <- ggplot(datos_limpios, aes(x = Cintura)) + 
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  theme_minimal() + labs(title = "Distribución de la Circunferencia de Cintura ($cm$)")
ggsave("eda_dist_cintura.png", plot = p2)



#
p3 <- ggplot(datos_limpios, aes(x = Cintura, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: Cintura vs Colesterol Total",
       x = "Circunferencia de Cintura ($cm$)", y = "Colesterol Total ($mg/dL$)")
ggsave("eda_bivariado_crudo.png", plot = p3)

#Relación Cintura-Colesterol separada por Sexo
p4 <- ggplot(datos_limpios, aes(x = Cintura, y = Colesterol, color = Sexo)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relación Cintura vs Colesterol Estratificada por Sexo",
       x = "Circunferencia de Cintura ($cm$)", y = "Colesterol Total ($mg/dL$)")
ggsave("eda_estratificado_sexo.png", plot = p4)


# Relación Cintura-Colesterol separada por Nivel Socioeconómico (NSE)
p5 <- ggplot(datos_limpios, aes(x = Cintura, y = Colesterol)) +
  geom_point(alpha = 0.2, aes(color = NSE_Grupo)) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~NSE_Grupo) +
  theme_bw() +
  labs(title = "Relación Cintura vs Colesterol por Nivel Socioeconómico",
       x = "Circunferencia de Cintura ($cm$)", y = "Colesterol Total ($mg/dL$)")
ggsave("eda_estratificado_nse.png", plot = p5)






















