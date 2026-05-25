
library(foreign)
library(dplyr)


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




































