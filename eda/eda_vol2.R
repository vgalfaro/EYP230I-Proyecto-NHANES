
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
  mutate(
    Sexo = factor(Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
  )

print("Datos finales listos para el EDA:")
summary(datos_limpios)

# 4. EDA

#  Distribución Univariada de colesterol y circunferencia de cintura

p1 <- ggplot(datos_limpios, aes(x = Colesterol)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() + labs(title = "Distribución del Colesterol Total ($mg/dL$)")
p1

p2 <- ggplot(datos_limpios, aes(x = Cintura)) + 
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  theme_minimal() + labs(title = "Distribución de la Circunferencia de Cintura ($cm$)")
p2

#
p3 <- ggplot(datos_limpios, aes(x = Cintura, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: Cintura vs Colesterol Total",
       x = "Circunferencia de Cintura ($cm$)", y = "Colesterol Total ($mg/dL$)")
p3

#Relación Cintura-Colesterol separada por Sexo
p4 <- ggplot(datos_limpios, aes(x = Cintura, y = Colesterol, color = Sexo)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relación Cintura vs Colesterol Estratificada por Sexo",
       x = "Circunferencia de Cintura ($cm$)", y = "Colesterol Total ($mg/dL$)")
p4

# Relación Cintura-Colesterol separada por Nivel Socioeconómico (NSE)
p5 <- ggplot(datos_limpios, aes(x = Cintura, y = Colesterol)) +
  geom_point(alpha = 0.2, aes(color = NSE_Grupo)) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~NSE_Grupo) +
  theme_bw() +
  labs(title = "Relación Cintura vs Colesterol por Nivel Socioeconómico",
       x = "Circunferencia de Cintura ($cm$)", y = "Colesterol Total ($mg/dL$)")
p5




ggplot(datos_limpios, aes(x = Cintura, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: IMC vs Colesterol Total",
       x = "IMC ($cm$)", y = "Colesterol Total ($mg/dL$)")




ggplot(datos_limpios, aes(x = Circ_Brazo, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: IMC vs Colesterol Total",
       x = "IMC ($cm$)", y = "Colesterol Total ($mg/dL$)")


ggplot(datos_limpios, aes(x = Peso, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: peso vs Colesterol Total",
       x = "peso", y = "Colesterol Total ($mg/dL$)")

ggplot(datos_limpios, aes(x = Edad, y = Colesterol)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: peso vs Colesterol Total",
       x = "peso", y = "Colesterol Total ($mg/dL$)")





ggplot(datos_limpios, aes(x = Cintura, y = IMC)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: Cintura vs IMC",
       x = "Cintura", y = "IMC")




ggplot(datos_limpios, aes(x = Peso, y = IMC)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: Cintura vs IMC",
       x = "Cintura", y = "IMC")


ggplot(datos_limpios, aes(x = Presion_sis, y = Presion_dia)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: Cintura vs IMC",
       x = "sis", y = "dia")



ggplot(datos_limpios, aes(x = Ritmo, y = Presion_sis)) +
  geom_point(alpha = 0.3, color = "gray30") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación Cruda: Cintura vs IMC",
       x = "sis", y = "dia")







