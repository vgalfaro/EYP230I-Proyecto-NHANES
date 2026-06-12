library(foreign)
library(dplyr)
library(ggplot2)
library(lmtest)

##############################
# REGRESIÓN EN AMBOS MODELOS #
##############################
{
  # Se carga la matriz de covariables
  covariables <- read.csv("covariables.csv", sep=",")
  covariables_H <- covariables %>% 
    filter(Sexo == 0)
  covariables_M <- covariables %>% 
    filter(Sexo == 1)
  
  # Realizamos las regresiones
  modelo_completo <- lm(Col ~ Sexo + IMC + Dia + Rpres + Edad + Edad2, data = covariables)
  modelo_reducido <- lm(Col ~ Sexo + IMC + Dia + Rpres + Edad, data = covariables)
  
  modelo_completo_H <- lm(Col ~ IMC + Dia + Rpres + Edad + Edad2, data = covariables_H)
  modelo_reducido_H <- lm(Col ~ IMC + Dia + Rpres + Edad, data = covariables_H)
  
  modelo_completo_M <- lm(Col ~ IMC + Dia + Rpres + Edad + Edad2, data = covariables_M)
  modelo_reducido_M <- lm(Col ~ IMC + Dia + Rpres + Edad, data = covariables_M)
  
  
  # Revisamos los resultado
  summary(modelo_completo)
  summary(modelo_reducido)
  
  summary(modelo_completo_H)
  summary(modelo_reducido_H)
  
  summary(modelo_completo_M)
  summary(modelo_reducido_M)
}

################################
# F-TEST PARA MODELOS ANIDADOS #
################################
{
  # Obtenemos el SCE para cada modelo
  SCE_completo <- sum(residuals(modelo_completo)^2)
  SCE_reducido <- sum(residuals(modelo_reducido)^2)
  
  SCE_completo_H <- sum(residuals(modelo_completo_H)^2)
  SCE_reducido_H <- sum(residuals(modelo_reducido_H)^2)
  
  SCE_completo_M <- sum(residuals(modelo_completo_M)^2)
  SCE_reducido_M <- sum(residuals(modelo_reducido_M)^2)
  
  # Tamaño de cada modelo
  p_completo <- length(coef(modelo_completo))
  p_reducido <- length(coef(modelo_reducido))
  
  n_datos <- dim(covariables)[1]
  n_datos_H <- dim(covariables_H)[1]
  n_datos_M <- dim(covariables_M)[1]
  
  # Estadístico F y p-value
  estadistico_F <- ((SCE_reducido - SCE_completo)*(n_datos - p_completo))/(SCE_completo*(p_completo - p_reducido))
  p_value <- 1-pf(estadistico_F,
                    p_completo-p_reducido, n_datos - p_completo)
  
  estadistico_F_H <- ((SCE_reducido_H - SCE_completo_H)*(n_datos - p_completo))/(SCE_completo_H*(p_completo - p_reducido))
  p_value_H <- 1-pf(estadistico_F_H,
                  p_completo-p_reducido, n_datos_H - p_completo)
  
  estadistico_F_M <- ((SCE_reducido_M - SCE_completo_M)*(n_datos - p_completo))/(SCE_completo_M*(p_completo - p_reducido))
  p_value_M <- 1-pf(estadistico_F_M,
                    p_completo-p_reducido, n_datos_M - p_completo)
}

######################
# MEDICIÓN VARIACIÓN #
######################
{
  # Médimos la variación de cada observación normalizado por la variación estandar
  pred_completo <- predict(modelo_completo)
  pred_reducido <- predict(modelo_reducido)
  SPD <- sqrt(mean((pred_completo-pred_reducido)^2)) / sd(covariables$Col)
  
  pred_completo_H <- predict(modelo_completo_H)
  pred_reducido_H <- predict(modelo_reducido_H)
  SPD_H <- sqrt(mean((pred_completo_H-pred_reducido_H)^2)) / sd(covariables_H$Col)
  
  pred_completo_M <- predict(modelo_completo_M)
  pred_reducido_M <- predict(modelo_reducido_M)
  SPD_M <- sqrt(mean((pred_completo_M-pred_reducido_M)^2)) / sd(covariables_M$Col)
}

#################################
# NORMALIDAD Y HOMOCEDASTICIDAD #
#################################
{
  # Test Breush-Pagan
  bp_value_completo <- bptest(modelo_completo_H)$p.value
  bp_value_completo <- bptest(modelo_reducido_H)$p.value
  
  bp_value_completo_H <- bptest(modelo_completo_H)$p.value
  bp_value_completo_H <- bptest(modelo_reducido_H)$p.value
  
  bp_value_completo_M <- bptest(modelo_completo_M)$p.value
  bp_value_completo_M <- bptest(modelo_reducido_M)$p.value
}