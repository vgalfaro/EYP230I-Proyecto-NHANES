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
  modelo_com <- lm(Col ~ Sexo + IMC + Dia + Rpres + Edad + Edad2, data = covariables)
  modelo_red <- lm(Col ~ Sexo + IMC + Dia + Rpres + Edad, data = covariables)
  
  modelo_com_H <- lm(Col ~ IMC + Dia + Rpres + Edad + Edad2, data = covariables_H)
  modelo_red_H <- lm(Col ~ IMC + Dia + Rpres + Edad, data = covariables_H)
  
  modelo_com_M <- lm(Col ~ IMC + Dia + Rpres + Edad + Edad2, data = covariables_M)
  modelo_red_M <- lm(Col ~ IMC + Dia + Rpres + Edad, data = covariables_M)
  
  
  # Revisamos los resultado
  summary(modelo_com)
  summary(modelo_red)
  
  summary(modelo_com_H)
  summary(modelo_red_H)
  
  summary(modelo_com_M)
  summary(modelo_red_M)
}

################################
# F-TEST PARA MODELOS ANIDADOS #
################################
{
  # Obtenemos el SCE para cada modelo
  SCE_com <- sum(residuals(modelo_com)^2)
  SCE_red <- sum(residuals(modelo_red)^2)
  
  SCE_com_H <- sum(residuals(modelo_com_H)^2)
  SCE_red_H <- sum(residuals(modelo_red_H)^2)
  
  SCE_com_M <- sum(residuals(modelo_com_M)^2)
  SCE_red_M <- sum(residuals(modelo_red_M)^2)
  
  # Tamaño de cada modelo
  p_com <- length(coef(modelo_com))
  p_red <- length(coef(modelo_red))
  
  n_datos <- dim(covariables)[1]
  n_datos_H <- dim(covariables_H)[1]
  n_datos_M <- dim(covariables_M)[1]
  
  # Estadístico F y p-value
  estadistico_F <- ((SCE_red - SCE_com)*(n_datos - p_com))/(SCE_com*(p_com - p_red))
  p_value <- 1-pf(estadistico_F,
                    p_com-p_red, n_datos - p_com)
  
  estadistico_F_H <- ((SCE_red_H - SCE_com_H)*(n_datos - p_com))/(SCE_com_H*(p_com - p_red))
  p_value_H <- 1-pf(estadistico_F_H,
                  p_com-p_red, n_datos_H - p_com)
  
  estadistico_F_M <- ((SCE_red_M - SCE_com_M)*(n_datos - p_com))/(SCE_com_M*(p_com - p_red))
  p_value_M <- 1-pf(estadistico_F_M,
                    p_com-p_red, n_datos_M - p_com)
}

######################
# MEDICIÓN VARIACIÓN #
######################
{
  # Médimos la variación de cada observación normalizado por la variación estandar
  pred_com <- predict(modelo_com)
  pred_red <- predict(modelo_red)
  SPD <- sqrt(mean((pred_com-pred_red)^2)) / sd(covariables$Col)
  
  pred_com_H <- predict(modelo_com_H)
  pred_red_H <- predict(modelo_red_H)
  SPD_H <- sqrt(mean((pred_com_H-pred_red_H)^2)) / sd(covariables_H$Col)
  
  pred_com_M <- predict(modelo_com_M)
  pred_red_M <- predict(modelo_red_M)
  SPD_M <- sqrt(mean((pred_com_M-pred_red_M)^2)) / sd(covariables_M$Col)
}

#################################
# NORMALIDAD Y HOMOCEDASTICIDAD #
#################################
{
  # Test Breush-Pagan
  {
    bp_value_com <- bptest(modelo_com)$p.value
    bp_value_red <- bptest(modelo_red)$p.value
    
    bp_value_com_H <- bptest(modelo_com_H)$p.value
    bp_value_red_H <- bptest(modelo_red_H)$p.value
    
    bp_value_com_M <- bptest(modelo_com_M)$p.value
    bp_value_red_M <- bptest(modelo_red_M)$p.value
  }
  
  # Test Shapiro-Wilk
  {
    # Separamos los residuos por edad de cada modelo
    res_edad_com <- split(residuals(modelo_com), covariables$Edad)
    res_edad_red <- split(residuals(modelo_red), covariables$Edad)
    
    res_edad_com_H <- split(residuals(modelo_com_H), covariables_H$Edad)
    res_edad_red_H <- split(residuals(modelo_red_H), covariables_H$Edad)
    
    res_edad_com_M <- split(residuals(modelo_com_M), covariables_M$Edad)
    res_edad_red_M <- split(residuals(modelo_red_M), covariables_M$Edad)
    
    # Contamos la cantidad de veces que no se rechaza normalidad
    norm_com <- 0
    norm_red <- 0
    norm_com_H <- 0
    norm_red_H <- 0
    norm_com_M <- 0
    norm_red_M <- 0
    
    for(i in 1:length(unique(covariables$Edad))){
      if(shapiro.test(res_edad_com[[i]])$p.value >= 0.05){ norm_com <- norm_com + 1}
      if(shapiro.test(res_edad_red[[i]])$p.value >= 0.05){ norm_red <- norm_red + 1}
      
      if(shapiro.test(res_edad_com_H[[i]])$p.value >= 0.05){ norm_com_H <- norm_com_H + 1
      if(shapiro.test(res_edad_red_H[[i]])$p.value >= 0.05){ norm_red_H <- norm_red_H + 1}
      
      if(shapiro.test(res_edad_com_M[[i]])$p.value >= 0.05){ norm_com <- norm_com_M + 1}
      if(shapiro.test(res_edad_red_M[[i]])$p.value >= 0.05){ norm_red <- norm_red_M + 1}
    }
  }
}
}

################
# COLINEALIDAD #
################
{
  # Valor vif
  cor_com <- cor(select(covariables, -Col))
  vif_com <- diag(solve(cor_com))
  cor_red <- cor(select(covariables, -Col, -Edad2))
  vif_red <- diag(solve(cor_red))
  
  cor_com_H <- cor(select(covariables_H, -Col, -Sexo))
  vif_com_H <- diag(solve(cor_com_H))
  cor_red_H <- cor(select(covariables_H, -Col, -Sexo, -Edad2))
  vif_red_H <- diag(solve(cor_red_H))
  
  cor_com_M <- cor(select(covariables_M, -Col, -Sexo))
  vif_com_M <- diag(solve(cor_com_M))
  cor_red_M <- cor(select(covariables_M, -Col, -Sexo, -Edad2))
  vif_red_M <- diag(solve(cor_red_M))
}

##################
# DATOS EXTREMOS #
##################
{
  # Calculamos las distancias de Cook
  cook_com <- cooks.distance(modelo_com)
  cook_red <- cooks.distance(modelo_red)
  
  cook_com_H <- cooks.distance(modelo_com_H)
  cook_red_H <- cooks.distance(modelo_red_H)
  
  cook_com_M <- cooks.distance(modelo_com_M)
  cook_red_M <- cooks.distance(modelo_red_M)
  
  # Vemos cuantos datos son influyentes
  influyentes_com <- which(cook_com > 1)
  influyentes_red <- which(cook_red > 1)
  
  influyentes_com_H <- which(cook_com_H > 1)
  influyentes_red_H <- which(cook_red_H > 1)
  
  influyentes_com_M <- which(cook_com_M > 1)
  influyentes_red_M <- which(cook_red_M > 1)
}


####################
# GRÁFICOS ERRORES #
####################
{
  plot(modelo_com)
  plot(modelo_red)
  
  plot(modelo_com_H)
  plot(modelo_red_H)
  
  plot(modelo_com_M)
  plot(modelo_red_M)
}

################ 
# SENSIBILIDAD #
################

# Creamos el dataset filtrando los valores de colesterol más altos
datos_sensibilidad <- subset(covariables, Col <= 300)

# Hacemos la regresión con los nuevos datos
modelo_sensibilidad <- lm(Col ~ Edad + Sexo + IMC + Dia + Edad2 + Rpres, data = datos_sensibilidad)

# Resultados
summary(modelo_sensibilidad)
