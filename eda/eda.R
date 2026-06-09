library(foreign)
library(dplyr)
library(ggplot2)

#############################
# 1. CARGA Y UNIÓN DE DATOS #
#############################
{
  demo    <- read.xport("DEMO_L.xpt")
  body    <- read.xport("BMX_L.xpt")
  chol    <- read.xport("TCHOL_L.xpt")
  pres    <- read.xport("BPXO_L.xpt")
}

#######################
# 2. LIMPIEZA INICIAL #
#######################
{
  # Calcular promedio de mediciones de presiones diarias.
  pres$promedio_sistolica <- rowMeans(pres[, c("BPXOSY1", "BPXOSY2", "BPXOSY3")])
  pres$promedio_diastolica<- rowMeans(pres[,c("BPXODI1","BPXODI2","BPXODI3")])
  
  # Unión secuencial indexada por "SEQN"
  datos_crudos <- demo %>%
    inner_join(chol, by = "SEQN") %>%
    inner_join(body, by = "SEQN") %>%
    inner_join(pres, by = "SEQN")
  
  # Seleccionamos únicamente las variables del modelo conceptual.
  datos_modelo <- datos_crudos %>%
    select(SEQN, 
           Col = LBXTC, 
           Edad = RIDAGEYR, 
           Sexo = RIAGENDR,
           IMC = BMXBMI,
           Sis = promedio_sistolica,
           Dia = promedio_diastolica)
  
  # Dimensiones tras filtro de adultos
  dim(datos_modelo)
}

############################################
# 3. REVISIÓN DE FALTANTES  Y CODIFICACIÓN #
############################################
{
  #########################
  # A. Diagnóstico de NAs #
  #########################
  {
    # Vemos la cantidad de Nas por variable
    nas_resumen <- data.frame(
      Total_NA = colSums(is.na(datos_modelo)),
      Porcentaje_NA = round(colMeans(is.na(datos_modelo)) * 100, 2)
    )
    print(nas_resumen)
    
    # Dropeamos los Nas de colesterol
    datos_limpio <- datos_modelo %>%
      filter(!is.na(Col))
    
    # Comparamos las medidas de tendencia central antes y despues de hacer drop
    print(summary(datos_modelo))
    print(summary(datos_limpio))
    
    # Rellenamos las presiones y el IMC con el promedio en cada edad
    datos <- datos_limpio %>%
      group_by(Edad) %>%
      mutate(
        across(
          c(Dia, Sis, IMC),
          ~ if_else(
            is.na(.x),
            mean(.x, na.rm = TRUE),
            .x
          )
        )
      ) %>%
      ungroup()
    
  }
  
  ################################
  # B. Codificación de Variables #
  ################################
  {
    # Codificamos Sexo en {0,1}
    datos <- datos %>%
      mutate(Sexo = Sexo - 1)
    # datos <- datos %>%
    #   mutate(
    #     Sexo = factor(Sexo, levels = c(0, 1), labels = c("Hombre", "Mujer")),
    #   )
    
    # Dropeamos la columna S
    datos <- datos %>% select(-SEQN)
  }
}

##########
# 4. EDA #
##########
{
  # Graficamos Edad vs Colesterol
  {
    ggplot(datos, aes(x = Edad, y = Col)) +
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
  }
  
  # Graficamos Edad vs Colesterol diferenciando Sexo
  {
    ggplot(datos, aes(x = Edad, y = Col)) +
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
  }
  
  # Matriz de covariables y corrleacón
  {
    # Centramos la Edad para no tener colinealidad con Edad^2
    covariables <- datos
    covariables$Edad <- covariables$Edad - mean(covariables$Edad)
    covariables$Edad2 <- (covariables$Edad)^2
    
    # Codificamos el rango entre presiones para no tener colinealidad entre Dia y Sis
    covariables$Rpres <- covariables$Sis - covariables$Dia
    
    # Revisamos las correlaciones
    cor(covariables %>% select(-Col))
    
    # Dropeamos Sis
    covariables <- covariables %>% select(-Sis)
    
  }
}
