library(foreign)
library(dplyr)
library(ggplot2)

dataset <- read.csv("covariables.csv", sep=",")

modelo_completo <- lm(Col ~ Edad + Sexo + IMC + Dia + Edad2 + Rpres, data = dataset)

summary(modelo_completo)

modelo_reducido <- lm(Col ~ Edad + Sexo + IMC + Dia + Rpres, data = dataset)

summary(modelo_reducido)
