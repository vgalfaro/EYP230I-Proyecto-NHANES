library(foreign)

demo   <- read.xport("DEMO_L.xpt")
body   <- read.xport("BMX_L.xpt")
presion <- read.xport("BPXO_L.xpt")
chol   <- read.xport("TCHOL_L.xpt")

# 2. Unir todo secuencialmente usando "SEQN"
paso1 <- merge(demo, chol, by = "SEQN")
paso2 <- merge(paso1, body, by = "SEQN")
datos_finales <- merge(paso2, presion, by = "SEQN")

# 3. Revisar las dimensiones (cuántos pacientes y cuántas variables quedaron)
dim(datos_finales)

# 4. Vemos la  cantidad total de valores faltantes en todo el dataset
total_nas <- sum(is.na(datos_finales))
print(paste("Total de datos faltantes en el dataset:", total_nas))


# 4.1 Vemos la cantidad de datos faltantes por cada columna (variable)
nas_por_columna <- colSums(is.na(datos_finales))
print("Datos faltantes por columna:")
print(nas_por_columna[nas_por_columna > 0])

# 4.2  Porcentaje de datos faltantes por cada columna
porcentaje_nas <- colMeans(is.na(datos_finales)) * 100
print("Porcentaje de datos faltantes por columna:")
print(porcentaje_nas[porcentaje_nas > 0])

# 5. Resumen estadístico general (incluye el conteo de NA al final de cada columna)
summary(datos_finales)

# 6. Ver opciones para tratar datos faltantes

# 6.A Opción de eliminar columna con al menos un datos faltante
datos_completos <- na.omit(datos_finales)
dim(datos_completos) 

#Quedan muy pocos datos con esta


#6.B  Opción de reemplazar por la mediana 
datos_imputados <- datos_finales
if("LBXTC" %in% colnames(datos_imputados)) {
  media_chol <- mean(datos_imputados$LBXTC, na.rm = TRUE) # na.rm = TRUE ignora los NAs para calcular la media
  
  # Reemplazar los NAs de esa columna por la media calculada
  datos_imputados$LBXTC[is.na(datos_imputados$LBXTC)] <- media_chol
}