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

