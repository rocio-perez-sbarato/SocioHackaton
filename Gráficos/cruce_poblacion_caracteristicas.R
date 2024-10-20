
# Cargar las librerías necesarias
library(dplyr)
library(readr)

# Cargar las bases de datos
características_año <- read.csv("características_año.csv", sep = ";", fileEncoding = "ISO-8859-1")
escuelas_con_extranjeros <- read.csv("escuelas_con_extranjeros.csv", sep = ",", fileEncoding = "ISO-8859-1") # Usa comas como separador
escuelas_sin_extranjeros <- read.csv("escuelas_sin_extranjeros.csv", sep = ",", fileEncoding = "ISO-8859-1")  # Usa comas como separador

# EXTRANJEROS: Filtrar características de escuelas con extranjeros
características_con_extranjeros <- características_año %>%
  semi_join(escuelas_con_extranjeros, by = "id")

# SIN EXTRANJEROS: Filtrar características de escuelas sin extranjeros
características_sin_extranjeros <- características_año %>%
  semi_join(escuelas_sin_extranjeros, by = "id")

# Mostrar primeros resultados
head(características_con_extranjeros, 10)
head(características_sin_extranjeros, 10)

# Guardar las tablas filtradas en archivos CSV (al final del código)
write.csv(características_con_extranjeros, "características_con_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(características_sin_extranjeros, "características_sin_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
