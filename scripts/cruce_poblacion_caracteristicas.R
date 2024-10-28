# Cargar las librerías necesarias
library(dplyr)
library(readr)

# ---------------------------------------------------------
# Nota: Actualizar Caracteristicas.csv y fileEncoding de acuerdo a la base de datos
# ---------------------------------------------------------

# Cargar las bases de datos
caracteristicas <- read.csv("Caracteristicas.csv", sep = ";", fileEncoding = "ISO-8859-1")
escuelas_con_extranjeros <- read.csv("escuelas_con_extranjeros.csv", sep = ",", fileEncoding = "ISO-8859-1") # Usa comas como separador
escuelas_sin_extranjeros <- read.csv("escuelas_sin_extranjeros.csv", sep = ",", fileEncoding = "ISO-8859-1")  # Usa comas como separador

# EXTRANJEROS: Filtrar caracteristicas de escuelas con extranjeros
caracteristicas_con_extranjeros <- caracteristicas %>%
  semi_join(escuelas_con_extranjeros, by = "id")

# SIN EXTRANJEROS: Filtrar caracteristicas de escuelas sin extranjeros
caracteristicas_sin_extranjeros <- caracteristicas %>%
  semi_join(escuelas_sin_extranjeros, by = "id")

# Mostrar primeros resultados
head(caracteristicas_con_extranjeros, 10)
head(caracteristicas_sin_extranjeros, 10)

# Guardar las tablas filtradas en archivos CSV (al final del código)
write.csv(caracteristicas_con_extranjeros, "caracteristicas_con_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(caracteristicas_sin_extranjeros, "caracteristicas_sin_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
