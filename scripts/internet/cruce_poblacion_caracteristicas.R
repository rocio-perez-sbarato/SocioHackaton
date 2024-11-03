##############################################################################
# Código de cruce de bases de datos                                          #
# Nota: Ejecutar `filtrado_escuelas_extranjeros.R` antes de este             #
#       script para generar las tablas necesarias:                           #
#       `poblacion`, `escuelas_con_extranjeros`, `escuelas_sin_extranjeros`. #
##############################################################################

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

# Solo por si hace falta
caracteristicas$provincia <- iconv(caracteristicas$provincia, from = "latin1", to = "UTF-8")
caracteristicas$provincia <- gsub("Ã³", "ó", caracteristicas$provincia)
caracteristicas$provincia <- gsub("Ã­", "í", caracteristicas$provincia)
caracteristicas$provincia <- gsub("Ã©", "é", caracteristicas$provincia)
caracteristicas$provincia <- gsub("Ãº", "ú", caracteristicas$provincia)
caracteristicas$provincia <- gsub("Ã±", "ñ", caracteristicas$provincia)
caracteristicas$provincia <- gsub("Ã¡", "á", caracteristicas$provincia)

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
