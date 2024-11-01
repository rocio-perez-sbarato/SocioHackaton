##############################################################################
# Código de análisis de extranjeros por provincia                            #
# Nota: Ejecutar `filtrado_escuelas_extranjeros.R` antes de este             #
#       script para generar las tablas necesarias:                           #
#       `escuelas_con_extranjeros`.                                          #
##############################################################################

# Cargar las librerías necesarias
library(dplyr)
library(readr)

# Crear un nuevo dataframe solo con las columnas de electricidad
extranjeros_por_provincia <- escuelas_con_extranjeros %>%
  # Calcular las cantidades por provincia
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_con_extranjeros = n()
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_por_provincia = round((cantidad_escuelas_con_extranjeros / nrow(escuelas_con_extranjeros)) * 100, 2)
  )

# Mostrar los resultados
head(extranjeros_por_provincia)

# Guardar la tabla en archivos .csv
write.csv(extranjeros_por_provincia, "extranjeros_por_provincia.csv", row.names = FALSE, fileEncoding = "UTF-8")