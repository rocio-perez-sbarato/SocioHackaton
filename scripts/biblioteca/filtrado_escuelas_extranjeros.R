# Cargar las librerías necesarias
library(dplyr)
library(readr)

# ---------------------------------------------------------
# Nota: Actualizar Poblacion.csv y fileEncoding de acuerdo a la base de datos
# Actualizar columnas_extrajeros de acuerdo al año.
# ---------------------------------------------------------

# Cargar la base de datos 
poblacion <- read.csv("Poblacion.csv", sep = ";", fileEncoding = "UTF-8")

# Definir las columnas que contienen los datos de extranjeros
columnas_extranjeros <- c(
  'Bolivia...Primaria','Paraguay...Primaria', 'Perú...Primaria', 'Venezuela...Primaria',
  'Bolivia...Secundaria', 'Paraguay...Secundaria','Perú...Secundaria', 'Venezuela...Secundaria'
)

# Calcular el total de extranjeros por escuela
poblacion <- poblacion %>%
  rowwise() %>%
  mutate(total_extranjeros_por_escuela = sum(c_across(all_of(columnas_extranjeros)), na.rm = TRUE))

# Renombrar la columna ID1 a id
poblacion <- poblacion %>%
rename(id = ID1)

extranjeros_por_escuela <- poblacion %>%
  select(id,provincia,sector,ambito,total_extranjeros_por_escuela)

escuelas_con_extranjeros <- extranjeros_por_escuela %>% filter(total_extranjeros_por_escuela > 0)

# Filtrar las escuelas sin extranjeros
escuelas_sin_extranjeros <- poblacion %>%
  filter(total_extranjeros_por_escuela == 0) %>%
  select(id, provincia, sector, ambito, total_extranjeros_por_escuela)

# Mostrar el resultado
head(escuelas_con_extranjeros, 10) 
head(escuelas_sin_extranjeros, 10)  # Muestra las primeras 10 filas

# Guardar las tablas en un .csv
write.csv(extranjeros_por_escuela, "extranjeros_por_escuela.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(escuelas_sin_extranjeros, "escuelas_sin_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(escuelas_con_extranjeros, "escuelas_con_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")