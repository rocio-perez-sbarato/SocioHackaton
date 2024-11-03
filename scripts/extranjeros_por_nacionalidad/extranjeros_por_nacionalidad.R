##############################################################################
# Código de sectores de escuelas primarias y secundarias en Argentina        #
# Nota: Ejecutar `filtrado_escuelas_extranjeros.R` antes de este             #
#       script para generar la tabla necesaria:                              #
#       `poblacion`.                                                         #
##############################################################################

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Instalar y cargar la librería writexl
install.packages("writexl")
library(writexl)

# Seleccionar las columnas relevantes
columnas_pais <- c(
  'Bolivia...Primaria', 'Brasil...Primaria', 'Chile...Primaria', 'Colombia...Primaria',
  'Ecuador...Primaria', 'Paraguay...Primaria', 'Perú...Primaria', 'Uruguay...Primaria',
  'Venezuela...Primaria', 'Otros.países.de.América...Primaria', 'Europa...Primaria',
  'Asia...Primaria', 'Otros...Primaria',
  'Bolivia...Secundaria', 'Brasil...Secundaria', 'Chile...Secundaria',
  'Colombia...Secundaria', 'Ecuador...Secundaria', 'Paraguay...Secundaria',
  'Perú...Secundaria', 'Uruguay...Secundaria', 'Venezuela...Secundaria',
  'Otros.países.de.América...Secundaria', 'Europa...Secundaria',
  'Asia...Secundaria', 'Otros...Secundaria'
)

# Convertir columnas a tipo numérico
poblacion[columnas_pais] <- lapply(poblacion[columnas_pais], function(x) as.numeric(as.character(x)))

# Calcular los totales por provincia, país de origen y nivel educativo
totales_por_provincia <- poblacion %>%
  group_by(provincia) %>%
  summarise(across(all_of(columnas_pais), ~ sum(as.numeric(.), na.rm = TRUE), .names = "total_{col}"))

# Mostrar el resultado de totales por provincia
totales_por_provincia

# Sumar cantidad total de extranjeros en todas las provincias
total_extranjeros_pais <- totales_por_provincia %>%
  summarise(total_extranjeros_pais = sum(across(starts_with("total_")), na.rm = TRUE))

# Mostrar el total de extranjeros en el país
total_extranjeros_pais

# Calcular la cantidad total de extranjeros por nacionalidad
extranjeros_por_nacionalidad <- poblacion %>%
  summarise(
    Bolivia = sum(`Bolivia...Primaria`, `Bolivia...Secundaria`, na.rm = TRUE),
    Brasil = sum(`Brasil...Primaria`, `Brasil...Secundaria`, na.rm = TRUE),
    Chile = sum(`Chile...Primaria`, `Chile...Secundaria`, na.rm = TRUE),
    Colombia = sum(`Colombia...Primaria`, `Colombia...Secundaria`, na.rm = TRUE),
    Ecuador = sum(`Ecuador...Primaria`, `Ecuador...Secundaria`, na.rm = TRUE),
    Paraguay = sum(`Paraguay...Primaria`, `Paraguay...Secundaria`, na.rm = TRUE),
    Perú = sum(`Perú...Primaria`, `Perú...Secundaria`, na.rm = TRUE),
    Uruguay = sum(`Uruguay...Primaria`, `Uruguay...Secundaria`, na.rm = TRUE),
    Venezuela = sum(`Venezuela...Primaria`, `Venezuela...Secundaria`, na.rm = TRUE),
    Otros_países_America = sum(`Otros.países.de.América...Primaria`, `Otros.países.de.América...Secundaria`, na.rm = TRUE),
    Europa = sum(`Europa...Primaria`, `Europa...Secundaria`, na.rm = TRUE),
    Asia = sum(`Asia...Primaria`, `Asia...Secundaria`, na.rm = TRUE),
    Otros = sum(`Otros...Primaria`, `Otros...Secundaria`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "nacionalidad", values_to = "cantidad") %>%
  group_by(nacionalidad) %>%  # Agrupar por nacionalidad
  summarise(cantidad = sum(cantidad)) %>%  # Sumar la cantidad por nacionalidad
  arrange(desc(cantidad))  # Ordenar por cantidad

# Calcular el total de extranjeros
total_extranjeros <- sum(extranjeros_por_nacionalidad$cantidad)

# Agregar porcentaje
extranjeros_por_nacionalidad <- extranjeros_por_nacionalidad %>%
  mutate(porcentaje = (cantidad / total_extranjeros) * 100)

# Mostrar el resultado con porcentajes
print(extranjeros_por_nacionalidad)

# Guardar el data frame en un archivo Excel
write_xlsx(extranjeros_por_nacionalidad, "extranjeros_por_nacionalidad_2015.xlsx")

# Mostrar si los datos coinciden
total_extranjeros == total_extranjeros_pais