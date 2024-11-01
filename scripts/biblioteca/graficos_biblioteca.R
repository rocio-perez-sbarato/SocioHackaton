######################################################################
# Análisis de acceso a biblioteca por escuelas primarias             #
# y secundarias en Argentina                                         #
# Nota: Ejecutar `cruce_poblacion_caracteristicas.R` antes de este   #
#       script para preparar las tablas necesarias llamadas          #
#       caracteristicas_extranjeros, caracteristicas_sin_extranjeros #
#       y caracteristicas                                            #
######################################################################

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# ================================================================
# TOTAL
# ================================================================

# Definir las columnas que contienen información sobre biblioteca y laboratorio
columnas_biblioteca_laboratorio <- c(
  'id',
  'provincia',
  'BibliotecaDisponedealmenosunaSi')

# Crear un nuevo dataframe solo con las columnas relevantes
características_infraestructura_nacional <- características %>%
  select(all_of(columnas_biblioteca_laboratorio)) %>%
  # Calcular los totales usando rowSums para contar las escuelas con biblioteca y laboratorio
  mutate(
    total_biblioteca = rowSums(select(., 'BibliotecaDisponedealmenosunaSi') == "X", na.rm = TRUE)
  ) %>%
  # Calcular las cantidades
  summarise(
    cantidad_escuelas_con_biblioteca = sum(total_biblioteca > 0, na.rm = TRUE),
    cantidad_escuelas_sin_biblioteca = sum(total_biblioteca == 0, na.rm = TRUE),
    total = nrow(características)
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_biblioteca = round((cantidad_escuelas_con_biblioteca / total) * 100,2),
    porcentaje_escuelas_sin_biblioteca = round((cantidad_escuelas_sin_biblioteca / total) * 100,2)
  )

# ================================================================
# EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas relevantes
características_infraestructura_nacional_extranjeros <- características_con_extranjeros %>%
  select(all_of(columnas_biblioteca_laboratorio)) %>%
  # Calcular los totales usando rowSums para contar las escuelas con biblioteca y laboratorio
  mutate(
    total_biblioteca = rowSums(select(., 'BibliotecaDisponedealmenosunaSi') == "X", na.rm = TRUE),
  ) %>%
  # Calcular las cantidades
  summarise(
    cantidad_escuelas_con_biblioteca = sum(total_biblioteca > 0, na.rm = TRUE),
    cantidad_escuelas_sin_biblioteca = sum(total_biblioteca == 0, na.rm = TRUE),
    total_con_extranjeros = nrow(características_con_extranjeros)
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_biblioteca = round((cantidad_escuelas_con_biblioteca / total_con_extranjeros) * 100,2),
    porcentaje_escuelas_sin_biblioteca = round((cantidad_escuelas_sin_biblioteca / total_con_extranjeros) * 100,2)
  )

# ================================================================
# SIN EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas relevantes
características_infraestructura_nacional_sin_extranjeros <- características_sin_extranjeros %>%
  select(all_of(columnas_biblioteca_laboratorio)) %>%
  # Calcular los totales usando rowSums para contar las escuelas con biblioteca y laboratorio
  mutate(
    total_biblioteca = rowSums(select(., 'BibliotecaDisponedealmenosunaSi') == "X", na.rm = TRUE),
  ) %>%
  # Calcular las cantidades
  summarise(
    cantidad_escuelas_con_biblioteca = sum(total_biblioteca > 0, na.rm = TRUE),
    cantidad_escuelas_sin_biblioteca = sum(total_biblioteca == 0, na.rm = TRUE),
    total_sin_extranjeros = nrow(características_sin_extranjeros)
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_biblioteca = round((cantidad_escuelas_con_biblioteca / total_sin_extranjeros) * 100,2),
    porcentaje_escuelas_sin_biblioteca = round((cantidad_escuelas_sin_biblioteca / total_sin_extranjeros) * 100,2)
  )

# ================================================================
# MOSTRAR RESULTADOS 
# ================================================================

# Mostrar los resultados
head(características_infraestructura_nacional_extranjeros)
head(características_infraestructura_nacional_sin_extranjeros)
head(características_infraestructura_nacional)

# ================================================================
# GRÁFICOS
# ================================================================

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# Colores para el gráfico
colors <- c('#91cd4d', '#cd4d51')  # Colores para el gráfico

# ------------------ TOTAL -----------------

# Extraer cantidades de características de infraestructura sin extranjeros
cantidad_escuelas_con_biblioteca <- características_infraestructura_nacional$cantidad_escuelas_con_biblioteca
cantidad_escuelas_sin_biblioteca <- características_infraestructura_nacional$cantidad_escuelas_sin_biblioteca

# Cálculo de porcentajes
porcentaje_con_biblioteca <- características_infraestructura_nacional$porcentaje_escuelas_con_biblioteca
porcentaje_sin_biblioteca <- características_infraestructura_nacional$porcentaje_escuelas_sin_biblioteca

# Crear un dataframe para el gráfico de acceso a biblioteca
conteo_biblioteca <- data.frame(
  biblioteca_categoria = c("Con Biblioteca", "Sin Biblioteca"),
  cantidad = c(cantidad_escuelas_con_biblioteca,
               cantidad_escuelas_sin_biblioteca),
  porcentaje = c(porcentaje_con_biblioteca,
                 porcentaje_sin_biblioteca)
)

# Colores para el gráfico
colors <- c('#91cd4d', '#cd4d51')  # Colores para el gráfico

# Crear el gráfico de torta para acceso a biblioteca
pie(conteo_biblioteca$porcentaje,
    labels = paste0(round(conteo_biblioteca$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas \n según Disponibilidad de Biblioteca (2012)",
    border = colors)

# Agregar la leyenda
legend("bottomleft", legend = conteo_biblioteca$biblioteca_categoria, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_disponibilidad_biblioteca.png", width = 500, height = 500)
pie(conteo_biblioteca_extranjeros$porcentaje,
    labels = paste0(round(conteo_biblioteca$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas \n según Disponibilidad de Biblioteca (2012)",
    border = colors)
legend("bottomleft", legend = conteo_biblioteca$biblioteca_categoria, fill = colors, cex = 0.8)
dev.off()

# ------------------ EXTRANJEROS -----------------

# Extraer cantidades de características de infraestructura con extranjeros
cantidad_escuelas_con_biblioteca_extranjeros <- características_infraestructura_nacional_extranjeros$cantidad_escuelas_con_biblioteca
cantidad_escuelas_sin_biblioteca_extranjeros <- características_infraestructura_nacional_extranjeros$cantidad_escuelas_sin_biblioteca

# Cálculo de porcentajes
porcentaje_con_biblioteca <- características_infraestructura_nacional_extranjeros$porcentaje_escuelas_con_biblioteca
porcentaje_sin_biblioteca <- características_infraestructura_nacional_extranjeros$porcentaje_escuelas_sin_biblioteca

# Crear un dataframe para el gráfico de acceso a biblioteca
conteo_biblioteca_extranjeros <- data.frame(
  biblioteca_categoria = c("Con Biblioteca", "Sin Biblioteca"),
  cantidad = c(cantidad_escuelas_con_biblioteca_extranjeros,
               cantidad_escuelas_sin_biblioteca_extranjeros),
  porcentaje = c(porcentaje_con_biblioteca,
                 porcentaje_sin_biblioteca)
)

# Crear el gráfico de torta para acceso a biblioteca
pie(conteo_biblioteca_extranjeros$porcentaje,
    labels = paste0(round(conteo_biblioteca_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas con Extranjeros \n según Disponibilidad de Biblioteca (2023)",
    border = colors)

# Agregar la leyenda
legend("bottomleft", legend = conteo_biblioteca_extranjeros$biblioteca_categoria, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_disponibilidad_biblioteca_extranjeros.png", width = 500, height = 500)
pie(conteo_biblioteca_extranjeros$porcentaje,
    labels = paste0(round(conteo_biblioteca_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas con Extranjeros \n según Disponibilidad de Biblioteca (2023)",
    border = colors)
legend("bottomleft", legend = conteo_biblioteca_extranjeros$biblioteca_categoria, fill = colors, cex = 0.8)
dev.off()

# ------------------ SIN EXTRANJEROS -----------------

# Extraer cantidades de características de infraestructura sin extranjeros
cantidad_escuelas_con_biblioteca_sin_extranjeros <- características_infraestructura_nacional_sin_extranjeros$cantidad_escuelas_con_biblioteca
cantidad_escuelas_sin_biblioteca_sin_extranjeros <- características_infraestructura_nacional_sin_extranjeros$cantidad_escuelas_sin_biblioteca

# Cálculo de porcentajes
porcentaje_con_biblioteca_sin_extranjeros <- características_infraestructura_nacional_sin_extranjeros$porcentaje_escuelas_con_biblioteca
porcentaje_sin_biblioteca_sin_extranjeros <- características_infraestructura_nacional_sin_extranjeros$porcentaje_escuelas_sin_biblioteca

# Crear un dataframe para el gráfico de acceso a biblioteca
conteo_biblioteca_sin_extranjeros <- data.frame(
  biblioteca_categoria = c("Con Biblioteca", "Sin Biblioteca"),
  cantidad = c(cantidad_escuelas_con_biblioteca_sin_extranjeros,
               cantidad_escuelas_sin_biblioteca_sin_extranjeros),
  porcentaje = c(porcentaje_con_biblioteca_sin_extranjeros,
                 porcentaje_sin_biblioteca_sin_extranjeros)
)

# Crear el gráfico de torta para acceso a biblioteca
pie(conteo_biblioteca_sin_extranjeros$porcentaje,
    labels = paste0(round(conteo_biblioteca_sin_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas sin Extranjeros \n según Disponibilidad de Biblioteca (2023)",
    border = colors)

# Agregar la leyenda
legend("bottomleft", legend = conteo_biblioteca_sin_extranjeros$biblioteca_categoria, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_disponibilidad_biblioteca_sin_extranjeros.png", width = 500, height = 500)
pie(conteo_biblioteca_sin_extranjeros$porcentaje,
    labels = paste0(round(conteo_biblioteca_sin_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas sin Extranjeros \n según Disponibilidad de Biblioteca (2023)",
    border = colors)
legend("bottomleft", legend = conteo_biblioteca_sin_extranjeros$biblioteca_categoria, fill = colors, cex = 0.8)
dev.off()

