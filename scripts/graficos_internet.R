######################################################################
# Análisis de acceso a internet por escuelas primarias               #
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

# Definir las columnas que contienen información sobre Internet
columnas_internet_pago_gratis <- c(
  'id',
  'provincia',
  'sector',
  'ambito',
  'Internet...Tipo.de.servicio...Gratuito',
  'Internet...Tipo.de.servicio...Pago'
)
# =============================================================
# Nota: En la base de 2023, hay que incorporar las columnas 
# 'Internet...Tipo.de.servicio...Gratuito.Otro' e 
# 'Internet...Tipo.de.servicio...Gratuito.Estado'.
# =============================================================

# -------------------- EXTRANJEROS --------------------
# Crear un nuevo dataframe con las columnas de Internet para escuelas con extranjeros
características_internet_nacional_extranjeros <- características_con_extranjeros %>%
  select(all_of(columnas_internet_pago_gratis)) %>%
  mutate(total_internet_gratuito = rowSums(select(., 'Internet...Tipo.de.servicio...Gratuito') == "X", na.rm = TRUE),
         total_internet_pago = rowSums(select(., 'Internet...Tipo.de.servicio...Pago') == "X", na.rm = TRUE)) %>%
  summarise(
    cantidad_escuelas_sin_internet = sum(total_internet_gratuito == 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_gratuito = sum(total_internet_gratuito > 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_pago = sum(total_internet_pago > 0 & total_internet_gratuito == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet = sum(total_internet_pago > 0 | total_internet_gratuito, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_internet_pago > 0 & total_internet_gratuito, na.rm = TRUE),
    cantidad_escuelas_extranjeros = nrow(características_con_extranjeros)
  )

# -------------------- SIN EXTRANJEROS --------------------
# Crear un nuevo dataframe con las columnas de Internet para escuelas sin extranjeros
características_internet_nacional_sin_extranjeros <- características_sin_extranjeros %>%
  select(all_of(columnas_internet_pago_gratis)) %>%
  mutate(total_internet_gratuito = rowSums(select(., 'Internet...Tipo.de.servicio...Gratuito') == "X", na.rm = TRUE),
         total_internet_pago = rowSums(select(., 'Internet...Tipo.de.servicio...Pago') == "X", na.rm = TRUE)) %>%
  summarise(
    cantidad_escuelas_sin_internet = sum(total_internet_gratuito == 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_gratuito = sum(total_internet_gratuito > 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_pago = sum(total_internet_pago > 0 & total_internet_gratuito == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet = sum(total_internet_pago > 0 | total_internet_gratuito, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_internet_pago > 0 & total_internet_gratuito, na.rm = TRUE),
    cantidad_escuelas_sin_extranjeros = nrow(características_sin_extranjeros)
  )

# -------------------- TOTAL NACIONAL --------------------
# Crear un nuevo dataframe con las columnas de Internet a nivel nacional
características_internet_nacional <- características %>%
  select(all_of(columnas_internet_pago_gratis)) %>%
  mutate(total_internet_gratuito = rowSums(select(., 'Internet...Tipo.de.servicio...Gratuito') == "X", na.rm = TRUE),
         total_internet_pago = rowSums(select(., 'Internet...Tipo.de.servicio...Pago') == "X", na.rm = TRUE)) %>%
  summarise(
    cantidad_escuelas_sin_internet = sum(total_internet_gratuito == 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_gratuito = sum(total_internet_gratuito > 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_pago = sum(total_internet_pago > 0 & total_internet_gratuito == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet = sum(total_internet_pago > 0 | total_internet_gratuito, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_internet_pago > 0 & total_internet_gratuito, na.rm = TRUE),
    cantidad_escuelas_total = nrow(características)
  )

# -------------------- MOSTRAR RESULTADOS --------------------
# Mostrar las primeras filas de cada dataframe
head(características_internet_nacional_extranjeros)
head(características_internet_nacional_sin_extranjeros)
head(características_internet_nacional)

# ================================================================
# GRÁFICOS
# ================================================================

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# Colores para el gráfico
colors <- c("#63B8FF","#FFA07A", "#7CCD7C")

# ================================ NACIONAL =================================
# Asignación de datos de acceso a internet a nivel nacional
cantidad_escuelas_sin_internet_nacional <- características_internet_nacional$cantidad_escuelas_sin_internet
cantidad_escuelas_con_internet_gratuito_nacional <- características_internet_nacional$cantidad_escuelas_con_internet_gratuito
cantidad_escuelas_con_internet_pago_nacional <- características_internet_nacional$cantidad_escuelas_con_internet_pago
total_escuelas_nacional <- características_internet_nacional$cantidad_escuelas_total

# Cálculo de porcentajes con redondeo
porcentaje_sin_internet_nacional <- round((cantidad_escuelas_sin_internet_nacional / total_escuelas_nacional) * 100, 2)
porcentaje_con_internet_gratuito_nacional <- round((cantidad_escuelas_con_internet_gratuito_nacional / total_escuelas_nacional) * 100, 2)
porcentaje_con_internet_pago_nacional <- round((cantidad_escuelas_con_internet_pago_nacional / total_escuelas_nacional) * 100, 2)

# Crear dataframe
conteo_internet_nacional <- data.frame(
  internet_categoria = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito"),
  cantidad = c(cantidad_escuelas_con_internet_pago_nacional,
              cantidad_escuelas_sin_internet_nacional,
               cantidad_escuelas_con_internet_gratuito_nacional),
  porcentaje = c(porcentaje_con_internet_pago_nacional,
                porcentaje_sin_internet_nacional,
                porcentaje_con_internet_gratuito_nacional)
)

# Crear el gráfico de torta para acceso a internet
pie(conteo_internet_nacional$porcentaje,
    labels = paste0(round(conteo_internet_nacional$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas \n según Acceso a Internet en Argentina (2023)",
    border = colors)

# Agregar la leyenda
legend("bottomright", legend = conteo_internet_nacional$internet_categoria, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_acceso_internet_nacional.png", width = 500, height = 500)
pie(conteo_internet_nacional$porcentaje,
    labels = paste0(round(conteo_internet_nacional$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas \n según Acceso a Internet en Argentina (2023)",
    border = colors)
legend("bottomright", legend = conteo_internet_nacional$internet_categoria, fill = colors, cex = 0.8)
dev.off()

# ========================= SIN EXTRANJEROS =================================
# Asignación de datos de acceso a internet para escuelas sin extranjeros
cantidad_escuelas_sin_internet_sin_extranjeros <- características_internet_nacional_sin_extranjeros$cantidad_escuelas_sin_internet
cantidad_escuelas_con_internet_gratuito_sin_extranjeros <- características_internet_nacional_sin_extranjeros$cantidad_escuelas_con_internet_gratuito
cantidad_escuelas_con_internet_pago_sin_extranjeros <- características_internet_nacional_sin_extranjeros$cantidad_escuelas_con_internet_pago
total_escuelas_sin_extranjeros <- características_internet_nacional_sin_extranjeros$cantidad_escuelas_sin_extranjeros

# Cálculo de porcentajes con redondeo
porcentaje_sin_internet_sin_extranjeros <- round((cantidad_escuelas_sin_internet_sin_extranjeros / total_escuelas_sin_extranjeros) * 100, 2)
porcentaje_con_internet_gratuito_sin_extranjeros <- round((cantidad_escuelas_con_internet_gratuito_sin_extranjeros / total_escuelas_sin_extranjeros) * 100, 2)
porcentaje_con_internet_pago_sin_extranjeros <- round((cantidad_escuelas_con_internet_pago_sin_extranjeros / total_escuelas_sin_extranjeros) * 100, 2)

# Crear un dataframe para el gráfico y para guardar
conteo_internet_sin_extranjeros <- data.frame(
  internet_categoria = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito"),
  cantidad = c(cantidad_escuelas_con_internet_pago_sin_extranjeros,
              cantidad_escuelas_sin_internet_sin_extranjeros,
               cantidad_escuelas_con_internet_gratuito_sin_extranjeros),
  porcentaje = c(porcentaje_con_internet_pago_sin_extranjeros,
                porcentaje_sin_internet_sin_extranjeros,
                porcentaje_con_internet_gratuito_sin_extranjeros)
)

# Crear el gráfico de torta para acceso a internet
pie(conteo_internet_sin_extranjeros$porcentaje,
    labels = paste0(round(conteo_internet_sin_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas sin Extranjeros \n según Acceso a Internet (2023)",
    border = colors)

# Agregar la leyenda
legend("bottomright", legend = conteo_internet_sin_extranjeros$internet_categoria, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_acceso_internet_sin_extranjeros.png", width = 500, height = 500)
pie(conteo_internet_sin_extranjeros$porcentaje,
    labels = paste0(round(conteo_internet_sin_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas sin Extranjeros \n según Acceso a Internet (2023)",
    border = colors)
legend("bottomright", legend = conteo_internet_sin_extranjeros$internet_categoria, fill = colors, cex = 0.8)
dev.off()

# ============================ EXTRANJEROS =================================
# Asignación de datos de acceso a internet para escuelas con extranjeros
cantidad_escuelas_sin_internet_extranjeros <- características_internet_nacional_extranjeros$cantidad_escuelas_sin_internet
cantidad_escuelas_con_internet_gratuito_extranjeros <- características_internet_nacional_extranjeros$cantidad_escuelas_con_internet_gratuito
cantidad_escuelas_con_internet_pago_extranjeros <- características_internet_nacional_extranjeros$cantidad_escuelas_con_internet_pago
total_escuelas_extranjeros <- características_internet_nacional_extranjeros$cantidad_escuelas_extranjeros

# Cálculo de porcentajes con redondeo
porcentaje_sin_internet <- round((cantidad_escuelas_sin_internet_extranjeros / total_escuelas_extranjeros) * 100, 2)
porcentaje_con_internet_gratuito <- round((cantidad_escuelas_con_internet_gratuito_extranjeros / total_escuelas_extranjeros) * 100, 2)
porcentaje_con_internet_pago <- round((cantidad_escuelas_con_internet_pago_extranjeros / total_escuelas_extranjeros) * 100, 2)

# Crear un dataframe para el gráfico y para guardar
conteo_internet_extranjeros <- data.frame(
  internet_categoria = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito"),
  cantidad = c(cantidad_escuelas_con_internet_pago_extranjeros,
              cantidad_escuelas_sin_internet_extranjeros,
               cantidad_escuelas_con_internet_gratuito_extranjeros),
  porcentaje = c(porcentaje_con_internet_pago,
                porcentaje_sin_internet,
                porcentaje_con_internet_gratuito)
)

# Crear el gráfico de torta para acceso a internet
pie(conteo_internet_extranjeros$porcentaje,
    labels = paste0(round(conteo_internet_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas con Extranjeros \n según Acceso a Internet (2023)",
    border = colors)

# Agregar la leyenda
legend("bottomright", legend = conteo_internet_extranjeros$internet_categoria, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_acceso_internet_extranjeros.png", width = 500, height = 500)
pie(conteo_internet_extranjeros$porcentaje,
    labels = paste0(round(conteo_internet_extranjeros$porcentaje, 2), "%"),
    col = colors,
    main = "Distribución de Escuelas con Extranjeros \n según Acceso a Internet (2023)",
    border = colors)
legend("bottomright", legend = conteo_internet_extranjeros$internet_categoria, fill = colors, cex = 0.8)
dev.off()

