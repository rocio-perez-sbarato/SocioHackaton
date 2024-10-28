# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# ================================================================
# TOTAL
# ================================================================

# TOTAL: Cantidad de escuelas privadas y estatales (total)
conteo_sector_escuelas_total <- poblacion %>%
  group_by(sector) %>%
  summarise(cantidad_escuelas_por_sector = n())

# Calcular el porcentaje de escuelas por sector (total)
total_escuelas_total <- sum(conteo_sector_escuelas_total$cantidad_escuelas_por_sector)
conteo_sector_escuelas_total <- conteo_sector_escuelas_total %>%
  mutate(porcentaje_escuelas_por_sector = round((cantidad_escuelas_por_sector / total_escuelas_total) * 100, 2))

# ================================================================
# EXTRANJEROS
# ================================================================

# EXTRANJEROS: Cantidad de escuelas con estudiantes extranjeros
conteo_sector_escuelas_extranjeros <- escuelas_con_extranjeros %>%
  group_by(sector) %>%
  summarise(cantidad_escuelas_por_sector = n())

# Calcular el porcentaje de escuelas con extranjeros por sector
total_escuelas_extranjeros <- sum(conteo_sector_escuelas_extranjeros$cantidad_escuelas_por_sector)
conteo_sector_escuelas_extranjeros <- conteo_sector_escuelas_extranjeros %>%
  mutate(porcentaje_escuelas_por_sector = round((cantidad_escuelas_por_sector / total_escuelas_extranjeros) * 100, 2))

# ================================================================
# SIN EXTRANJEROS
# ================================================================

# SIN EXTRANJEROS: Cantidad de escuelas sin estudiantes extranjeros
conteo_sector_escuelas_sin_extranjeros <- escuelas_sin_extranjeros %>%
  group_by(sector) %>%
  summarise(cantidad_escuelas_por_sector = n())

# Calcular el porcentaje de escuelas sin extranjeros por sector
total_escuelas_sin_extranjeros <- sum(conteo_sector_escuelas_sin_extranjeros$cantidad_escuelas_por_sector)
conteo_sector_escuelas_sin_extranjeros <- conteo_sector_escuelas_sin_extranjeros %>%
  mutate(porcentaje_escuelas_por_sector = round((cantidad_escuelas_por_sector / total_escuelas_sin_extranjeros) * 100, 2))

# ================================================================
# EXPORTAR CSV 
# ================================================================

# Guardar los resultados nacionales en CSV
write.csv(conteo_sector_escuelas_total, "conteo_sector_escuelas_total.csv", row.names = FALSE)
write.csv(conteo_sector_escuelas_extranjeros, "conteo_sector_escuelas_extranjeros.csv", row.names = FALSE)
write.csv(conteo_sector_escuelas_sin_extranjeros, "conteo_sector_escuelas_sin_extranjeros.csv", row.names = FALSE)

# ================================================================
# GRÁFICOS
# ================================================================

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# Definir colores para los sectores
colors <- c('#ff7555', '#41e1d1')

# ------------------ TOTAL -----------------

# Extraer los porcentajes y sectores
porcentajes <- conteo_sector_escuelas_total$porcentaje_escuelas_por_sector
sectores <- conteo_sector_escuelas_total$sector


# Crear gráfico de torta con los porcentajes
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = "Distribución Total de Escuelas Privadas \n y Estatales en Argentina (2021)", border = colors)

# Agregar la leyenda
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_escuelas_sector_2021.png", width = 500, height = 500)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = "Distribución Total de Escuelas Privadas \n y Estatales en Argentina (2021)", border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()  # Cerrar el dispositivo gráfico para guardar la imagen

# ------------------ EXTRANJEROS -----------------

# Extraer los porcentajes y sectores
porcentajes <- conteo_sector_escuelas_extranjeros$porcentaje_escuelas_por_sector
sectores <- conteo_sector_escuelas_extranjeros$sector

# Crear gráfico de torta con los porcentajes
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = "Distribución de Escuelas con Extranjeros \n según Gestión Privada y Estatal en Argentina (2021)", border = colors)

# Agregar la leyenda
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_escuelas_sector_extranjeros_2021.png", width = 500, height = 500)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = "Distribución de Escuelas con Extranjeros \n según Gestión Privada y Estatal en Argentina (2021)", border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()  # Cerrar el dispositivo gráfico para guardar la imagen

# ------------------ SIN EXTRANJEROS -----------------

# Extraer los porcentajes y sectores
porcentajes <- conteo_sector_escuelas_sin_extranjeros$porcentaje_escuelas_por_sector
sectores <- conteo_sector_escuelas_sin_extranjeros$sector

# Crear gráfico de torta con los porcentajes
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = "Distribución de Escuelas sin Extranjeros \n según Gestión Privada y Estatal en Argentina (2021)", border = colors)

# Agregar la leyenda
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

# Guardar el gráfico como imagen
png("grafico_torta_escuelas_sector_sin_extranjeros_2021.png", width = 500, height = 500)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = "Distribución de Escuelas sin Extranjeros \n según Gestión Privada y Estatal en Argentina (2021)", border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()  # Cerrar el dispositivo gráfico para guardar la imagen