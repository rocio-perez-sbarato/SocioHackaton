##############################################################################
# Código de sectores de escuelas primarias y secundarias en Argentina        #
# Nota: Ejecutar `filtrado_escuelas_extranjeros.R` antes de este             #
#       script para generar las tablas necesarias:                           #
#       `poblacion`, `escuelas_con_extranjeros`, `escuelas_sin_extranjeros`. #
##############################################################################

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# ================================================================
# PROVINCIA: TOTAL
# ================================================================

# PROVINCIA: Cantidad de escuelas por sector en cada provincia
cantidad_escuelas_por_sector_y_provincia <- poblacion %>%
  group_by(provincia, sector) %>%
  summarise(cantidad_escuelas_por_sector_y_provincia = n(), .groups = "drop")

# Calcular el porcentaje de escuelas por sector y provincia
total_escuelas_por_provincia <- cantidad_escuelas_por_sector_y_provincia %>%
  group_by(provincia) %>%
  summarise(total_escuelas = sum(cantidad_escuelas_por_sector_y_provincia))

cantidad_escuelas_por_sector_y_provincia <- cantidad_escuelas_por_sector_y_provincia %>%
  left_join(total_escuelas_por_provincia, by = "provincia") %>%
  mutate(porcentaje_escuelas_por_sector_y_provincia = round((cantidad_escuelas_por_sector_y_provincia / total_escuelas) * 100, 2)) %>%
  select(-total_escuelas) # Eliminar la columna para que no esté repetida

# ================================================================
# PROVINCIA: EXTRANJEROS
# ================================================================

# PROVINCIA: Cantidad de escuelas con extranjeros por sector en cada provincia
cantidad_escuelas_por_sector_y_provincia_extranjeros <- escuelas_con_extranjeros %>%
  group_by(provincia, sector) %>%
  summarise(cantidad_escuelas_por_sector_y_provincia = n(), .groups = "drop")

# Calcular el porcentaje de escuelas con extranjeros por sector y provincia
total_escuelas_por_provincia_extranjeros <- cantidad_escuelas_por_sector_y_provincia_extranjeros %>%
  group_by(provincia) %>%
  summarise(total_escuelas_extranjeros = sum(cantidad_escuelas_por_sector_y_provincia))

cantidad_escuelas_por_sector_y_provincia_extranjeros <- cantidad_escuelas_por_sector_y_provincia_extranjeros %>%
  left_join(total_escuelas_por_provincia_extranjeros, by = "provincia") %>%
  mutate(porcentaje_escuelas_por_sector_y_provincia_extranjeros = round((cantidad_escuelas_por_sector_y_provincia / total_escuelas_extranjeros) * 100, 2)) %>%
  select(-total_escuelas_extranjeros) # Eliminar la columna para que no esté repetida

# ================================================================
# PROVINCIA: SIN EXTRANJEROS
# ================================================================

# Calcular la cantidad de escuelas sin extranjeros por sector en cada provincia
cantidad_escuelas_por_sector_y_provincia_sin_extranjeros <- escuelas_sin_extranjeros %>%
  group_by(provincia, sector) %>%
  summarise(cantidad_escuelas_por_sector_y_provincia = n(), .groups = "drop")

# Calcular el porcentaje de escuelas sin extranjeros por sector y provincia
total_escuelas_por_provincia_sin_extranjeros <- cantidad_escuelas_por_sector_y_provincia_sin_extranjeros %>%
  group_by(provincia) %>%
  summarise(total_escuelas_sin_extranjeros = sum(cantidad_escuelas_por_sector_y_provincia))

cantidad_escuelas_por_sector_y_provincia_sin_extranjeros <- cantidad_escuelas_por_sector_y_provincia_sin_extranjeros %>%
  left_join(total_escuelas_por_provincia_sin_extranjeros, by = "provincia") %>%
  mutate(porcentaje_escuelas_por_sector_y_provincia_sin_extranjeros = round((cantidad_escuelas_por_sector_y_provincia / total_escuelas_sin_extranjeros) * 100, 2)) %>%
  select(-total_escuelas_sin_extranjeros)  # Eliminar la columna para que no esté repetida

# Guardar los resultados provinciales en CSV
write.csv(cantidad_escuelas_por_sector_y_provincia, "cantidad_escuelas_por_sector_y_provincia.csv", row.names = FALSE)
write.csv(cantidad_escuelas_por_sector_y_provincia_extranjeros, "cantidad_escuelas_por_sector_y_provincia_extranjeros.csv", row.names = FALSE)
write.csv(cantidad_escuelas_por_sector_y_provincia_sin_extranjeros, "cantidad_escuelas_por_sector_y_provincia_sin_extranjeros.csv", row.names = FALSE)

# --------------GRÁFICOS------------

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# ------------------ TOTAL -----------------

df <- data.frame(
  provincia = cantidad_escuelas_por_sector_y_provincia$provincia,
  sector = cantidad_escuelas_por_sector_y_provincia$sector,
  cantidad_escuelas = cantidad_escuelas_por_sector_y_provincia$cantidad_escuelas_por_sector_y_provincia,  
  porcentaje = cantidad_escuelas_por_sector_y_provincia$porcentaje_escuelas_por_sector_y_provincia
)

# Asegurarse de que el sector estatal aparezca primero
df$sector <- factor(df$sector, levels = c("Privado", "Estatal"))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(df, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +  # Mantener 'stack' para apilar
  geom_text(aes(label = round(porcentaje, 2)),  # Redondear el porcentaje a 1 decimal
            position = position_stack(vjust = 0.5),  # Ajustar el texto en el centro
            color = "white",  # Color del texto
            size = 4) +  # Ajustar tamaño de texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas', title = 'Porcentaje del Total de Escuelas \n por Provincia y Sector (2021)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),  # Eliminar el título de la leyenda
        legend.background = element_rect(size = 0.2)) +  # Fondo gris para la leyenda
  scale_fill_manual('Sector', values = c('#41e1d1', '#ff7555')) +
  coord_flip()  # Voltear el gráfico para que sea horizontal (Estatal a la izquierda)

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_sector.png", width = 8, height = 8, dpi = 300, bg = "white")

# ------------------ EXTRANJEROS -----------------

df <- data.frame(
  provincia = cantidad_escuelas_por_sector_y_provincia_extranjeros$provincia,
  sector = cantidad_escuelas_por_sector_y_provincia_extranjeros$sector,
  cantidad_escuelas = cantidad_escuelas_por_sector_y_provincia_extranjeros$cantidad_escuelas_por_sector_y_provincia,
  porcentaje = cantidad_escuelas_por_sector_y_provincia_extranjeros$porcentaje_escuelas_por_sector_y_provincia_extranjeros
)

# Asegurarse de que el sector estatal aparezca primero
df$sector <- factor(df$sector, levels = c("Privado", "Estatal"))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(df, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +  # Mantener 'stack' para apilar
  geom_text(aes(label = round(porcentaje, 2)),  # Redondear el porcentaje a 1 decimal
            position = position_stack(vjust = 0.4),  # Centrar el texto en el medio de cada segmento
            color = "white",  # Color del texto
            size = 4) +  # Ajustar tamaño de texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas con Extranjeros', title = 'Porcentaje de Escuelas con Extranjeros \n por Provincia y Sector (2021)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),  # Eliminar el título de la leyenda
        legend.background = element_rect(size = 0.2)) +  # Fondo gris para la leyenda
  scale_fill_manual('Sector', values = c('#41e1d1', '#ff7555')) +
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_sector_extranjeros.png", width = 8, height = 8, dpi = 300, bg = "white")

# ------------------ SIN EXTRANJEROS -----------------

df <- data.frame(
  provincia = cantidad_escuelas_por_sector_y_provincia_sin_extranjeros$provincia,
  sector = cantidad_escuelas_por_sector_y_provincia_sin_extranjeros$sector,
  cantidad_escuelas = cantidad_escuelas_por_sector_y_provincia_sin_extranjeros$cantidad_escuelas_por_sector_y_provincia,
  porcentaje = cantidad_escuelas_por_sector_y_provincia_sin_extranjeros$porcentaje_escuelas_por_sector_y_provincia_sin_extranjeros
)

# Asegurarse de que el sector estatal aparezca primero
df$sector <- factor(df$sector, levels = c("Privado", "Estatal"))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(df, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +  # Mantener 'stack' para apilar
  geom_text(aes(label = round(porcentaje, 2)),  # Redondear el porcentaje a 1 decimal
            position = position_stack(vjust = 0.5),  # Centrar el texto en el medio de cada segmento
            color = "white",  # Color del texto
            size = 4) +  # Ajustar tamaño de texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas sin Extranjeros', title = 'Porcentaje de Escuelas sin Extranjeros \n por Provincia y Sector (2021)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),  # Eliminar el título de la leyenda
        legend.background = element_rect(size = 0.2)) +  # Fondo gris para la leyenda
  scale_fill_manual('Sector', values =  c('#41e1d1', '#ff7555')) +
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_sector_sin_extranjeros.png", width = 8, height = 8, dpi = 300, bg = "white")
