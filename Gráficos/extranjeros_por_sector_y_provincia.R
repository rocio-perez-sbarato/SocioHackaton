# Cargar la base de datos
poblacion <- read.csv("bdd_por_escuela_población_2021.csv", sep = ";", fileEncoding = "UTF-8")

# TOTAL: Cantidad de escuelas privadas y estatales (total)
conteo_sector_escuelas_total <- poblacion %>%
  group_by(sector) %>%
  summarise(cantidad_escuelas_por_sector = n())

# Calcular el porcentaje de escuelas por sector (total)
total_escuelas_total <- sum(conteo_sector_escuelas_total$cantidad_escuelas_por_sector)
conteo_sector_escuelas_total <- conteo_sector_escuelas_total %>%
  mutate(porcentaje_escuelas_por_sector = round((cantidad_escuelas_por_sector / total_escuelas_total) * 100, 2))

# EXTRANJEROS: Cantidad de escuelas privadas y estatales con estudiantes extranjeros
conteo_sector_escuelas_extranjeros <- escuelas_con_extranjeros %>%
  group_by(sector) %>%
  summarise(cantidad_escuelas_por_sector = n())

# Calcular el porcentaje de escuelas con extranjeros por sector
total_escuelas_extranjeros <- sum(conteo_sector_escuelas_extranjeros$cantidad_escuelas_por_sector)
conteo_sector_escuelas_extranjeros <- conteo_sector_escuelas_extranjeros %>%
  mutate(porcentaje_escuelas_por_sector = round((cantidad_escuelas_por_sector / total_escuelas_extranjeros) * 100, 2))

# SIN EXTRANJEROS: Cantidad de escuelas privadas y estatales sin estudiantes extranjeros
conteo_sector_escuelas_sin_extranjeros <- escuelas_sin_extranjeros %>%
  group_by(sector) %>%
  summarise(cantidad_escuelas_por_sector = n())

# Calcular el porcentaje de escuelas sin extranjeros por sector
total_escuelas_sin_extranjeros <- sum(conteo_sector_escuelas_sin_extranjeros$cantidad_escuelas_por_sector)
conteo_sector_escuelas_sin_extranjeros <- conteo_sector_escuelas_sin_extranjeros %>%
  mutate(porcentaje_escuelas_por_sector = round((cantidad_escuelas_por_sector / total_escuelas_sin_extranjeros) * 100, 2))

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
  select(-total_escuelas)

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
  select(-total_escuelas_extranjeros)

# PROVINCIA: Cantidad de escuelas sin extranjeros por sector en cada provincia
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
  select(-total_escuelas_sin_extranjeros)

# Guardar todos los resultados en archivos CSV
write.csv(conteo_sector_escuelas_total, "conteo_sector_escuelas_total.csv", row.names = FALSE)
write.csv(conteo_sector_escuelas_extranjeros, "conteo_sector_escuelas_extranjeros.csv", row.names = FALSE)
write.csv(conteo_sector_escuelas_sin_extranjeros, "conteo_sector_escuelas_sin_extranjeros.csv", row.names = FALSE)
write.csv(cantidad_escuelas_por_sector_y_provincia, "cantidad_escuelas_por_sector_y_provincia.csv", row.names = FALSE)
write.csv(cantidad_escuelas_por_sector_y_provincia_extranjeros, "cantidad_escuelas_por_sector_y_provincia_extranjeros.csv", row.names = FALSE)
write.csv(cantidad_escuelas_por_sector_y_provincia_sin_extranjeros, "cantidad_escuelas_por_sector_y_provincia_sin_extranjeros.csv", row.names = FALSE)

# Mostrar las primeras filas de cada resultado para verificar
head(conteo_sector_escuelas_total)
head(conteo_sector_escuelas_extranjeros)
head(conteo_sector_escuelas_sin_extranjeros)
head(cantidad_escuelas_por_sector_y_provincia)
head(cantidad_escuelas_por_sector_y_provincia_extranjeros)
head(cantidad_escuelas_por_sector_y_provincia_sin_extranjeros)

# --------------GRÁFICOS------------

# Definir colores para los sectores
colors <- c('#ff7555', '#41e1d1')

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# ------------------ TOTAL -----------------

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

# ------------------ Provincial -----------------

df <- data.frame(
  provincia = cantidad_escuelas_por_sector_y_provincia$provincia,
  sector = cantidad_escuelas_por_sector_y_provincia$sector,
  cantidad_escuelas = cantidad_escuelas_por_sector_y_provincia$cantidad_escuelas_por_sector_y_provincia,  # Accediendo directamente desde el CSV
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
ggsave("porcentaje_de_escuelas_por_provincia_sector_2021.png", width = 8, height = 8, dpi = 300, bg = "white")

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

# ------------------ Provincial -----------------

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
ggsave("porcentaje_de_escuelas_por_provincia_sector_extranjeros_2021.png", width = 8, height = 8, dpi = 300, bg = "white")

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

# ------------------ Provincial -----------------

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
ggsave("porcentaje_de_escuelas_por_provincia_sector_sin_extranjeros_2021.png", width = 8, height = 8, dpi = 300, bg = "white")
