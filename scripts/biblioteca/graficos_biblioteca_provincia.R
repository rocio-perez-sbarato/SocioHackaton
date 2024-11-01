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

# Definir las columnas que contienen información sobre biblioteca
columnas_biblioteca <- c(
  'id',
  'provincia',
  'Biblioteca...Dispone.de.al.menos.una....Si')

# ================================================================
# PROVINCIA: TOTAL
# ================================================================

# Crear un nuevo dataframe solo con las columnas relevantes
características_infraestructura_provincial <- características %>%
  select(all_of(columnas_biblioteca)) %>%
  # Calcular los totales usando rowSums para contar las escuelas con biblioteca 
  mutate(
    total_biblioteca = rowSums(select(., 'Biblioteca...Dispone.de.al.menos.una....Si') == "X", na.rm = TRUE),
  ) %>%
  # Agrupar por provincia
  group_by(provincia) %>%
  # Calcular las cantidades y porcentajes
  summarise(
    cantidad_escuelas_con_biblioteca = sum(total_biblioteca > 0, na.rm = TRUE),
    cantidad_escuelas_sin_biblioteca = sum(total_biblioteca == 0, na.rm = TRUE),
    total = n(),  # Cambiar el nombre para reflejar el contexto
    .groups = 'drop'  # Para eliminar la agrupación después de la summarise
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_biblioteca = round((cantidad_escuelas_con_biblioteca / total) * 100,2),
    porcentaje_escuelas_sin_biblioteca = round((cantidad_escuelas_sin_biblioteca / total) * 100,2)
    )

# ================================================================
# PROVINCIA: EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas relevantes
características_infraestructura_provincial_extranjeros <- características_con_extranjeros %>%
  select(all_of(columnas_biblioteca)) %>%
  # Calcular los totales usando rowSums para contar las escuelas con biblioteca 
  mutate(
    total_biblioteca = rowSums(select(., 'Biblioteca...Dispone.de.al.menos.una....Si') == "X", na.rm = TRUE),
  ) %>%
  # Agrupar por provincia
  group_by(provincia) %>%
  # Calcular las cantidades y porcentajes
  summarise(
    cantidad_escuelas_con_biblioteca = sum(total_biblioteca > 0, na.rm = TRUE),
    cantidad_escuelas_sin_biblioteca = sum(total_biblioteca == 0, na.rm = TRUE),
    total_con_extranjeros = n(),
    .groups = 'drop'  # Para eliminar la agrupación después de la summarise
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_biblioteca = round((cantidad_escuelas_con_biblioteca / total_con_extranjeros) * 100,2),
    porcentaje_escuelas_sin_biblioteca = round((cantidad_escuelas_sin_biblioteca / total_con_extranjeros) * 100,2)
  )

# ================================================================
# PROVINCIA: SIN EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas relevantes
características_infraestructura_provincial_sin_extranjeros <- características_sin_extranjeros %>%
  select(all_of(columnas_biblioteca)) %>%
  # Calcular los totales usando rowSums para contar las escuelas con biblioteca 
  mutate(
    total_biblioteca = rowSums(select(., 'Biblioteca...Dispone.de.al.menos.una....Si') == "X", na.rm = TRUE),
  ) %>%
  # Agrupar por provincia
  group_by(provincia) %>%
  # Calcular las cantidades y porcentajes
  summarise(
    cantidad_escuelas_con_biblioteca = sum(total_biblioteca > 0, na.rm = TRUE),
    cantidad_escuelas_sin_biblioteca = sum(total_biblioteca == 0, na.rm = TRUE),
    total_sin_extranjeros = n(),
    .groups = 'drop'  # Para eliminar la agrupación después de la summarise
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
head(características_infraestructura_provincial_extranjeros)
head(características_infraestructura_provincial_sin_extranjeros)
head(características_infraestructura_provincial)

# ================================================================
# GRÁFICOS
# ================================================================

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# ------------------ TOTAL -----------------

df_biblioteca <- data.frame(
  provincia = características_infraestructura_provincial$provincia,
  sector = c(rep("Con Biblioteca", nrow(características_infraestructura_provincial)),
             rep("Sin Biblioteca", nrow(características_infraestructura_provincial))),
  cantidad_escuelas = c(características_infraestructura_provincial$cantidad_escuelas_con_biblioteca,
                        características_infraestructura_provincial$cantidad_escuelas_sin_biblioteca),
  porcentaje = c(características_infraestructura_provincial$porcentaje_escuelas_con_biblioteca,
                 características_infraestructura_provincial$porcentaje_escuelas_sin_biblioteca)
)

# Reordenar el factor 'sector' para que "Con Biblioteca" aparezca primero
df_biblioteca$sector <- factor(df_biblioteca$sector, levels = c("Sin Biblioteca", "Con Biblioteca"))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(df_biblioteca, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +  # Mantener 'stack' para apilar
  geom_text(aes(label = round(porcentaje, 1)),  # Redondear el porcentaje a 1 decimal
            position = position_stack(vjust = 0.5),  # Centrar el texto en el medio de cada segmento
            color = "white",  # Color del texto
            size = 4) +  # Ajustar tamaño de texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas \n por Provincia y Disponibilidad de Biblioteca (2023)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),  # Eliminar el título de la leyenda
        legend.background = element_rect(size = 0.2)) +  # Fondo gris para la leyenda
  scale_fill_manual('Disponibilidad de Biblioteca', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_disponibilidad_biblioteca.png", width = 8, height = 8, dpi = 300)

# ------------------ EXTRANJEROS -----------------

df_biblioteca <- data.frame(
  provincia = características_infraestructura_provincial_extranjeros$provincia,
  sector = c(rep("Con Biblioteca", nrow(características_infraestructura_provincial_extranjeros)),
             rep("Sin Biblioteca", nrow(características_infraestructura_provincial_extranjeros))),
  cantidad_escuelas = c(características_infraestructura_provincial_extranjeros$cantidad_escuelas_con_biblioteca,
                        características_infraestructura_provincial_extranjeros$cantidad_escuelas_sin_biblioteca),
  porcentaje = c(características_infraestructura_provincial_extranjeros$porcentaje_escuelas_con_biblioteca,
                 características_infraestructura_provincial_extranjeros$porcentaje_escuelas_sin_biblioteca)
)

# Reordenar el factor 'sector' para que "Con Biblioteca" aparezca primero
df_biblioteca$sector <- factor(df_biblioteca$sector, levels = c("Sin Biblioteca", "Con Biblioteca"))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(df_biblioteca, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +  # Mantener 'stack' para apilar
  geom_text(aes(label = round(porcentaje, 1)),  # Redondear el porcentaje a 1 decimal
            position = position_stack(vjust = 0.5),  # Centrar el texto en el medio de cada segmento
            color = "white",  # Color del texto
            size = 4) +  # Ajustar tamaño de texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas Con Extranjeros \n por Provincia y Disponibilidad de Biblioteca (2023)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),  # Eliminar el título de la leyenda
        legend.background = element_rect(size = 0.2)) +  # Fondo gris para la leyenda
  scale_fill_manual('Disponibilidad de Biblioteca', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_disponibilidad_biblioteca_extranjeros.png", width = 8, height = 8, dpi = 300)

# ------------------ SIN EXTRANJEROS -----------------

df_biblioteca <- data.frame(
  provincia = características_infraestructura_provincial_sin_extranjeros$provincia,
  sector = c(rep("Con Biblioteca", nrow(características_infraestructura_provincial_sin_extranjeros)),
             rep("Sin Biblioteca", nrow(características_infraestructura_provincial_sin_extranjeros))),
  cantidad_escuelas = c(características_infraestructura_provincial_sin_extranjeros$cantidad_escuelas_con_biblioteca,
                        características_infraestructura_provincial_sin_extranjeros$cantidad_escuelas_sin_biblioteca),
  porcentaje = c(características_infraestructura_provincial_sin_extranjeros$porcentaje_escuelas_con_biblioteca,
                 características_infraestructura_provincial_sin_extranjeros$porcentaje_escuelas_sin_biblioteca)
)

# Reordenar el factor 'sector' para que "Con Biblioteca" aparezca primero
df_biblioteca$sector <- factor(df_biblioteca$sector, levels = c("Sin Biblioteca", "Con Biblioteca"))


# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(df_biblioteca, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +  # Mantener 'stack' para apilar
  geom_text(aes(label = round(porcentaje, 1)),  # Redondear el porcentaje a 1 decimal
            position = position_stack(vjust = 0.5),  # Centrar el texto en el medio de cada segmento
            color = "white",  # Color del texto
            size = 4) +  # Ajustar tamaño de texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas Sin Extranjeros \n por Provincia y Disponibilidad de Biblioteca (2023)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),  # Eliminar el título de la leyenda
        legend.background = element_rect(size = 0.2)) +  # Fondo gris para la leyenda
  scale_fill_manual('Disponibilidad de Biblioteca', values = c( '#cd4d51','#91cd4d'))   +
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_disponibilidad_biblioteca_sin_extranjeros.png", width = 8, height = 8, dpi = 300)