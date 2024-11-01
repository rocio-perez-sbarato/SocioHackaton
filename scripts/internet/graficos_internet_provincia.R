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

# ================================================================
# PROVINCIA: TOTAL
# ================================================================

# =============================================================
# Nota: En la base de 2023, hay que incorporar las columnas 
# 'Internet...Tipo.de.servicio...Gratuito.Otro' e 
# 'Internet...Tipo.de.servicio...Gratuito.Estado'.
# =============================================================

# Definir las columnas que contienen información sobre Internet
columnas_internet_pago_gratis <- c(
  'id',
  'provincia',
  'sector',
  'ambito',
  'Internet...Tipo.de.servicio...Gratuito',
  'Internet...Tipo.de.servicio...Pago'
)

# Crear un nuevo dataframe solo con las columnas de Internet
características_internet_por_provincia_total <- características %>%
  select(all_of(columnas_internet_pago_gratis)) %>%
  mutate(total_internet_gratuito = rowSums(select(., 'Internet...Tipo.de.servicio...Gratuito') == "X",
                                            na.rm = TRUE),
         total_internet_pago = rowSums(select(., 'Internet...Tipo.de.servicio...Pago') == "X", na.rm = TRUE)) %>%
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_sin_internet = sum(total_internet_gratuito == 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_gratuito = sum(total_internet_gratuito > 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_pago = sum(total_internet_pago > 0 & total_internet_gratuito == 0, na.rm = TRUE),
    cantidad_escuelas_total = n()
  ) %>%
  ungroup()

# Cálculo de porcentajes
características_internet_por_provincia_total <- características_internet_por_provincia_total %>%
  mutate(total_escuelas = cantidad_escuelas_total,
         porcentaje_sin_internet = round((cantidad_escuelas_sin_internet / total_escuelas) * 100,2),
         porcentaje_con_internet_gratuito = round((cantidad_escuelas_con_internet_gratuito / total_escuelas) * 100,2),
         porcentaje_con_internet_pago = round((cantidad_escuelas_con_internet_pago / total_escuelas) * 100,2))

# ================================================================
# PROVINCIA: CON EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas de Internet
características_internet_por_provincia_extranjeros <- características_con_extranjeros %>%
  select(all_of(columnas_internet_pago_gratis)) %>%
  mutate(total_internet_gratuito = rowSums(select(., 'Internet...Tipo.de.servicio...Gratuito') == "X",
                                            na.rm = TRUE),
         total_internet_pago = rowSums(select(., 'Internet...Tipo.de.servicio...Pago') == "X", na.rm = TRUE)) %>%
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_sin_internet = sum(total_internet_gratuito == 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_gratuito = sum(total_internet_gratuito > 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_pago = sum(total_internet_pago > 0 & total_internet_gratuito == 0, na.rm = TRUE),
    cantidad_escuelas_con_extranjeros = n()
  ) %>%
  ungroup()

# Cálculo de porcentajes
características_internet_por_provincia_extranjeros <- características_internet_por_provincia_extranjeros %>%
  mutate(total_escuelas_extranjeros = cantidad_escuelas_con_extranjeros,
         porcentaje_sin_internet = round((cantidad_escuelas_sin_internet / total_escuelas_extranjeros) * 100,2),
         porcentaje_con_internet_gratuito = round((cantidad_escuelas_con_internet_gratuito / total_escuelas_extranjeros) * 100,2),
         porcentaje_con_internet_pago = round((cantidad_escuelas_con_internet_pago / total_escuelas_extranjeros) * 100,2))

# ================================================================
# PROVINCIA: SIN EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas de Internet
características_internet_por_provincia_sin_extranjeros <- características_sin_extranjeros %>%
  select(all_of(columnas_internet_pago_gratis)) %>%
  mutate(total_internet_gratuito = rowSums(select(., 'Internet...Tipo.de.servicio...Gratuito') == "X",
                                            na.rm = TRUE),
         total_internet_pago = rowSums(select(., 'Internet...Tipo.de.servicio...Pago') == "X", na.rm = TRUE)) %>%
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_sin_internet = sum(total_internet_gratuito == 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_gratuito = sum(total_internet_gratuito > 0 & total_internet_pago == 0, na.rm = TRUE),
    cantidad_escuelas_con_internet_pago = sum(total_internet_pago > 0 & total_internet_gratuito == 0, na.rm = TRUE),
    cantidad_escuelas_sin_extranjeros = n()
  ) %>%
  ungroup()

# Cálculo de porcentajes
características_internet_por_provincia_sin_extranjeros <- características_internet_por_provincia_sin_extranjeros %>%
  mutate(total_escuelas_sin_extranjeros = cantidad_escuelas_sin_extranjeros,
         porcentaje_sin_internet = round((cantidad_escuelas_sin_internet / total_escuelas_sin_extranjeros) * 100,2),
         porcentaje_con_internet_gratuito = round((cantidad_escuelas_con_internet_gratuito / total_escuelas_sin_extranjeros) * 100,2),
         porcentaje_con_internet_pago = round((cantidad_escuelas_con_internet_pago / total_escuelas_sin_extranjeros) * 100,2))

# ================================================================
# EXPORTAR CSV Y MOSTRAR RESULTADOS
# ================================================================

# Mostrar los resultados
head(características_internet_por_provincia_total, 10)
head(características_internet_por_provincia_sin_extranjeros, 10)
head(características_internet_por_provincia_extranjeros, 10)

# Guardar los resultados en archivos CSV
write.csv(características_internet_por_provincia_total, 'características_internet_por_provincia_total.csv', row.names = FALSE)
write.csv(características_internet_por_provincia_sin_extranjeros, 'características_internet_por_provincia_sin_extranjeros.csv', row.names = FALSE)
write.csv(características_internet_por_provincia_extranjeros, 'características_internet_por_provincia_extranjeros.csv', row.names = FALSE)

# ================================================================
# GRÁFICOS
# ================================================================

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# ------------------ TOTAL -----------------

# Reestructurar el dataframe para facilitar la creación del gráfico
caracteristicas_long <- caracteristicas_internet_por_provincia_total %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_internet",
               values_to = "porcentaje") %>%
  mutate(tipo_internet = factor(tipo_internet,
                                 levels = c("porcentaje_sin_internet", "porcentaje_con_internet_gratuito", "porcentaje_con_internet_pago"),
                                 labels = c("Sin Internet", "Con Internet Gratuito", "Con Internet Pago" )))  # Reordenar niveles

# Asegurarse de que "Con Internet Gratuito" aparezca primero
caracteristicas_long$tipo_internet <- factor(caracteristicas_long$tipo_internet,
                                             levels = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito" ))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(caracteristicas_long, aes(x = provincia, y = porcentaje, fill = tipo_internet)) +
  geom_bar(stat = 'identity', position = 'stack') +  # Apilar las barras
  geom_text(aes(label = round(porcentaje, 2)),
            position = position_stack(vjust = 0.4), size = 4, # Agregar los porcentajes encima de las barras
            color = "white") + # Color del texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas \n por Provincia y Acceso a Internet en Argentina (2014)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual(values = c("#63B8FF","#FFA07A", "#7CCD7C")) +  # Colores ajustados al nuevo orden
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con fondo blanco
ggsave("porcentaje_de_escuelas_por_provincia_tipo_internet_total.png", width = 8, height = 8, dpi = 300, bg = "white")

# ------------------ EXTRANJEROS -----------------

# Reestructurar el dataframe para facilitar la creación del gráfico
caracteristicas_long <- caracteristicas_internet_por_provincia_total %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_internet",
               values_to = "porcentaje") %>%
  mutate(tipo_internet = factor(tipo_internet,
                                 levels = c("porcentaje_sin_internet", "porcentaje_con_internet_gratuito", "porcentaje_con_internet_pago"),
                                 labels = c("Sin Internet", "Con Internet Gratuito", "Con Internet Pago" )))  # Reordenar niveles

# Asegurarse de que "Con Internet Gratuito" aparezca primero
caracteristicas_long$tipo_internet <- factor(caracteristicas_long$tipo_internet,
                                             levels = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito" ))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(caracteristicas_long, aes(x = provincia, y = porcentaje, fill = tipo_internet)) +
  geom_bar(stat = 'identity', position = 'stack') +  # Apilar las barras
  geom_text(aes(label = round(porcentaje, 2)),
            position = position_stack(vjust = 0.4), size = 4, # Agregar los porcentajes encima de las barras
            color = "white") + # Color del texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas con Extranjeros \n por Provincia y Acceso a Internet (2015)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual(values = c("#63B8FF","#FFA07A", "#7CCD7C")) +  # Colores ajustados al nuevo orden
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con fondo blanco
ggsave("porcentaje_de_escuelas_por_provincia_tipo_internet_extranjeros.png", width = 8, height = 8, dpi = 300, bg = "white")

# ------------------ SIN EXTRANJEROS -----------------

# Reestructurar el dataframe para facilitar la creación del gráfico
caracteristicas_long <- caracteristicas_internet_por_provincia_sin_extranjeros %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_internet",
               values_to = "porcentaje") %>%
  mutate(tipo_internet = factor(tipo_internet,
                                 levels = c("porcentaje_sin_internet", "porcentaje_con_internet_gratuito", "porcentaje_con_internet_pago"),
                                 labels = c("Sin Internet", "Con Internet Gratuito", "Con Internet Pago" )))  # Reordenar niveles

# Asegurarse de que "Con Internet Gratuito" aparezca primero
caracteristicas_long$tipo_internet <- factor(caracteristicas_long$tipo_internet,
                                             levels = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito" ))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(caracteristicas_long, aes(x = provincia, y = porcentaje, fill = tipo_internet)) +
  geom_bar(stat = 'identity', position = 'stack') +  # Apilar las barras
  geom_text(aes(label = round(porcentaje, 2)),
            position = position_stack(vjust = 0.4), size = 4, # Agregar los porcentajes encima de las barras
            color = "white") + # Color del texto
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas sin Extranjeros \n por Provincia y Acceso a Internet (2015)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual(values = c("#63B8FF","#FFA07A", "#7CCD7C")) +  # Colores ajustados al nuevo orden
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con fondo blanco
ggsave("porcentaje_de_escuelas_por_provincia_tipo_internet_sin_extranjeros.png", width = 8, height = 8, dpi = 300, bg = "white")

