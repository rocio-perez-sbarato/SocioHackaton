# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# ================================================================
# PROVINCIAL: TOTAL
# ================================================================

# Crear un nuevo dataframe solo con las columnas de electricidad
características_electricidad_por_provincia <- características %>%
  mutate(total_electricidad = rowSums(select(., 'Electricidad...Si') == "X", na.rm = TRUE),
         total_electricidad_red_publica = rowSums(select(., 'Electricidad...Red.pública') == "X", na.rm = TRUE),
         total_electricidad_otros = rowSums(select(.,
                                                  'Electricidad...Grupo.electrógeno',
                                                  'Electricidad...Panel.fotovoltaico.solar',
                                                  'Electricidad...Generador.eólico',
                                                  'Electricidad...Generador.hidráulico',
                                                  'Electricidad...Otro') == "X",
                                             na.rm = TRUE)) %>%
  # Calcular las cantidades por provincia
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_con_electricidad_red_publica = sum(total_electricidad_red_publica > 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_otros = sum(total_electricidad_otros > 0 & total_electricidad_red_publica == 0, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_electricidad_red_publica > 0 & total_electricidad_otros > 0, na.rm = TRUE),
    cantidad_escuelas_sin_electricidad = sum(total_electricidad_red_publica == 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_sin_duplicados = sum(total_electricidad > 0, na.rm = TRUE) - cantidad_escuelas_con_ambas ,
    cantidad_escuelas_sin_duplicados = n() - cantidad_escuelas_con_ambas
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_electricidad_red_publica = round((cantidad_escuelas_con_electricidad_red_publica / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad_otros = round((cantidad_escuelas_con_electricidad_otros / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_sin_electricidad = round((cantidad_escuelas_sin_electricidad / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad = round((cantidad_escuelas_con_electricidad_sin_duplicados / cantidad_escuelas_sin_duplicados) * 100, 2)
  )

# ================================================================
# PROVINCIAL: SIN EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas de electricidad
características_electricidad_por_provincia_sin_extranjeros <- características_sin_extranjeros %>%
  mutate(total_electricidad = rowSums(select(., 'Electricidad...Si') == "X", na.rm = TRUE),
         total_electricidad_red_publica = rowSums(select(., 'Electricidad...Red.pública') == "X", na.rm = TRUE),
         total_electricidad_otros = rowSums(select(.,
                                                  'Electricidad...Grupo.electrógeno',
                                                  'Electricidad...Panel.fotovoltaico.solar',
                                                  'Electricidad...Generador.eólico',
                                                  'Electricidad...Generador.hidráulico',
                                                  'Electricidad...Otro') == "X",
                                             na.rm = TRUE)) %>%
  # Calcular las cantidades por provincia
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_con_electricidad_red_publica = sum(total_electricidad_red_publica > 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_otros = sum(total_electricidad_otros > 0 & total_electricidad_red_publica == 0, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_electricidad_red_publica > 0 & total_electricidad_otros > 0, na.rm = TRUE),
    cantidad_escuelas_sin_electricidad = sum(total_electricidad_red_publica == 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_sin_duplicados = sum(total_electricidad > 0, na.rm = TRUE) - cantidad_escuelas_con_ambas ,
    cantidad_escuelas_sin_duplicados = n() - cantidad_escuelas_con_ambas
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_electricidad_red_publica = round((cantidad_escuelas_con_electricidad_red_publica / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad_otros = round((cantidad_escuelas_con_electricidad_otros / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_sin_electricidad = round((cantidad_escuelas_sin_electricidad / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad = round((cantidad_escuelas_con_electricidad_sin_duplicados / cantidad_escuelas_sin_duplicados) * 100, 2)
  )

# ================================================================
# PROVINCIAL: EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas de electricidad
características_electricidad_por_provincia_extranjeros <- características_extranjeros %>%
  mutate(total_electricidad = rowSums(select(., 'ElectricidadSi') == "X", na.rm = TRUE),
         total_electricidad_red_publica = rowSums(select(., 'ElectricidadRedpública') == "X", na.rm = TRUE),
         total_electricidad_otros = rowSums(select(.,
                                                  'ElectricidadGrupoelectrógeno',
                                                  'ElectricidadPanelfotovoltaicosolar',
                                                  'ElectricidadGeneradoreólico',
                                                  'ElectricidadGeneradorhidráulico',
                                                  'ElectricidadOtro') == "X",
                                             na.rm = TRUE))  %>%
  # Calcular las cantidades por provincia
  group_by(provincia) %>%
  summarise(
    cantidad_escuelas_con_electricidad_red_publica = sum(total_electricidad_red_publica > 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_otros = sum(total_electricidad_otros > 0 & total_electricidad_red_publica == 0, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_electricidad_red_publica > 0 & total_electricidad_otros > 0, na.rm = TRUE),
    cantidad_escuelas_sin_electricidad = sum(total_electricidad_red_publica == 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_sin_duplicados = sum(total_electricidad > 0, na.rm = TRUE) - cantidad_escuelas_con_ambas ,
    cantidad_escuelas_sin_duplicados = n() - cantidad_escuelas_con_ambas
  ) %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_electricidad_red_publica = round((cantidad_escuelas_con_electricidad_red_publica / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad_otros = round((cantidad_escuelas_con_electricidad_otros / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_sin_electricidad = round((cantidad_escuelas_sin_electricidad / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad = round((cantidad_escuelas_con_electricidad_sin_duplicados / cantidad_escuelas_sin_duplicados) * 100, 2)
  )


# ================================================================
# MOSTRAR RESULTADOS Y GUARDAR CSV
# ================================================================

# Mostrar los resultados
head(características_electricidad_por_provincia)
head(características_electricidad_por_provincia_extranjeros)
head(características_electricidad_por_provincia_sin_extranjeros)

# Guardar las tablas en archivos .csv
write.csv(características_electricidad_por_provincia, "características_electricidad_por_provincia.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(características_electricidad_por_provincia_extranjeros, "características_electricidad_por_provincia_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(características_electricidad_por_provincia_sin_extranjeros, "características_electricidad_por_provincia_sin_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ================================================================
# GRÁFICOS
# ================================================================

# ---------------------------------------------------------
# Nota: Actualizar los títulos de acuerdo al año
# ---------------------------------------------------------

# ------------------ TOTAL -----------------

características_seleccionadas <- características_electricidad_por_provincia %>%
  select(provincia,
         porcentaje_escuelas_con_electricidad_red_publica,
         porcentaje_escuelas_con_electricidad_otros,
         porcentaje_escuelas_sin_electricidad)


# Reestructurar el dataframe para facilitar la creación del gráfico
características_long <- características_seleccionadas %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_electricidad",
               values_to = "porcentaje") %>%
  mutate(tipo_electricidad = factor(tipo_electricidad,
                                    levels = c("porcentaje_escuelas_sin_electricidad",
                                                "porcentaje_escuelas_con_electricidad_otros",
                                                "porcentaje_escuelas_con_electricidad_red_publica"
                                               ),
                                    labels = c("Sin Electricidad", "Otros Tipos de Electricidad", "Con Red Pública")))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(características_long, aes(x = provincia, y = porcentaje, fill = tipo_electricidad)) +
  geom_bar(stat = 'identity', position = 'stack') +  # Apilar las barras
  geom_text(aes(label = sprintf("%.2f", porcentaje)),
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +  # Añadir etiquetas de porcentaje en blanco
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas por Provincia y  \n Acceso a Electricidad en Argentina (2021)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual(values = c("#CD3278", "#4682B4", "#3CB371")) +  # Asegurar que "Con Red Pública" tiene el color deseado
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_tipo_electricidad_2021.png", width = 8, height = 8, dpi = 300, bg = "white")


# ------------------ SIN EXTRANJEROS -----------------

características_seleccionadas <- características_electricidad_por_provincia_sin_extranjeros %>%
  select(provincia,
         porcentaje_escuelas_con_electricidad_red_publica,
         porcentaje_escuelas_con_electricidad_otros,
         porcentaje_escuelas_sin_electricidad)

# Reestructurar el dataframe para facilitar la creación del gráfico
características_long <- características_seleccionadas %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_electricidad",
               values_to = "porcentaje") %>%
  mutate(tipo_electricidad = factor(tipo_electricidad,
                                    levels = c("porcentaje_escuelas_sin_electricidad",
                                                "porcentaje_escuelas_con_electricidad_otros",
                                                "porcentaje_escuelas_con_electricidad_red_publica"
                                               ),
                                    labels = c("Sin Electricidad", "Otros Tipos de Electricidad", "Con Red Pública")))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(características_long, aes(x = provincia, y = porcentaje, fill = tipo_electricidad)) +
  geom_bar(stat = 'identity', position = 'stack') +  # Apilar las barras
  geom_text(aes(label = sprintf("%.2f", porcentaje)),
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +  # Añadir etiquetas de porcentaje en blanco
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas sin Extranjeros \n por Provincia y Acceso a Electricidad en Argentina (2021)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual(values = c("#CD3278", "#4682B4", "#3CB371")) +  # Asegurar que "Con Red Pública" tiene el color deseado
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_tipo_electricidad_sin_extranjeros_2021.png", width = 8, height = 8, dpi = 300, bg = "white")


# ------------------ EXTRANJEROS -----------------

características_seleccionadas <- características_electricidad_por_provincia_extranjeros %>%
  select(provincia,
         porcentaje_escuelas_con_electricidad_red_publica,
         porcentaje_escuelas_con_electricidad_otros,
         porcentaje_escuelas_sin_electricidad)

# Reestructurar el dataframe para facilitar la creación del gráfico
características_long <- características_seleccionadas %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_electricidad",
               values_to = "porcentaje") %>%
  mutate(tipo_electricidad = factor(tipo_electricidad,
                                    levels = c("porcentaje_escuelas_sin_electricidad",
                                                "porcentaje_escuelas_con_electricidad_otros",
                                                "porcentaje_escuelas_con_electricidad_red_publica"
                                               ),
                                    labels = c("Sin Electricidad", "Otros Tipos de Electricidad", "Con Red Pública")))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(características_long, aes(x = provincia, y = porcentaje, fill = tipo_electricidad)) +
  geom_bar(stat = 'identity', position = 'stack') +  # Apilar las barras
  geom_text(aes(label = sprintf("%.2f", porcentaje)),
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +  # Añadir etiquetas de porcentaje en blanco
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = 'Porcentaje del Total de Escuelas con Extranjeros \n por Provincia y Acceso a Electricidad en Argentina (2021)') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual(values = c("#CD3278", "#4682B4", "#3CB371")) +  # Asegurar que "Con Red Pública" tiene el color deseado
  coord_flip()  # Voltear el gráfico para que sea horizontal

# Guardar el gráfico con ggsave
ggsave("porcentaje_de_escuelas_por_provincia_tipo_electricidad_extranjeros_2021.png", width = 8, height = 8, dpi = 300, bg = "white")