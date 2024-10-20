
library(ggplot2)
library(dplyr)
library(tidyr)

# Reestructurar el dataframe para facilitar la creación del gráfico
características_long <- características_internet_por_provincia_total %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_internet",
               values_to = "porcentaje") %>%
  mutate(tipo_internet = factor(tipo_internet,
                                 levels = c("porcentaje_sin_internet", "porcentaje_con_internet_gratuito", "porcentaje_con_internet_pago"),
                                 labels = c("Sin Internet", "Con Internet Gratuito", "Con Internet Pago" )))  # Reordenar niveles

# Asegurarse de que "Con Internet Gratuito" aparezca primero
características_long$tipo_internet <- factor(características_long$tipo_internet,
                                             levels = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito" ))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(características_long, aes(x = provincia, y = porcentaje, fill = tipo_internet)) +
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
ggsave("porcentaje_de_escuelas_por_provincia_tipo_internet_total_2014.png", width = 8, height = 8, dpi = 300, bg = "white")

# Reestructurar el dataframe para facilitar la creación del gráfico
características_long <- características_internet_por_provincia_sin_extranjeros %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_internet",
               values_to = "porcentaje") %>%
  mutate(tipo_internet = factor(tipo_internet,
                                 levels = c("porcentaje_sin_internet", "porcentaje_con_internet_gratuito", "porcentaje_con_internet_pago"),
                                 labels = c("Sin Internet", "Con Internet Gratuito", "Con Internet Pago" )))  # Reordenar niveles

# Asegurarse de que "Con Internet Gratuito" aparezca primero
características_long$tipo_internet <- factor(características_long$tipo_internet,
                                             levels = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito" ))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(características_long, aes(x = provincia, y = porcentaje, fill = tipo_internet)) +
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
ggsave("porcentaje_de_escuelas_por_provincia_tipo_internet_sin_extranjeros_2015.png", width = 8, height = 8, dpi = 300, bg = "white")

# Reestructurar el dataframe para facilitar la creación del gráfico
características_long <- características_internet_por_provincia_total %>%
  select(provincia, starts_with("porcentaje_")) %>%  # Seleccionar solo las columnas que empiezan con 'porcentaje_'
  pivot_longer(cols = starts_with("porcentaje_"),
               names_to = "tipo_internet",
               values_to = "porcentaje") %>%
  mutate(tipo_internet = factor(tipo_internet,
                                 levels = c("porcentaje_sin_internet", "porcentaje_con_internet_gratuito", "porcentaje_con_internet_pago"),
                                 labels = c("Sin Internet", "Con Internet Gratuito", "Con Internet Pago" )))  # Reordenar niveles

# Asegurarse de que "Con Internet Gratuito" aparezca primero
características_long$tipo_internet <- factor(características_long$tipo_internet,
                                             levels = c("Con Internet Pago", "Sin Internet", "Con Internet Gratuito" ))

# Crear el gráfico de barras apiladas en orientación horizontal
ggplot(características_long, aes(x = provincia, y = porcentaje, fill = tipo_internet)) +
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
ggsave("porcentaje_de_escuelas_por_provincia_tipo_internet_extranjeros_2015.png", width = 8, height = 8, dpi = 300, bg = "white")

# Nota: Asegúrese de actualizar los títulos de los gráficos de acuerdo al año correspondiente.
