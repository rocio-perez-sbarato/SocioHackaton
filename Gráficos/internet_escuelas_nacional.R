# Definir las columnas que contienen información sobre Internet
columnas_internet_pago_gratis <- c(
  'id',
  'provincia',
  'sector',
  'ambito',
  'Internet...Tipo.de.servicio...Gratuito',
  'Internet...Tipo.de.servicio...Pago'
)

# Nota: En la base de 2023, hay que incorporar las columnas 
# 'Internet...Tipo.de.servicio...Gratuito.Otro' e 
# 'Internet...Tipo.de.servicio...Gratuito.Estado'.

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
