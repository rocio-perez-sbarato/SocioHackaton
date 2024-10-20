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
