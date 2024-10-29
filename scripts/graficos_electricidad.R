######################################################################
# Análisis de acceso a electricidad por escuelas primarias           #
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

# ---------------------------------------------------------
# Nota: No hicimos gráficos de estos resultados porque 
# la gran mayoría de las escuelas tienen electricidad 
# con red pública.
# ---------------------------------------------------------

# ================================================================
# TOTAL
# ================================================================

# Crear un nuevo dataframe solo con las columnas de electricidad
características_electricidad_nacional <- características %>%  # Cambiar a tu dataframe sin extranjeros
  mutate(total_electricidad = rowSums(select(., 'Electricidad...Si') == "X", na.rm = TRUE),
         total_electricidad_red_publica = rowSums(select(., 'Electricidad...Red.pública') == "X", na.rm = TRUE),
         total_electricidad_otros = rowSums(select(.,
                                                  'Electricidad...Grupo.electrógeno',
                                                  'Electricidad...Panel.fotovoltaico.solar',
                                                  'Electricidad...Generador.eólico',
                                                  'Electricidad...Generador.hidráulico',
                                                  'Electricidad...Otro') == "X",
                                             na.rm = TRUE)) %>%
  # Calcular las cantidades
  summarise(
    cantidad_escuelas_con_electricidad_red_publica = sum(total_electricidad_red_publica > 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_otros = sum(total_electricidad_otros > 0 & total_electricidad_red_publica == 0, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_electricidad_red_publica > 0 & total_electricidad_otros > 0, na.rm = TRUE),
    cantidad_escuelas_sin_electricidad = sum(total_electricidad_red_publica == 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_sin_duplicados = sum(total_electricidad > 0, na.rm = TRUE) - cantidad_escuelas_con_ambas ,
    cantidad_escuelas_sin_duplicados = nrow(características) - cantidad_escuelas_con_ambas
  )  %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_electricidad_red_publica = round((cantidad_escuelas_con_electricidad_red_publica / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad_otros = round((cantidad_escuelas_con_electricidad_otros / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_sin_electricidad = round((cantidad_escuelas_sin_electricidad / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad = round((cantidad_escuelas_con_electricidad_sin_duplicados / cantidad_escuelas_sin_duplicados) * 100, 2)
  )

# ================================================================
# EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas de electricidad
características_electricidad_nacional_extranjeros <- características_extranjeros %>%
  mutate(total_electricidad = rowSums(select(., 'ElectricidadSi') == "X", na.rm = TRUE),
         total_electricidad_red_publica = rowSums(select(., 'ElectricidadRedpública') == "X", na.rm = TRUE),
         total_electricidad_otros = rowSums(select(.,
                                                  'ElectricidadGrupoelectrógeno',
                                                  'ElectricidadPanelfotovoltaicosolar',
                                                  'ElectricidadGeneradoreólico',
                                                  'ElectricidadGeneradorhidráulico',
                                                  'ElectricidadOtro') == "X",
                                             na.rm = TRUE)) %>%
  # Calcular las cantidades
  summarise(
    cantidad_escuelas_con_electricidad_red_publica = sum(total_electricidad_red_publica > 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_otros = sum(total_electricidad_otros > 0 & total_electricidad_red_publica == 0, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_electricidad_red_publica > 0 & total_electricidad_otros > 0, na.rm = TRUE),
    cantidad_escuelas_sin_electricidad = sum(total_electricidad_red_publica == 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_sin_duplicados = sum(total_electricidad > 0, na.rm = TRUE) - cantidad_escuelas_con_ambas ,
    cantidad_escuelas_sin_duplicados = nrow(características_electricidad_extranjeros) - cantidad_escuelas_con_ambas
  )  %>%
  # Calcular los porcentajes
  mutate(
    porcentaje_escuelas_con_electricidad_red_publica = round((cantidad_escuelas_con_electricidad_red_publica / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad_otros = round((cantidad_escuelas_con_electricidad_otros / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_sin_electricidad = round((cantidad_escuelas_sin_electricidad / cantidad_escuelas_sin_duplicados) * 100, 2),
    porcentaje_escuelas_con_electricidad = round((cantidad_escuelas_con_electricidad_sin_duplicados / cantidad_escuelas_sin_duplicados) * 100, 2)
  )


# ================================================================
# SIN EXTRANJEROS
# ================================================================

# Crear un nuevo dataframe solo con las columnas de electricidad
características_electricidad_nacional_sin_extranjeros <- características_sin_extranjeros %>%
  mutate(total_electricidad = rowSums(select(., 'Electricidad...Si') == "X", na.rm = TRUE),
         total_electricidad_red_publica = rowSums(select(., 'Electricidad...Red.pública') == "X", na.rm = TRUE),
         total_electricidad_otros = rowSums(select(.,
                                                  'Electricidad...Grupo.electrógeno',
                                                  'Electricidad...Panel.fotovoltaico.solar',
                                                  'Electricidad...Generador.eólico',
                                                  'Electricidad...Generador.hidráulico',
                                                  'Electricidad...Otro') == "X",
                                             na.rm = TRUE)) %>%
  # Calcular las cantidades
  summarise(
    cantidad_escuelas_con_electricidad_red_publica = sum(total_electricidad_red_publica > 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_otros = sum(total_electricidad_otros > 0 & total_electricidad_red_publica == 0, na.rm = TRUE),
    cantidad_escuelas_con_ambas = sum(total_electricidad_red_publica > 0 & total_electricidad_otros > 0, na.rm = TRUE),
    cantidad_escuelas_sin_electricidad = sum(total_electricidad_red_publica == 0 & total_electricidad_otros == 0, na.rm = TRUE),
    cantidad_escuelas_con_electricidad_sin_duplicados = sum(total_electricidad > 0, na.rm = TRUE) - cantidad_escuelas_con_ambas ,
    cantidad_escuelas_sin_duplicados = nrow(características_electricidad_sin_extranjeros) - cantidad_escuelas_con_ambas
  )  %>%
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
head(características_electricidad_nacional_sin_extranjeros)
head(características_electricidad_nacional_extranjeros)
head(características_electricidad_nacional)

# Guardar las tablas en archivos .csv
write.csv(características_electricidad_nacional_sin_extranjeros, "características_electricidad_nacional_sin_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(características_electricidad_nacional, "características_electricidad_nacional.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(características_electricidad_nacional_extranjeros, "características_electricidad_nacional_extranjeros.csv", row.names = FALSE, fileEncoding = "UTF-8")

