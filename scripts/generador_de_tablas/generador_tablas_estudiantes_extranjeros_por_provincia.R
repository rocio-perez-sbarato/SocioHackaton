library(gt)
library(dplyr)

year <- 2023

datos <- read.csv(paste0("extranjeros_por_provincia/porcentaje_extranjeros_por_provincia_",year,".csv"),fileEncoding = "UTF-8")

datos <- datos %>%
  select(
    provincia, 
    total_extranjeros, 
    Bolivia, 
    Paraguay,
    Perú,
    Venezuela,
    Otros,
    porcentaje_Bolivia, 
    porcentaje_Paraguay, 
    porcentaje_Perú,
    porcentaje_Venezuela,
    porcentaje_Otros,
  )

tabla <- datos %>%
  gt() %>%
  tab_header(
    title = "Distribución de Nacionalidades Extranjeras por Provincia",
    subtitle = paste("Datos del año", year)
  ) %>%
  fmt_number(
    columns = starts_with("porcentaje_"),
    decimals = 2
  ) %>%
  cols_label(
    provincia = "Provincia",
    total_extranjeros = "Total Extranjeros",
    Bolivia = "Bolivia",
    Paraguay = "Paraguay",
    Perú = "Perú",
    porcentaje_Bolivia = "Porcentaje Bolivia",
    porcentaje_Paraguay = "Porcentaje Paraguay",
    porcentaje_Venezuela = "Porcentaje Venezuela",
    porcentaje_Perú = "Porcentaje Perú",
    porcentaje_Otros = "Porcentaje Otros"
  ) %>%
  tab_options(
    table.width = pct(100),  # tabla al 100% del espacio
    column_labels.font.size = 12,
    column_labels.font.weight = "bold",
    table.border.top.width = px(2), 
    table.border.bottom.width = px(2),
    heading.align = "center"
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black", weight = px(1)),  # bordes entre celdas
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  )

print(tabla)

gtsave(tabla, filename = paste0("extranjeros_por_provincia/tabla_distribucion_nacionalidades_",year,".png"),expand = 20, vwidth = 1200, vheight = 1200)
