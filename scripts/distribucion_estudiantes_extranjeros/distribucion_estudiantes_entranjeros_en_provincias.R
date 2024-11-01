library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

year <- 2011

####################################################################################### EXTRACCIÓN DE DATOS TABLA POBLACIÓN #######################################################################################
###################################################################################################################################################################################################################

if (year <= 2020 && year !=2016) {
  ruta_archivo_poblacion <- paste0("bd/", year, "/Poblacion.csv")
  poblacion <- read.csv(ruta_archivo_poblacion, sep = ";",fileEncoding = "ISO-8859-1")
} else {
  ruta_archivo_poblacion <- paste0("bd/", year, "/Poblacion.csv")
  poblacion <- read.csv(ruta_archivo_poblacion, sep = ";")
}

poblacion$provincia <- iconv(poblacion$provincia, from = "latin1", to = "UTF-8")

poblacion$provincia <- gsub("Ã³", "ó", poblacion$provincia)
poblacion$provincia <- gsub("Ã­", "í", poblacion$provincia)
poblacion$provincia <- gsub("Ã©", "é", poblacion$provincia)
poblacion$provincia <- gsub("Ãº", "ú", poblacion$provincia)
poblacion$provincia <- gsub("Ã±", "ñ", poblacion$provincia)
poblacion$provincia <- gsub("Ã¡", "á", poblacion$provincia)

str(poblacion)

colnames(poblacion) <- make.names(colnames(poblacion))

print(colnames(poblacion))

resultado <- poblacion %>%
  rowwise() %>% 
  mutate(
    extranjeros = sum(c_across(matches("^Bolivia...Primaria|Brasil...Primaria|Chile...Primaria|Colombia...Primaria|Ecuador...Primaria|Paraguay...Primaria|Perú...Primaria|Uruguay...Primaria|Venezuela...Primaria|Otros.países.de.América...Primaria|Europa...Primaria|Asia...Primaria|Otros...Primaria|Bolivia...Secundaria|Brasil...Secundaria|Chile...Secundaria|Colombia...Secundaria|Ecuador...Secundaria|Paraguay...Secundaria|Perú...Secundaria|Uruguay...Secundaria|Venezuela...Secundaria|Otros.países.de.América...Secundaria|Europa...Secundaria|Asia...Secundaria|Otros...Secundaria")), na.rm = TRUE),
    Bolivia=sum(c_across(c(Bolivia...Primaria, Bolivia...Secundaria)), na.rm = TRUE),
    Paraguay=sum(c_across(c(Paraguay...Primaria, Paraguay...Secundaria)), na.rm = TRUE),
    Perú=sum(c_across(c(Perú...Primaria, Perú...Secundaria)), na.rm = TRUE),
   # Venezuela=sum(c_across(c(Venezuela...Primaria, Venezuela...Secundaria)), na.rm = TRUE)
  ) %>%
  ungroup()

colnames(resultado) <- make.names(colnames(resultado))


ii <- resultado %>%
  select(ID1,provincia,extranjeros,Bolivia,Paraguay,Perú)#,Venezuela) 

write.csv(ii, "conteo_estudiantes_extranjeros_por_institucion.csv", row.names = FALSE)


##################################################################################### PROCESAMIENTO DE DATOS PARA ARMAR LOS GRÁFICOS ####################################################################################
#########################################################################################################################################################################################################################

porcentaje_nacionalidades_por_provincia_1 <- ii %>%
  group_by(provincia) %>%  
  summarise(
    total_extranjeros = sum(extranjeros, na.rm = TRUE),
    Bolivia = sum(Bolivia, na.rm = TRUE),
    Paraguay = sum(Paraguay, na.rm = TRUE),
    Perú = sum(Perú, na.rm = TRUE),
    #Venezuela = sum(Venezuela, na.rm = TRUE)
  ) %>%
  mutate(
    porcentaje_Bolivia = round((Bolivia / total_extranjeros) * 100,2),
    porcentaje_Paraguay = round((Paraguay / total_extranjeros) * 100,2),
    porcentaje_Perú = round((Perú / total_extranjeros) * 100,2),
   # porcentaje_Venezuela = round((Venezuela / total_extranjeros) * 100,2)
  )

porcentaje_nacionalidades_por_provincia_1 <- porcentaje_nacionalidades_por_provincia_1 %>%
  mutate(porcentaje_Otros = 100 - (porcentaje_Bolivia + porcentaje_Paraguay + 
                                    porcentaje_Perú),# + porcentaje_Venezuela),
         Otros = total_extranjeros - Bolivia - Perú - Paraguay)# - Venezuela)


print(porcentaje_nacionalidades_por_provincia_1)

porcentaje_nacionalidades_por_provincia <- porcentaje_nacionalidades_por_provincia_1 %>%
  select(provincia, total_extranjeros, 
         porcentaje_Bolivia, porcentaje_Paraguay, 
         porcentaje_Perú,# porcentaje_Venezuela, 
         porcentaje_Otros)


print(porcentaje_nacionalidades_por_provincia)

dir_path <- "extranjeros_por_provincia"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

write.csv(porcentaje_nacionalidades_por_provincia_1, paste0("extranjeros_por_provincia/porcentaje_extranjeros_por_provincia_",year,".csv"), row.names = FALSE)


porcentaje_largo <- porcentaje_nacionalidades_por_provincia %>%
  pivot_longer(
    cols = starts_with("porcentaje_"),
    names_to = "nacionalidad",
    values_to = "porcentaje"
  ) %>%
  mutate(nacionalidad = gsub("porcentaje_", "", nacionalidad)) %>%
  select(provincia, nacionalidad, porcentaje) 

print(porcentaje_nacionalidades_por_provincia)
print(porcentaje_largo)

provincias <- unique(porcentaje_largo$provincia)
print(provincias)

############################################################### GENERACIÓN DE GRÁFICOS PARA CADA PROVINCIA ARGENTINA SEGÚN SUS ESTUDIANTES EXTRANJEROS ##################################################################
#########################################################################################################################################################################################################################

colors <- c(
  "Bolivia" = "#4CAF50", 
  "Paraguay" = "#2196F3",
  "Perú" = "#D32F2F",
 # "Venezuela" = "#FFC107",
  "Otros" = "#B0B0B0"
)

for (provincia in provincias) {
  datos_provincia <- porcentaje_largo %>% filter(provincia == !!provincia)
  
  print(datos_provincia)
  
  p <- ggplot(datos_provincia, aes(x = nacionalidad, y = porcentaje, fill = nacionalidad)) +
    geom_bar(stat = "identity", width = 0.6) +  # ancho de barras
    geom_text(aes(label = round(porcentaje, 2)), 
              position = position_stack(vjust = 1.05),  # posicion del texto
              color = "black", vjust = -0.5) +  # texto un poco mas arriba
    scale_fill_manual(values = colors) +
    labs(title = paste("Distribución de Nacionalidades Extranjeras en", provincia, year),
         x = "Nacionalidad", y = "Porcentaje", fill = NULL) +
    theme_minimal(base_size = 14) +  # tamaño de fuente
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),  # nombres del eje x en vertical
      panel.background = element_rect(fill = "white"),  # fondo grafico
      plot.background = element_rect(fill = "white"),  # fondo imagen
      plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),  # tamaño y posicion del titulo
      axis.title = element_text(size = 10)  # tamaño de los titulos de los ejes
    )
  
  dir_path <- "extranjeros_por_provincia"
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  dir_path <- paste0("extranjeros_por_provincia/", provincia)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  ggsave(paste0("extranjeros_por_provincia/",provincia,"/nacionalidades_extranjeras_en_", provincia, "_", year, ".png"), plot = p, width = 6, height = 6)
}


############################## PROCESAMIENTO DE DATOS Y GENERACIÓN DE GRÁFICOS PARA CADA NACIONALIDAD EXTRANJERA A LO LARGO DE ARGENTINA (división en provincias) #######################################################
#########################################################################################################################################################################################################################

colores <- c(
  "Buenos Aires"  = "#D32F2F",  # Rojo 
  "Catamarca" = "#F06292",  # Rosa
  "Chaco" = "#BA68C8",  # Morado
  "Chubut" = "#7986CB",  # Azul
  "Ciudad de Buenos Aires" = "#4FC3F7",  # Cian
  "Corrientes" = "#64B5F6",  # Azul
  "Córdoba" = "#81C784",  # Verde claro
  "Entre Ríos" = "#E0E0E0",  # Gris claro
  "Formosa" = "#FFB74D",  # Naranja claro
  "Jujuy" = "#FF8A65",  # Coral
  "La Pampa" = "#A1887F",  # Marrón
  "Otras" = "#BDBDBD",  # Gris
  "La Rioja" = "#90A4AE",  # Azul grisáceo
  "Mendoza" = "#F57C00",  # Naranja
  "Misiones" = "#FFF176",  # Amarillo claro
  "Neuquén" = "#4DB6AC",  # Verde azulado
  "Río Negro" = "#E57373",  # Rojo claro
  "Salta" = "#64B5F6",  # Azul claro
  "San Juan" = "#B39DDB",  # Lila
  "San Luis" = "#FBC02D",  # Amarillo
  "Santa Cruz" = "#FFAB91",  # Naranja claro
  "Santa Fe" = "#9575CD",  # Morado
  "Santiago del Estero" = "#1976D2",  # Azul oscuro
  "Tierra del Fuego" = "#388E3C",  # Verde
  "Tucumán" = "#C2185B"   # Rosa fuerte
)

dir_path <- "distribucion"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- paste0("distribucion/", as.character(year))
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

nacionalidades_extranjeros <- c("Bolivia","Paraguay","Perú")#,"Venezuela")

for (nacion in nacionalidades_extranjeros) {

total_nacionalidad_extranjera_por_provincia <- porcentaje_nacionalidades_por_provincia_1 %>%
  group_by(provincia) %>%
  summarise(total_nacionalidad_extranjera = sum(across(all_of(nacion)), na.rm = TRUE))

total_nacionalidad_extranjera_en_argentina <- sum(total_nacionalidad_extranjera_por_provincia$total_nacionalidad_extranjera, na.rm = TRUE)

porcentaje_nacionalidad_extranjera_por_provincia <- total_nacionalidad_extranjera_por_provincia %>%
  mutate(porcentaje = (total_nacionalidad_extranjera / total_nacionalidad_extranjera_en_argentina) * 100)

porcentaje_nacionalidad_extranjera_por_provincia <- porcentaje_nacionalidad_extranjera_por_provincia %>%
  mutate(porcentaje = round(porcentaje, 2))

print(porcentaje_nacionalidad_extranjera_por_provincia)

write.csv(porcentaje_nacionalidad_extranjera_por_provincia, paste0("porcentaje_nacionalidad_extranjera_",nacion,"_","por_provincia_",year,".csv"), row.names = FALSE)

top_5_provincias <- porcentaje_nacionalidad_extranjera_por_provincia %>%
  arrange(desc(porcentaje)) %>%
  slice_head(n = 5)

otros_porcentaje <- porcentaje_nacionalidad_extranjera_por_provincia %>%
  filter(!provincia %in% top_5_provincias$provincia) %>%
  summarise(porcentaje = sum(porcentaje), total_nacionalidad_extranjera=sum(total_nacionalidad_extranjera))

resultado_final <- top_5_provincias %>%
  bind_rows(data.frame(provincia = "Otras", porcentaje = otros_porcentaje$porcentaje, total_nacionalidad_extranjera=otros_porcentaje$total_nacionalidad_extranjera))

print(resultado_final)

nombres_provincias <- resultado_final$provincia

colores_asignados <- colores[nombres_provincias]


ggplot(resultado_final, aes(x = reorder(provincia, -porcentaje), y = porcentaje, fill = provincia)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_asignados) +
  labs(title = "Distribución de Porcentajes por Provincia",
       x = "Provincia",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafico <- ggplot(resultado_final, aes(x = reorder(provincia, -porcentaje), y = porcentaje, fill = provincia)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(porcentaje, 2)), 
            vjust = -0.5,  # posicion del texto
            color = "black") +
  labs(title = paste("Distribución de Nacionalidades Extranjeras en Argentina ", year, ":", nacion),
       x = "Provincia", y = "Porcentaje",fill = NULL) +
  scale_fill_manual(values = colores_asignados) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # rotar etiquetas del eje x
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  ylim(0, max(resultado_final$porcentaje) + 5) +  # limite superior del eje y
  theme(plot.title = element_text(size = 16))

print(grafico)

ggsave(paste0("distribucion/",year,"/distribucion_",nacion,"_en_el_pais_",year,".png"), width = 10, height = 6)

}
