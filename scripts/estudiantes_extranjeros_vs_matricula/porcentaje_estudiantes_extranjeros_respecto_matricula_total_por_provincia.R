library(dplyr)
library(readr)
library(ggplot2)

provincias_X6 <- c("Buenos Aires", "Catamarca", "Córdoba", "Corrientes", "Chubut", "Entre Ríos", "Formosa", "La Pampa", "San Juan", "San Luis", "Tierra del Fuego", "Tucumán")

year <- 2023

####################################################################################### EXTRACCIÓN DE DATOS TABLA MATRÍCULA #######################################################################################
###################################################################################################################################################################################################################

if (year <= 2020) {
  ruta_archivo <- paste0("bd/", year, "/Matricula.csv")
  matricula <- read.csv(ruta_archivo, sep = ";",fileEncoding = "ISO-8859-1")
} else {
  ruta_archivo <- paste0("bd/", year, "/Matricula.csv")
  matricula <- read.csv(ruta_archivo, sep = ";")
}

matricula$provincia <- iconv(matricula$provincia, from = "latin1", to = "UTF-8")

matricula$provincia <- gsub("Ã³", "ó", matricula$provincia)
matricula$provincia <- gsub("Ã­", "í", matricula$provincia)
matricula$provincia <- gsub("Ã©", "é", matricula$provincia)
matricula$provincia <- gsub("Ãº", "ú", matricula$provincia)
matricula$provincia <- gsub("Ã±", "ñ", matricula$provincia)
matricula$provincia <- gsub("Ã¡", "á", matricula$provincia)

str(matricula)

colnames(matricula) <- make.names(colnames(matricula))

print(colnames(matricula))

res <- matricula %>%
  rowwise() %>%  
  mutate(
    total_alumnos = sum(c_across(c(X_1, X_2, X_3, X_4, X_5, X_6, X_7, X_8,
                                   X_9, X_10, X_11, X_12, X_1314)), na.rm = TRUE),
    
    total_alumnos_primaria = if_else(provincia %in% provincias_X6, 
                                     sum(c_across(c(X_1, X_2, X_3, X_4, X_5, X_6)), na.rm = TRUE), 
                                     sum(c_across(c(X_1, X_2, X_3, X_4, X_5, X_6, X_7)), na.rm = TRUE)
    ),
    
    total_alumnos_secundaria = if_else(provincia %in% provincias_X6, 
                                       sum(c_across(c(X_7, X_8, X_9, X_10, X_11, X_12, X_1314)), na.rm = TRUE), 
                                       sum(c_across(c(X_8, X_9, X_10, X_11, X_12, X_1314)), na.rm = TRUE)
    )
  )


rt <- res %>%
  filter(total_alumnos > 0) %>% 
  select(ID1,provincia,total_alumnos,total_alumnos_primaria,total_alumnos_secundaria)


print(rt)
write.csv(rt, file = "resultado_matricula.csv", row.names = FALSE)  

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

poblacion <- poblacion %>%
  inner_join(matricula, by = "ID1")

resultado <- poblacion %>%
  rowwise() %>% 
  mutate(
    extranjeros = sum(c_across(matches("^Bolivia...Primaria|Brasil...Primaria|Chile...Primaria|Colombia...Primaria|Ecuador...Primaria|Paraguay...Primaria|Perú...Primaria|Uruguay...Primaria|Venezuela...Primaria|Otros.países.de.América...Primaria|Europa...Primaria|Asia...Primaria|Otros...Primaria|Bolivia...Secundaria|Brasil...Secundaria|Chile...Secundaria|Colombia...Secundaria|Ecuador...Secundaria|Paraguay...Secundaria|Perú...Secundaria|Uruguay...Secundaria|Venezuela...Secundaria|Otros.países.de.América...Secundaria|Europa...Secundaria|Asia...Secundaria|Otros...Secundaria")), na.rm = TRUE),
    extranjeros_primaria = sum(c_across(matches("^Bolivia...Primaria|Brasil...Primaria|Chile...Primaria|Colombia...Primaria|Ecuador...Primaria|Paraguay...Primaria|Perú...Primaria|Uruguay...Primaria|Venezuela...Primaria|Otros.países.de.América...Primaria|Europa...Primaria|Asia...Primaria|Otros...Primaria")), na.rm = TRUE),
    extranjeros_secundaria = sum(c_across(matches("^Bolivia...Secundaria|Brasil...Secundaria|Chile...Secundaria|Colombia...Secundaria|Ecuador...Secundaria|Paraguay...Secundaria|Perú...Secundaria|Uruguay...Secundaria|Venezuela...Secundaria|Otros.países.de.América...Secundaria|Europa...Secundaria|Asia...Secundaria|Otros...Secundaria")), na.rm = TRUE),
  )

print(resultado)

ii <- resultado %>%
  select(ID1, extranjeros,extranjeros_primaria,extranjeros_secundaria)

write.csv(ii, "resultado_poblacion.csv", row.names = FALSE)

##################################################################################### PROCESAMIENTO DE DATOS PARA ARMAR LOS GRÁFICOS ####################################################################################
#########################################################################################################################################################################################################################

resultado_final <- ii %>%
  inner_join(rt, by = "ID1")

write.csv(resultado_final, "poblacion_extranjera_total_prov.cvs", row.names = FALSE)


poblacion_porcentaje <- resultado_final %>%
  group_by(provincia) %>%  
  summarise(
    total_alumnos = sum(total_alumnos, na.rm = TRUE),
    total_alumnos_primaria = sum(total_alumnos_primaria, na.rm = TRUE),
    total_alumnos_secundaria = sum(total_alumnos_secundaria, na.rm = TRUE),
    total_extranjeros = sum(extranjeros, na.rm = TRUE),
    total_extranjeros_primaria = sum(extranjeros_primaria, na.rm = TRUE),
    total_extranjeros_secundaria = sum(extranjeros_secundaria, na.rm = TRUE),
  ) %>%
  mutate(
    porcentaje_extranjeros = (total_extranjeros / total_alumnos) * 100,
    porcentaje_extranjeros_primaria = (total_extranjeros_primaria / total_alumnos_primaria) * 100,
    porcentaje_extranjeros_secundaria = (total_extranjeros_secundaria / total_alumnos_secundaria) * 100
  )

print(poblacion_porcentaje)

write.csv(poblacion_porcentaje, "poblacion_porcentaje.cvs", row.names = FALSE)

################################# GENERACIÓN DE GRÁFICOS PARA CADA NIVEL EDUCATIVO POR PROVINCIA SEGÚN CANTIDAD DE ESTUDIANTES EXTRANJEROS CON RESPECTO A LA MATRÍCULA TOTAL DE ALUMNOS #################################
#########################################################################################################################################################################################################################

grafico <- ggplot(poblacion_porcentaje, aes(x = reorder(provincia, -porcentaje_extranjeros), y = porcentaje_extranjeros)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = round(porcentaje_extranjeros, 2)), vjust = -0.5) +
  labs(title = paste0("Porcentaje de Alumnos Extranjeros por Provincia ", year),
       x = "Provincia",
       y = "Porcentaje de Extranjeros (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dir_path <- "porcentaje_alumnos_extranjeros_por_provincia"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

ggsave(paste0("porcentaje_alumnos_extranjeros_por_provincia/porcentaje_alumnos_extranjeros_respecto_del_total_",year,".png"), plot = grafico, width = 10, height = 8)

grafico_primaria <- ggplot(poblacion_porcentaje, aes(x = reorder(provincia, -porcentaje_extranjeros_primaria), y = porcentaje_extranjeros_primaria)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = round(porcentaje_extranjeros_primaria, 2)), vjust = -0.5) +
  labs(title = paste0("Porcentaje de Alumnos Extranjeros en Primaria por Provincia ", year),
       x = "Provincia",
       y = "Porcentaje de Extranjeros (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dir_path <- "porcentaje_alumnos_extranjeros_por_provincia/primaria"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

ggsave(paste0("porcentaje_alumnos_extranjeros_por_provincia/primaria/porcentaje_alumnos_extranjeros_respecto_del_total_primaria_",year,".png"), plot = grafico_primaria, width = 10, height = 8)

grafico_secundaria <- ggplot(poblacion_porcentaje, aes(x = reorder(provincia, -porcentaje_extranjeros_secundaria), y = porcentaje_extranjeros_secundaria)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = round(porcentaje_extranjeros_secundaria, 2)), vjust = -0.5) +
  labs(title = paste0("Porcentaje de Alumnos Extranjeros en Secundaria por Provincia ", year),
       x = "Provincia",
       y = "Porcentaje de Extranjeros (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dir_path <- "porcentaje_alumnos_extranjeros_por_provincia/secundaria"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

ggsave(paste0("porcentaje_alumnos_extranjeros_por_provincia/secundaria/porcentaje_alumnos_extranjeros_respecto_del_total_secundaria_",year,".png"), plot = grafico_secundaria, width = 10, height = 8)
