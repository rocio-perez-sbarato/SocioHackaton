library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

provincias_X6 <- c("Buenos Aires", "Catamarca", "Córdoba", "Corrientes", "Chubut", "Entre Ríos", "Formosa", "La Pampa", "San Juan", "San Luis", "Tierra del Fuego", "Tucumán")

year <- 2023

####################################################################################### EXTRACCIÓN DE DATOS TABLA MATRÍCULA #######################################################################################
###################################################################################################################################################################################################################

#################################################################################################### PRIMARIO ##################################################################################################### 

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

conteo_alumnos_primaria <- matricula %>%
  rowwise() %>%
  mutate(
    total_alumnos = if (provincia %in% provincias_X6) {
      sum(c_across(c(X_1, X_2, X_3, X_4, X_5, X_6)), na.rm = TRUE)
    } else {
      sum(c_across(c(X_1, X_2, X_3, X_4, X_5, X_6, X_7)), na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  filter(total_alumnos > 0) %>%
  distinct(ID1, .keep_all = TRUE) %>% 
  select(ID1, total_alumnos, provincia)

cantidad_total_de_escuelas_primarias <- conteo_alumnos_primaria %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total
print(cantidad_total_de_escuelas_primarias)

print(conteo_alumnos_primaria)
write.csv(conteo_alumnos_primaria, file = "conteo_alumnos_primaria.csv", row.names = FALSE)

#################################################################################################### SECUNDARIO ##################################################################################################### 

conteo_alumnos_secundario <- matricula %>%
  rowwise() %>%
  mutate(
    total_alumnos = if (provincia %in% provincias_X6) {
      sum(c_across(c(X_7, X_8, X_9, X_10, X_11, X_12, X_1314)), na.rm = TRUE)
    } else {
      sum(c_across(c(X_8, X_9, X_10, X_11, X_12, X_1314)), na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  filter(total_alumnos > 0) %>%
  distinct(ID1, .keep_all = TRUE) %>% 
  select(ID1, total_alumnos, provincia)

cantidad_total_de_escuelas_secundarias <- conteo_alumnos_secundario %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total
print(cantidad_total_de_escuelas_secundarias)

print(conteo_alumnos_secundario)
write.csv(conteo_alumnos_secundario, file = "conteo_alumnos_secundario.csv", row.names = FALSE)

################################################################# EXTRACCIÓN DE DATOS TABLA POBLACIÓN Y CRUCE CON TABLAS DE PRIMARIO Y SECUNDARIO #################################################################
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

poblacion <- poblacion %>% # me quedo solo con las escuelas que tengan registro en tabla poblacion y tabla matricula
  inner_join(matricula, by = "ID1")

extranjeros_por_institucion <- poblacion %>%
  rowwise() %>%
  mutate(
    extranjeros_primaria = sum(c_across(matches("^Bolivia...Primaria|Paraguay...Primaria|Perú...Primaria|Venezuela...Primaria")), na.rm = TRUE),
    extranjeros_secundaria = sum(c_across(matches("^Bolivia...Secundaria|Paraguay...Secundaria|Perú...Secundaria|Venezuela...Secundaria")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(ID1, .keep_all = TRUE) %>%
  select(ID1, extranjeros_primaria, extranjeros_secundaria)

write.csv(extranjeros_por_institucion, file = "extranjeros_por_institucion.csv", row.names = FALSE)

escuelas_primarias_con_extranjeros_crudo <- extranjeros_por_institucion %>%
  filter(extranjeros_primaria > 0) %>%
  distinct(ID1)

escuelas_primarias_con_extranjeros <- conteo_alumnos_primaria %>%
  inner_join(escuelas_primarias_con_extranjeros_crudo, by = "ID1")

cantidad_de_escuelas_primarias_con_extranjeros <- escuelas_primarias_con_extranjeros %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total  # extraer el valor total

escuelas_secundarias_con_extranjeros_crudo <- extranjeros_por_institucion %>%
  filter(extranjeros_secundaria > 0) %>%
  distinct(ID1) 

escuelas_secundarias_con_extranjeros <- conteo_alumnos_secundario %>%
  inner_join(escuelas_secundarias_con_extranjeros_crudo, by = "ID1")

cantidad_de_escuelas_secundarias_con_extranjeros <- escuelas_secundarias_con_extranjeros %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

escuelas_primarias_sin_extranjeros <- conteo_alumnos_primaria %>%
  anti_join(escuelas_primarias_con_extranjeros_crudo, by = "ID1")

cantidad_de_escuelas_primarias_sin_extranjeros_2 <- escuelas_primarias_sin_extranjeros %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

escuelas_secundarias_sin_extranjeros <- conteo_alumnos_secundario %>%
  anti_join(escuelas_secundarias_con_extranjeros_crudo, by = "ID1")

cantidad_de_escuelas_secundarias_sin_extranjeros_2 <- escuelas_secundarias_sin_extranjeros %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

cantidad_de_escuelas_primarias_sin_extranjeros <- cantidad_total_de_escuelas_primarias - cantidad_de_escuelas_primarias_con_extranjeros
cantidad_de_escuelas_secundarias_sin_extranjeros <- cantidad_total_de_escuelas_secundarias - cantidad_de_escuelas_secundarias_con_extranjeros

print(paste("Total de cantidad_total_de_escuelas_primarias:", cantidad_total_de_escuelas_primarias))
print(paste("Total de cantidad_total_de_escuelas_secundarias:", cantidad_total_de_escuelas_secundarias))
print(paste("Total de cantidad_de_escuelas_primarias_con_extranjeros:", cantidad_de_escuelas_primarias_con_extranjeros))
print(paste("Total de cantidad_de_escuelas_secundarias_con_extranjeros:", cantidad_de_escuelas_secundarias_con_extranjeros))
print(paste("Total de cantidad_de_escuelas_primarias_sin_extranjeros:", cantidad_de_escuelas_primarias_sin_extranjeros))
print(paste("Total de cantidad_de_escuelas_secundarias_sin_extranjeros:", cantidad_de_escuelas_secundarias_sin_extranjeros))
print(paste("Total de cantidad_de_escuelas_primarias_sin_extranjeros_2:", cantidad_de_escuelas_primarias_sin_extranjeros_2))
print(paste("Total de cantidad_de_escuelas_secundarias_sin_extranjeros_2:", cantidad_de_escuelas_secundarias_sin_extranjeros_2))

######################################################## EXTRACCIÓN DE DATOS TABLA POBLACIÓN PARA ANÁLISIS DE BENEFICIOS DE ALIMENTACIÓN GRATUITOS ######################################################################
#########################################################################################################################################################################################################################

ids_comida <- poblacion %>%
  rowwise() %>% 
  mutate(
    cant_comidas = sum(c_across(starts_with("Beneficiarios.de.alimentación.grauita")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(cant_comidas > 0) %>% 
  select(ID1, cant_comidas)


escuelas_primarias_con_comida <- ids_comida %>%
  inner_join(conteo_alumnos_primaria, by = "ID1")

escuelas_secundarias_con_comida <- ids_comida %>%
  inner_join(conteo_alumnos_secundario, by = "ID1")

escuelas_primarias_con_extranjeros_y_comida <- ids_comida %>%
  inner_join(escuelas_primarias_con_extranjeros, by = "ID1")

escuelas_secundarias_con_extranjeros_y_comida <- ids_comida %>%
  inner_join(escuelas_secundarias_con_extranjeros, by = "ID1")

escuelas_primarias_sin_extranjeros_y_comida <- ids_comida %>%
  inner_join(escuelas_primarias_sin_extranjeros, by = "ID1")

escuelas_secundarias_sin_extranjeros_y_comida <- ids_comida %>%
  inner_join(escuelas_secundarias_sin_extranjeros, by = "ID1")



cantidad_escuelas_primarias_con_comida <- escuelas_primarias_con_comida %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

cantidad_escuelas_secundarias_con_comida <- escuelas_secundarias_con_comida %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

cantidad_escuelas_primarias_con_extranjeros_y_comida <- escuelas_primarias_con_extranjeros_y_comida %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

cantidad_escuelas_secundarias_con_extranjeros_y_comida <- escuelas_secundarias_con_extranjeros_y_comida %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

cantidad_escuelas_primarias_sin_extranjeros_y_comida <- escuelas_primarias_sin_extranjeros_y_comida %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total

cantidad_escuelas_secundarias_sin_extranjeros_y_comida <- escuelas_secundarias_sin_extranjeros_y_comida %>%
  summarise(total = n_distinct(ID1)) %>%
  .$total



cantidad_escuelas_primarias_sin_comida <- cantidad_total_de_escuelas_primarias - cantidad_escuelas_primarias_con_comida
cantidad_escuelas_secundarias_sin_comida <- cantidad_total_de_escuelas_secundarias - cantidad_escuelas_secundarias_con_comida

cantidad_escuelas_primarias_con_extranjeros_sin_comida <- cantidad_de_escuelas_primarias_con_extranjeros - cantidad_escuelas_primarias_con_extranjeros_y_comida
cantidad_escuelas_secundarias_con_extranjeros_sin_comida <- cantidad_de_escuelas_secundarias_con_extranjeros - cantidad_escuelas_secundarias_con_extranjeros_y_comida

cantidad_escuelas_primarias_sin_extranjeros_sin_comida <- cantidad_de_escuelas_primarias_sin_extranjeros - cantidad_escuelas_primarias_sin_extranjeros_y_comida
cantidad_escuelas_secundarias_sin_extranjeros_sin_comida <- cantidad_de_escuelas_secundarias_sin_extranjeros - cantidad_escuelas_secundarias_sin_extranjeros_y_comida



print(paste("Total de cantidad_escuelas_primarias_con_comida:", cantidad_escuelas_primarias_con_comida))
print(paste("Total de cantidad_escuelas_secundarias_con_comida:", cantidad_escuelas_secundarias_con_comida))

print(paste("Total de cantidad_escuelas_primarias_con_extranjeros_y_comida:", cantidad_escuelas_primarias_con_extranjeros_y_comida))
print(paste("Total de cantidad_escuelas_secundarias_con_extranjeros_y_comida:", cantidad_escuelas_secundarias_con_extranjeros_y_comida))

print(paste("Total de cantidad_escuelas_primarias_sin_extranjeros_y_comida:", cantidad_escuelas_primarias_sin_extranjeros_y_comida))
print(paste("Total de cantidad_escuelas_secundarias_sin_extranjeros_y_comida:", cantidad_escuelas_secundarias_sin_extranjeros_y_comida))



print(paste("Total de cantidad_escuelas_primarias_sin_comida:", cantidad_escuelas_primarias_sin_comida))
print(paste("Total de cantidad_escuelas_secundarias_sin_comida:", cantidad_escuelas_secundarias_sin_comida))

print(paste("Total de cantidad_escuelas_primarias_con_extranjeros_sin_comida:", cantidad_escuelas_primarias_con_extranjeros_sin_comida))
print(paste("Total de cantidad_escuelas_secundarias_con_extranjeros_sin_comida:", cantidad_escuelas_secundarias_con_extranjeros_sin_comida))

print(paste("Total de cantidad_escuelas_primarias_sin_extranjeros_sin_comida:", cantidad_escuelas_primarias_sin_extranjeros_sin_comida))
print(paste("Total de cantidad_escuelas_secundarias_sin_extranjeros_sin_comida:", cantidad_escuelas_secundarias_sin_extranjeros_sin_comida))



##### Relevación de Datos Sector Privado y Público #####

primarias_sector <- escuelas_primarias_con_comida %>%
  inner_join(matricula, by = "ID1") %>%
  count(sector) %>%
  mutate(porcentaje = (n / sum(n)) * 100)

print("Primarias con comida, total por sector y porcentaje:")
print(primarias_sector)

secundarias_sector <- escuelas_secundarias_con_comida %>%
  inner_join(matricula, by = "ID1") %>%
  count(sector) %>%
  mutate(porcentaje = (n / sum(n)) * 100)

print("Secundarias con comida, total por sector y porcentaje:")
print(secundarias_sector)



######################################## PROCESAMIENTO DE DATOS PARA GENERACIÓN DE GRÁFICOS ANALIZANDO LA DISTRIBUCIÓN DEL BENEFICIO ALIMENTICIO GRATUITO ###############################################################
#########################################################################################################################################################################################################################


tabla_resumen <- tibble(
  Categoria = c(
    "Escuelas primarias con beneficio alimenticio",
    "Escuelas primarias sin beneficio alimenticio",
    "Escuelas secundarias con beneficio alimenticio",
    "Escuelas secundarias sin beneficio alimenticio",
    
    "Escuelas primarias con extranjeros con beneficio alimenticio",
    "Escuelas primarias con extranjeros sin beneficio alimenticio",
    "Escuelas secundarias con extranjeros con beneficio alimenticio",
    "Escuelas secundarias con extranjeros sin beneficio alimenticio",
    
    "Escuelas primarias sin extranjeros con beneficio alimenticio",
    "Escuelas primarias sin extranjeros sin beneficio alimenticio",
    "Escuelas secundarias sin extranjeros con beneficio alimenticio",
    "Escuelas secundarias sin extranjeros sin beneficio alimenticio"
  ),
  Cantidad = c(
    cantidad_escuelas_primarias_con_comida,
    cantidad_escuelas_primarias_sin_comida,
    cantidad_escuelas_secundarias_con_comida,
    cantidad_escuelas_secundarias_sin_comida,
    
    cantidad_escuelas_primarias_con_extranjeros_y_comida,
    cantidad_escuelas_primarias_con_extranjeros_sin_comida,
    cantidad_escuelas_secundarias_con_extranjeros_y_comida,
    cantidad_escuelas_secundarias_con_extranjeros_sin_comida,
    
    cantidad_escuelas_primarias_sin_extranjeros_y_comida,
    cantidad_escuelas_primarias_sin_extranjeros_sin_comida,
    cantidad_escuelas_secundarias_sin_extranjeros_y_comida,
    cantidad_escuelas_secundarias_sin_extranjeros_sin_comida
  )
)


tabla_resumen <- tabla_resumen %>%
  mutate(Porcentaje = case_when(

    Categoria %in% c("Escuelas primarias con beneficio alimenticio", "Escuelas primarias sin beneficio alimenticio") ~ 
      (Cantidad / cantidad_total_de_escuelas_primarias) * 100,
    Categoria %in% c("Escuelas secundarias con beneficio alimenticio", "Escuelas secundarias sin beneficio alimenticio") ~ 
      (Cantidad / cantidad_total_de_escuelas_secundarias) * 100,
    

    Categoria %in% c("Escuelas primarias con extranjeros con beneficio alimenticio", "Escuelas primarias con extranjeros sin beneficio alimenticio") ~ 
      (Cantidad / cantidad_de_escuelas_primarias_con_extranjeros) * 100,
    Categoria %in% c("Escuelas secundarias con extranjeros con beneficio alimenticio", "Escuelas secundarias con extranjeros sin beneficio alimenticio") ~ 
      (Cantidad / cantidad_de_escuelas_secundarias_con_extranjeros) * 100,
    

    Categoria %in% c("Escuelas primarias sin extranjeros con beneficio alimenticio", "Escuelas primarias sin extranjeros sin beneficio alimenticio") ~ 
      (Cantidad / cantidad_de_escuelas_primarias_sin_extranjeros) * 100,
    Categoria %in% c("Escuelas secundarias sin extranjeros con beneficio alimenticio", "Escuelas secundarias sin extranjeros sin beneficio alimenticio") ~ 
      (Cantidad / cantidad_de_escuelas_secundarias_sin_extranjeros) * 100,
    
    TRUE ~ NA_real_
  ))

print(tabla_resumen)

colors <- c("turquoise","orange")

dir_path <- "distribucion_de_escuelas_por_niveles_educativos"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "distribucion_de_escuelas_por_niveles_educativos/primaria"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "distribucion_de_escuelas_por_niveles_educativos/secundaria"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios/primaria"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios/secundaria"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios/primaria/con_extranjeros"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios/secundaria/con_extranjeros"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios/primaria/sin_extranjeros"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

dir_path <- "beneficios_alimenticios/secundaria/sin_extranjeros"
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

####################################################################################################################################################################################
######################################## GRÁFICOS DE TORTA PARA DISTRIBUCIÓN TOTAL DE ESCUELAS CON Y SIN EXTRANJEROS ###############################################################
####################################################################################################################################################################################

############################################################### PRIMARIO ###############################################################

tabla1 <- tibble(
  Categoria = c(
    "Escuelas primarias sin extranjeros",
    "Escuelas primarias con extranjeros"
  ),
  Porcentaje = c(
    (cantidad_de_escuelas_primarias_sin_extranjeros / cantidad_total_de_escuelas_primarias) * 100,
    (cantidad_de_escuelas_primarias_con_extranjeros/ cantidad_total_de_escuelas_primarias) * 100
  )
)

porcentajes <- tabla1$Porcentaje
sectores <- tabla1$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias en Argentina\n con y sin Estudiantes Extranjeros", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("distribucion_de_escuelas_por_niveles_educativos/primaria/escuelas_primarias_con_y_sin_extranjeros_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias en Argentina\n con y sin Estudiantes Extranjeros", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

############################################################### SECUNDARIO  ###############################################################

tabla2 <- tibble(
  Categoria = c(
    "Escuelas secundarias sin extranjeros",
    "Escuelas secundarias con extranjeros"
  ),
  Porcentaje = c(
    (cantidad_de_escuelas_secundarias_sin_extranjeros / cantidad_total_de_escuelas_secundarias) * 100,
    (cantidad_de_escuelas_secundarias_con_extranjeros/ cantidad_total_de_escuelas_secundarias) * 100
  )
)

porcentajes <- tabla2$Porcentaje
sectores <- tabla2$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias en Argentina\n con y sin Estudiantes Extranjeros", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("distribucion_de_escuelas_por_niveles_educativos/secundaria/escuelas_secundarias_con_y_sin_extranjeros_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias en Argentina\n con y sin Estudiantes Extranjeros", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

####################################################################################################################################################################################
################################### GRÁFICOS DE TORTA PARA DISTRIBUCIÓN TOTAL DE ESCUELAS CON Y SIN BENEFICIO ALIMENTICIO ##########################################################
####################################################################################################################################################################################

############################################################### PRIMARIO ###############################################################

tabla13 <- tabla_resumen %>% 
  filter(Categoria == "Escuelas primarias con beneficio alimenticio" | 
           Categoria == "Escuelas primarias sin beneficio alimenticio")

print(tabla13)

porcentajes <- tabla13$Porcentaje
sectores <- tabla13$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Primarias en Argentina:\n Con o Sin Beneficio Alimenticio", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("beneficios_alimenticios/primaria/escuelas_primarias_con_y_sin_comida_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Primarias en Argentina:\n Con o Sin Beneficio Alimenticio", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

############################################################### SECUNDARIO ###############################################################

tabla14 <- tabla_resumen %>% 
  filter(Categoria == "Escuelas secundarias con beneficio alimenticio" | 
           Categoria == "Escuelas secundarias sin beneficio alimenticio")

print(tabla14)

porcentajes <- tabla14$Porcentaje
sectores <- tabla14$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Secundarias en Argentina:\n Con o Sin Beneficio Alimenticio", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("beneficios_alimenticios/secundaria/escuelas_secundarias_con_y_sin_comida_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Secundarias en Argentina:\n Con o Sin Beneficio Alimenticio", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

####################################################################################################################################################################################
########################## GRÁFICOS DE TORTA PARA DISTRIBUCIÓN TOTAL DE ESCUELAS CON ESTUDIANTES EXTRANJEROS, CON Y SIN BENEFICIO ALIMENTICIO ######################################
####################################################################################################################################################################################

############################################################### PRIMARIO ###############################################################

tabla3 <- tabla_resumen %>% 
  filter(Categoria == "Escuelas primarias con extranjeros con beneficio alimenticio" | 
           Categoria == "Escuelas primarias con extranjeros sin beneficio alimenticio")

print(tabla3)

porcentajes <- tabla3$Porcentaje
sectores <- tabla3$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Primarias en Argentina\n con Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("beneficios_alimenticios/primaria/con_extranjeros/escuelas_primarias_con_extranjeros_con_y_sin_comida_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Primarias en Argentina\n con Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

############################################################### SECUNDARIO ###############################################################

tabla4 <- tabla_resumen %>% 
  filter(Categoria == "Escuelas secundarias con extranjeros con beneficio alimenticio" | 
           Categoria == "Escuelas secundarias con extranjeros sin beneficio alimenticio")

print(tabla4)

porcentajes <- tabla4$Porcentaje
sectores <- tabla4$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Secundarias en Argentina\n con Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("beneficios_alimenticios/secundaria/con_extranjeros/escuelas_secundarias_con_extranjeros_con_y_sin_comida_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Secundarias en Argentina\n con Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

####################################################################################################################################################################################
########################## GRÁFICOS DE TORTA PARA DISTRIBUCIÓN TOTAL DE ESCUELAS SIN ESTUDIANTES EXTRANJEROS, CON Y SIN BENEFICIO ALIMENTICIO ######################################
####################################################################################################################################################################################

############################################################### PRIMARIO ###############################################################

tabla23 <- tabla_resumen %>% 
  filter(Categoria == "Escuelas primarias sin extranjeros con beneficio alimenticio" | 
           Categoria == "Escuelas primarias sin extranjeros sin beneficio alimenticio")

print(tabla23)

porcentajes <- tabla23$Porcentaje
sectores <- tabla23$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Primarias en Argentina\n sin Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("beneficios_alimenticios/primaria/sin_extranjeros/escuelas_primarias_sin_extranjeros_con_y_sin_comida_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Primarias en Argentina\n sin Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

############################################################### SECUNDARIO ###############################################################

tabla24 <- tabla_resumen %>% 
  filter(Categoria == "Escuelas secundarias sin extranjeros con beneficio alimenticio" | 
           Categoria == "Escuelas secundarias sin extranjeros sin beneficio alimenticio")

print(tabla24)

porcentajes <- tabla24$Porcentaje
sectores <- tabla24$Categoria

pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Secundarias en Argentina\n sin Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)

legend("bottomright", legend = sectores, fill = colors, cex = 0.8)

png(paste0("beneficios_alimenticios/secundaria/sin_extranjeros/escuelas_secundarias_sin_extranjeros_con_y_sin_comida_", year, ".png"), width = 600, height = 600)
pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución de Escuelas Secundarias en Argentina\n sin Estudiantes Extranjeros: Con o Sin Beneficio Alimenticio", year), border = colors)
legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
dev.off()

#################################################################################################################################################################################################################
####################################### PROCESAMIENTO DE DATOS Y GRÁFICOS DE BARRAS PARA ESCUELAS SEGÚN BRINDAN BENEFICIOS ALIMENTICIOS O NO POR PROVINCIA ######################################################
#################################################################################################################################################################################################################

############################################################################### PRIMARIA ################################################################################

escuelas_primarias_por_provincia <- conteo_alumnos_primaria %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_primarias_por_provincia = n())

print(n=30,escuelas_primarias_por_provincia)

escuelas_primarias_con_comida_por_provincia <- escuelas_primarias_con_comida %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_primarias_con_comida_por_provincia = n()) 

print(n=30,escuelas_primarias_con_comida_por_provincia)

escuelas_primarias_por_provincia_barras <- escuelas_primarias_por_provincia %>%
  left_join(escuelas_primarias_con_comida_por_provincia, by = "provincia") %>%
mutate(cantidad_escuelas_primarias_con_comida_por_provincia = replace_na(cantidad_escuelas_primarias_con_comida_por_provincia, 0),
    cantidad_escuelas_primarias_sin_comida_por_provincia = 
           cantidad_escuelas_primarias_por_provincia - cantidad_escuelas_primarias_con_comida_por_provincia)

escuelas_primarias_por_provincia_porcentajes <- escuelas_primarias_por_provincia_barras %>%
  mutate(
    porcentaje_con_comida = (cantidad_escuelas_primarias_con_comida_por_provincia / cantidad_escuelas_primarias_por_provincia) * 100,
    porcentaje_sin_comida = (cantidad_escuelas_primarias_sin_comida_por_provincia / cantidad_escuelas_primarias_por_provincia) * 100
  )

print(escuelas_primarias_por_provincia_porcentajes, n = 30)

write.csv(escuelas_primarias_por_provincia_porcentajes, file = "escuelas_primarias_por_provincia_porcentajes.csv", row.names = FALSE)

escuelas_primarias_por_provincia_porcentajes_barras <- escuelas_primarias_por_provincia_porcentajes  %>%
  select(provincia,porcentaje_con_comida,porcentaje_sin_comida)

print(n=30,escuelas_primarias_por_provincia_porcentajes_barras)

escuelas_primarias_por_provincia_porcentajes_barras <- escuelas_primarias_por_provincia_porcentajes_barras %>%
  mutate(provincia = factor(provincia, levels = escuelas_primarias_por_provincia_porcentajes_barras$provincia[order(porcentaje_con_comida)]))


escuelas_primarias_por_provincia_porcentajes_barras <- data.frame(
  provincia = escuelas_primarias_por_provincia_porcentajes_barras$provincia,
  sector = c(rep("Sin Beneficio Alimenticio", nrow(escuelas_primarias_por_provincia_porcentajes_barras)),
             rep("Con Beneficio Alimenticio", nrow(escuelas_primarias_por_provincia_porcentajes_barras))),
  porcentaje = c(escuelas_primarias_por_provincia_porcentajes_barras$porcentaje_sin_comida,
                 escuelas_primarias_por_provincia_porcentajes_barras$porcentaje_con_comida)
)

escuelas_primarias_por_provincia_porcentajes_barras$sector <- factor(escuelas_primarias_por_provincia_porcentajes_barras$sector, levels = c("Sin Beneficio Alimenticio", "Con Beneficio Alimenticio"))

print(escuelas_primarias_por_provincia_porcentajes_barras)

ggplot(escuelas_primarias_por_provincia_porcentajes_barras, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = round(porcentaje, 1)),
            position = position_stack(vjust = 0.5),  # centrar texto
            color = "white",
            size = 4) +
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = paste0('Porcentaje del Total de Escuelas Primarias\n por Provincia y Beneficio Alimenticio ',year)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual('Beneficio Alimenticio ', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()  # horizontal

ggsave(paste0("beneficios_alimenticios/primaria/barras_por_provincia_escuelas_primarias_con_y_sin_comida_", year, ".png"), width = 8, height = 8, dpi = 300)

############################################################################### SECUNDARIA ################################################################################

escuelas_secundarias_por_provincia <- conteo_alumnos_secundario %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_secundarias_por_provincia = n())

print(n=30,escuelas_secundarias_por_provincia)

escuelas_secundarias_con_comida_por_provincia <- escuelas_secundarias_con_comida %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_secundarias_con_comida_por_provincia = n()) 

print(n=30,escuelas_secundarias_con_comida_por_provincia)

escuelas_secundarias_por_provincia_barras <- escuelas_secundarias_por_provincia %>%
  left_join(escuelas_secundarias_con_comida_por_provincia, by = "provincia") %>%
  mutate(cantidad_escuelas_secundarias_con_comida_por_provincia = replace_na(cantidad_escuelas_secundarias_con_comida_por_provincia, 0),
    cantidad_escuelas_secundarias_sin_comida_por_provincia = 
           cantidad_escuelas_secundarias_por_provincia - cantidad_escuelas_secundarias_con_comida_por_provincia)

escuelas_secundarias_por_provincia_porcentajes <- escuelas_secundarias_por_provincia_barras %>%
  mutate(
    porcentaje_con_comida = (cantidad_escuelas_secundarias_con_comida_por_provincia / cantidad_escuelas_secundarias_por_provincia) * 100,
    porcentaje_sin_comida = (cantidad_escuelas_secundarias_sin_comida_por_provincia / cantidad_escuelas_secundarias_por_provincia) * 100
  )

print(escuelas_secundarias_por_provincia_porcentajes, n = 30)

write.csv(escuelas_secundarias_por_provincia_porcentajes, file = "escuelas_secundarias_por_provincia_porcentajes.csv", row.names = FALSE)

escuelas_secundarias_por_provincia_porcentajes_barras <- escuelas_secundarias_por_provincia_porcentajes  %>%
  select(provincia,porcentaje_con_comida,porcentaje_sin_comida)

escuelas_secundarias_por_provincia_porcentajes_barras <- escuelas_secundarias_por_provincia_porcentajes_barras %>%
  mutate(provincia = factor(provincia, levels = escuelas_secundarias_por_provincia_porcentajes_barras$provincia[order(porcentaje_con_comida)]))

print(n=30,escuelas_secundarias_por_provincia_porcentajes_barras)


escuelas_secundarias_por_provincia_porcentajes_barras <- data.frame(
  provincia = escuelas_secundarias_por_provincia_porcentajes_barras$provincia,
  sector = c(rep("Sin Beneficio Alimenticio", nrow(escuelas_secundarias_por_provincia_porcentajes_barras)),
    rep("Con Beneficio Alimenticio", nrow(escuelas_secundarias_por_provincia_porcentajes_barras))),
  porcentaje = c(escuelas_secundarias_por_provincia_porcentajes_barras$porcentaje_sin_comida,
                 escuelas_secundarias_por_provincia_porcentajes_barras$porcentaje_con_comida)
)

escuelas_secundarias_por_provincia_porcentajes_barras$sector <- factor(escuelas_secundarias_por_provincia_porcentajes_barras$sector, levels = c("Sin Beneficio Alimenticio", "Con Beneficio Alimenticio"))

print(escuelas_secundarias_por_provincia_porcentajes_barras)

ggplot(escuelas_secundarias_por_provincia_porcentajes_barras, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = round(porcentaje, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = paste0('Porcentaje del Total de Escuelas Secundarias\n por Provincia y Beneficio Alimenticio ',year)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual('Beneficio Alimenticio ', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()

ggsave(paste0("beneficios_alimenticios/secundaria/barras_por_provincia_escuelas_secundarias_con_y_sin_comida_", year, ".png"), width = 8, height = 8, dpi = 300)


#################################################################################################################################################################################################################
############################# PROCESAMIENTO DE DATOS Y GRÁFICOS DE BARRAS PARA ESCUELAS CON ESTUDIANTES EXTRANJEROS SEGÚN BRINDAN BENEFICIOS ALIMENTICIOS O NO POR PROVINCIA  ###################################
#################################################################################################################################################################################################################

############################################################################### PRIMARIA ################################################################################

escuelas_primarias_con_extranjeros_por_provincia <- escuelas_primarias_con_extranjeros %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_primarias_con_extranjeros_por_provincia = n())

print(n=30,escuelas_primarias_con_extranjeros_por_provincia)

escuelas_primarias_con_extranjeros_con_comida_por_provincia <- escuelas_primarias_con_extranjeros_y_comida %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_primarias_con_extranjeros_con_comida_por_provincia = n()) 

print(n=30,escuelas_primarias_con_extranjeros_con_comida_por_provincia)

escuelas_primarias_con_extranjeros_por_provincia_barras <- escuelas_primarias_con_extranjeros_por_provincia %>%
  left_join(escuelas_primarias_con_extranjeros_con_comida_por_provincia, by = "provincia") %>%
  mutate(cantidad_escuelas_primarias_con_extranjeros_con_comida_por_provincia = replace_na(cantidad_escuelas_primarias_con_extranjeros_con_comida_por_provincia, 0),
         cantidad_escuelas_primarias_con_extranjeros_sin_comida_por_provincia = 
           cantidad_escuelas_primarias_con_extranjeros_por_provincia - cantidad_escuelas_primarias_con_extranjeros_con_comida_por_provincia)

escuelas_primarias_con_extranjeros_por_provincia_porcentajes <- escuelas_primarias_con_extranjeros_por_provincia_barras %>%
  mutate(
    porcentaje_con_comida = (cantidad_escuelas_primarias_con_extranjeros_con_comida_por_provincia / cantidad_escuelas_primarias_con_extranjeros_por_provincia) * 100,
    porcentaje_sin_comida = (cantidad_escuelas_primarias_con_extranjeros_sin_comida_por_provincia / cantidad_escuelas_primarias_con_extranjeros_por_provincia) * 100
  )

print(escuelas_primarias_con_extranjeros_por_provincia_porcentajes, n = 30)

write.csv(escuelas_primarias_con_extranjeros_por_provincia_porcentajes, file = "escuelas_primarias_con_extranjeros_por_provincia_porcentajes.csv", row.names = FALSE)

escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras <- escuelas_primarias_con_extranjeros_por_provincia_porcentajes  %>%
  select(provincia,porcentaje_con_comida,porcentaje_sin_comida)

print(n=30,escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras)

escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras <- escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras %>%
  mutate(provincia = factor(provincia, levels = escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras$provincia[order(porcentaje_con_comida)]))


escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras <- data.frame(
  provincia = escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras$provincia,
  sector = c(rep("Sin Beneficio Alimenticio", nrow(escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras)),
             rep("Con Beneficio Alimenticio", nrow(escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras))),
  porcentaje = c(escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras$porcentaje_sin_comida,
                 escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras$porcentaje_con_comida)
)

escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras$sector <- factor(escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras$sector, levels = c("Sin Beneficio Alimenticio", "Con Beneficio Alimenticio"))

print(escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras)

ggplot(escuelas_primarias_con_extranjeros_por_provincia_porcentajes_barras, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = round(porcentaje, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = paste0('Porcentaje del Total de Escuelas Primarias con Estudiantes\n Extranjeros por Provincia y Beneficio Alimenticio ',year)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual('Beneficio Alimenticio ', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()

ggsave(paste0("beneficios_alimenticios/primaria/con_extranjeros/barras_por_provincia_escuelas_primarias_con_extranjeros_con_y_sin_comida_", year, ".png"), width = 8, height = 8, dpi = 300)

############################################################################### SECUNDARIA ################################################################################

escuelas_secundarias_con_extranjeros_por_provincia <- escuelas_secundarias_con_extranjeros %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_secundarias_con_extranjeros_por_provincia = n())

print(n=30,escuelas_secundarias_con_extranjeros_por_provincia)

escuelas_secundarias_con_extranjeros_con_comida_por_provincia <- escuelas_secundarias_con_extranjeros_y_comida %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_secundarias_con_extranjeros_con_comida_por_provincia = n()) 

print(n=30,escuelas_secundarias_con_extranjeros_con_comida_por_provincia)

escuelas_secundarias_con_extranjeros_por_provincia_barras <- escuelas_secundarias_con_extranjeros_por_provincia %>%
  left_join(escuelas_secundarias_con_extranjeros_con_comida_por_provincia, by = "provincia") %>%
  mutate(cantidad_escuelas_secundarias_con_extranjeros_con_comida_por_provincia = replace_na(cantidad_escuelas_secundarias_con_extranjeros_con_comida_por_provincia, 0),
         cantidad_escuelas_secundarias_con_extranjeros_sin_comida_por_provincia = 
           cantidad_escuelas_secundarias_con_extranjeros_por_provincia - cantidad_escuelas_secundarias_con_extranjeros_con_comida_por_provincia)

escuelas_secundarias_con_extranjeros_por_provincia_porcentajes <- escuelas_secundarias_con_extranjeros_por_provincia_barras %>%
  mutate(
    porcentaje_con_comida = (cantidad_escuelas_secundarias_con_extranjeros_con_comida_por_provincia / cantidad_escuelas_secundarias_con_extranjeros_por_provincia) * 100,
    porcentaje_sin_comida = (cantidad_escuelas_secundarias_con_extranjeros_sin_comida_por_provincia / cantidad_escuelas_secundarias_con_extranjeros_por_provincia) * 100
  )

print(escuelas_secundarias_con_extranjeros_por_provincia_barras, n = 30)

write.csv(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes, file = "escuelas_secundarias_con_extranjeros_por_provincia_porcentajes.csv", row.names = FALSE)

escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras <- escuelas_secundarias_con_extranjeros_por_provincia_porcentajes  %>%
  select(provincia,porcentaje_con_comida,porcentaje_sin_comida)

print(n=30,escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras)

escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras <- escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras %>%
  mutate(provincia = factor(provincia, levels = escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras$provincia[order(porcentaje_con_comida)]))


escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras <- data.frame(
  provincia = escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras$provincia,
  sector = c(rep("Sin Beneficio Alimenticio", nrow(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras)),
             rep("Con Beneficio Alimenticio", nrow(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras))),
  porcentaje = c(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras$porcentaje_sin_comida,
                 escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras$porcentaje_con_comida)
)

escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras$sector <- factor(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras$sector, levels = c("Sin Beneficio Alimenticio", "Con Beneficio Alimenticio"))

print(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras)

ggplot(escuelas_secundarias_con_extranjeros_por_provincia_porcentajes_barras, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = round(porcentaje, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = paste0('Porcentaje del Total de Escuelas Secundarias con Estudiantes\n Extranjeros por Provincia y Beneficio Alimenticio ',year)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual('Beneficio Alimenticio ', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()


ggsave(paste0("beneficios_alimenticios/secundaria/con_extranjeros/barras_por_provincia_escuelas_secundarias_con_extranjeros_con_y_sin_comida_", year, ".png"), width = 8, height = 8, dpi = 300)


#################################################################################################################################################################################################################
############################# PROCESAMIENTO DE DATOS Y GRÁFICOS DE BARRAS PARA ESCUELAS SIN ESTUDIANTES EXTRANJEROS SEGÚN BRINDAN BENEFICIOS ALIMENTICIOS O NO POR PROVINCIA  ###################################
#################################################################################################################################################################################################################

############################################################################### PRIMARIA ################################################################################

escuelas_primarias_sin_extranjeros_por_provincia <- escuelas_primarias_sin_extranjeros %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_primarias_sin_extranjeros_por_provincia = n())

print(n=30,escuelas_primarias_sin_extranjeros_por_provincia)

escuelas_primarias_sin_extranjeros_con_comida_por_provincia <- escuelas_primarias_sin_extranjeros_y_comida %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_primarias_sin_extranjeros_con_comida_por_provincia = n()) 

print(n=30,escuelas_primarias_sin_extranjeros_con_comida_por_provincia)

escuelas_primarias_sin_extranjeros_por_provincia_barras <- escuelas_primarias_sin_extranjeros_por_provincia %>%
  left_join(escuelas_primarias_sin_extranjeros_con_comida_por_provincia, by = "provincia") %>%
  mutate(cantidad_escuelas_primarias_sin_extranjeros_con_comida_por_provincia = replace_na(cantidad_escuelas_primarias_sin_extranjeros_con_comida_por_provincia, 0),
         cantidad_escuelas_primarias_sin_extranjeros_sin_comida_por_provincia = 
           cantidad_escuelas_primarias_sin_extranjeros_por_provincia - cantidad_escuelas_primarias_sin_extranjeros_con_comida_por_provincia)

escuelas_primarias_sin_extranjeros_por_provincia_porcentajes <- escuelas_primarias_sin_extranjeros_por_provincia_barras %>%
  mutate(
    porcentaje_con_comida = (cantidad_escuelas_primarias_sin_extranjeros_con_comida_por_provincia / cantidad_escuelas_primarias_sin_extranjeros_por_provincia) * 100,
    porcentaje_sin_comida = (cantidad_escuelas_primarias_sin_extranjeros_sin_comida_por_provincia / cantidad_escuelas_primarias_sin_extranjeros_por_provincia) * 100
  )

print(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes, n = 30)

write.csv(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes, file = "escuelas_primarias_sin_extranjeros_por_provincia_porcentajes.csv", row.names = FALSE)

escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras <- escuelas_primarias_sin_extranjeros_por_provincia_porcentajes  %>%
  select(provincia,porcentaje_con_comida,porcentaje_sin_comida)

print(n=30,escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras)

escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras <- escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras %>%
  mutate(provincia = factor(provincia, levels = escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras$provincia[order(porcentaje_con_comida)]))


escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras <- data.frame(
  provincia = escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras$provincia,
  sector = c(rep("Sin Beneficio Alimenticio", nrow(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras)),
             rep("Con Beneficio Alimenticio", nrow(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras))),
  porcentaje = c(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras$porcentaje_sin_comida,
                 escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras$porcentaje_con_comida)
)

escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras$sector <- factor(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras$sector, levels = c("Sin Beneficio Alimenticio", "Con Beneficio Alimenticio"))

print(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras)

ggplot(escuelas_primarias_sin_extranjeros_por_provincia_porcentajes_barras, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = round(porcentaje, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = paste0('Porcentaje del Total de Escuelas Primarias sin Estudiantes\n Extranjeros por Provincia y Beneficio Alimenticio ',year)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual('Beneficio Alimenticio ', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()


ggsave(paste0("beneficios_alimenticios/primaria/sin_extranjeros/barras_por_provincia_escuelas_primarias_sin_extranjeros_con_y_sin_comida_", year, ".png"), width = 8, height = 8, dpi = 300)

############################################################################### SECUNDARIA ################################################################################

escuelas_secundarias_sin_extranjeros_por_provincia <- escuelas_secundarias_sin_extranjeros %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_secundarias_sin_extranjeros_por_provincia = n())

print(n=30,escuelas_secundarias_sin_extranjeros_por_provincia)

escuelas_secundarias_sin_extranjeros_con_comida_por_provincia <- escuelas_secundarias_sin_extranjeros_y_comida %>%
  group_by(provincia) %>%
  summarize(cantidad_escuelas_secundarias_sin_extranjeros_con_comida_por_provincia = n()) 

print(n=30,escuelas_secundarias_sin_extranjeros_con_comida_por_provincia)

escuelas_secundarias_sin_extranjeros_por_provincia_barras <- escuelas_secundarias_sin_extranjeros_por_provincia %>%
  left_join(escuelas_secundarias_sin_extranjeros_con_comida_por_provincia, by = "provincia") %>%
  mutate(cantidad_escuelas_secundarias_sin_extranjeros_con_comida_por_provincia = replace_na(cantidad_escuelas_secundarias_sin_extranjeros_con_comida_por_provincia, 0),
         cantidad_escuelas_secundarias_sin_extranjeros_sin_comida_por_provincia = 
           cantidad_escuelas_secundarias_sin_extranjeros_por_provincia - cantidad_escuelas_secundarias_sin_extranjeros_con_comida_por_provincia)

escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes <- escuelas_secundarias_sin_extranjeros_por_provincia_barras %>%
  mutate(
    porcentaje_con_comida = (cantidad_escuelas_secundarias_sin_extranjeros_con_comida_por_provincia / cantidad_escuelas_secundarias_sin_extranjeros_por_provincia) * 100,
    porcentaje_sin_comida = (cantidad_escuelas_secundarias_sin_extranjeros_sin_comida_por_provincia / cantidad_escuelas_secundarias_sin_extranjeros_por_provincia) * 100
  )

print(escuelas_secundarias_sin_extranjeros_por_provincia_barras, n = 30)

write.csv(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes, file = "escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes.csv", row.names = FALSE)

escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras <- escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes  %>%
  select(provincia,porcentaje_con_comida,porcentaje_sin_comida)

print(n=30,escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras)

escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras <- escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras %>%
  mutate(provincia = factor(provincia, levels = escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras$provincia[order(porcentaje_con_comida)]))


escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras <- data.frame(
  provincia = escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras$provincia,
  sector = c(rep("Sin Beneficio Alimenticio", nrow(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras)),
             rep("Con Beneficio Alimenticio", nrow(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras))),
  porcentaje = c(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras$porcentaje_sin_comida,
                 escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras$porcentaje_con_comida)
)

escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras$sector <- factor(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras$sector, levels = c("Sin Beneficio Alimenticio", "Con Beneficio Alimenticio"))

print(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras)

ggplot(escuelas_secundarias_sin_extranjeros_por_provincia_porcentajes_barras, aes(fill = sector, x = provincia, y = porcentaje)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = round(porcentaje, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  theme_minimal() +
  labs(x = 'Provincia', y = 'Porcentaje de Escuelas',
       title = paste0('Porcentaje del Total de Escuelas Secundarias sin Estudiantes\n Extranjeros por Provincia y Beneficio Alimenticio ',year)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        legend.title = element_blank(),
        legend.background = element_rect(size = 0.2)) +
  scale_fill_manual('Beneficio Alimenticio ', values = c( '#cd4d51','#91cd4d')) +
  coord_flip()


ggsave(paste0("beneficios_alimenticios/secundaria/sin_extranjeros/barras_por_provincia_escuelas_secundarias_sin_extranjeros_con_y_sin_comida_", year, ".png"), width = 8, height = 8, dpi = 300)

########################## Distribución Total de Escuelas que Ofrecen Comida, con y sin Estudiantes Extranjeros en Argentina ########################## 

# ### primario
# 
# tabla7 <- data.frame(
#   Categoria = c("Escuelas primarias con comida con extranjeros",
#                 "Escuelas primarias con comida sin extranjeros"
#                 ),
#   Porcentaje = c(
#     (cantidad_escuelas_primarias_con_extranjeros_y_comida * 100) / cantidad_escuelas_primarias_con_comida,
#     (cantidad_escuelas_primarias_sin_extranjeros_y_comida * 100) / cantidad_escuelas_primarias_con_comida
#   )
# )
# print(tabla7)
# 
# porcentajes <- tabla7$Porcentaje
# sectores <- tabla7$Categoria
# 
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias que Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# 
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# 
# png(paste0("beneficios_alimenticios/primaria/con_comida/escuelas_primarias_con_comida_sin_y_con_extranjeros_", year, ".png"), width = 600, height = 600)
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias que Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# dev.off()
# 
# ### secundario
# 
# tabla8 <- data.frame(
#   Categoria = c("Escuelas secundarias con comida con extranjeros",
#                 "Escuelas secundarias con comida sin extranjeros"
#   ),
#   Porcentaje = c(
#     (cantidad_escuelas_secundarias_con_extranjeros_y_comida * 100) / cantidad_escuelas_secundarias_con_comida,
#     (cantidad_escuelas_secundarias_sin_extranjeros_y_comida * 100) / cantidad_escuelas_secundarias_con_comida
#   )
# )
# print(tabla8)
# 
# porcentajes <- tabla8$Porcentaje
# sectores <- tabla8$Categoria
# 
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias que Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# 
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# 
# png(paste0("beneficios_alimenticios/secundaria/con_comida/escuelas_secundarias_con_comida_sin_y_con_extranjeros_", year, ".png"), width = 600, height = 600)
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias que Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# dev.off()

########################## Distribución Total de Escuelas que No Ofrecen Comida, con y sin Estudiantes Extranjeros en Argentina ########################## 

# ### primario
# 
# tabla9 <- data.frame(
#   Categoria = c("Escuelas primarias sin comida con extranjeros",
#                 "Escuelas primarias sin comida sin extranjeros"
#   ),
#   Porcentaje = c(
#     (cantidad_escuelas_primarias_con_extranjeros_sin_comida * 100) / cantidad_escuelas_primarias_sin_comida,
#     (cantidad_escuelas_primarias_sin_extranjeros_sin_comida * 100) / cantidad_escuelas_primarias_sin_comida
#   )
# )
# print(tabla9)
# 
# porcentajes <- tabla9$Porcentaje
# sectores <- tabla9$Categoria
# 
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias que No Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# 
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# 
# png(paste0("beneficios_alimenticios/primaria/sin_comida/escuelas_primarias_sin_comida_sin_y_con_extranjeros_", year, ".png"), width = 600, height = 600)
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias que No Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# dev.off()
# 
# ### secundario
# 
# tabla10 <- data.frame(
#   Categoria = c("Escuelas secundarias sin comida con extranjeros",
#                 "Escuelas secundarias sin comida sin extranjeros"
#   ),
#   Porcentaje = c(
#     (cantidad_escuelas_secundarias_con_extranjeros_sin_comida * 100) / cantidad_escuelas_secundarias_sin_comida,
#     (cantidad_escuelas_secundarias_sin_extranjeros_sin_comida * 100) / cantidad_escuelas_secundarias_sin_comida
#   )
# )
# print(tabla10)
# 
# porcentajes <- tabla10$Porcentaje
# sectores <- tabla10$Categoria
# 
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias que No Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# 
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# 
# png(paste0("beneficios_alimenticios/secundaria/sin_comida/escuelas_secundarias_sin_comida_sin_y_con_extranjeros_", year, ".png"), width = 600, height = 600)
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias que No Ofrecen Comida,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# dev.off()


########################## Distribución Total de Escuelas que Ofrecen Comida o No, con y sin Estudiantes Extranjeros en Argentina ########################## 

# colors <- c("#00b3b3","#009999", "#ffcc00", "#ff9933")
# 
# ### primario
# 
# tabla5 <- tabla_resumen %>% 
#   filter(Categoria == "Escuelas primarias con comida sin extranjeros" | 
#            Categoria == "Escuelas primarias con comida con extranjeros" |
#            Categoria == "Escuelas primarias sin comida sin extranjeros" |
#            Categoria == "Escuelas primarias sin comida con extranjeros" )
# 
# print(tabla5)
# 
# porcentajes <- tabla5$Porcentaje
# sectores <- tabla5$Categoria
# 
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias que Ofrecen Comida o No,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# 
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# 
# png(paste0("beneficios_alimenticios/primaria/escuelas_primarias_con_o_sin_comida_con_o_sin_extranjeros_", year, ".png"), width = 800, height = 800)
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Primarias que Ofrecen Comida o No,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# dev.off()
# 
# ### secundario
# 
# tabla6 <- tabla_resumen %>% 
#   filter(Categoria == "Escuelas secundarias con comida sin extranjeros" | 
#            Categoria == "Escuelas secundarias con comida con extranjeros" | 
#            Categoria == "Escuelas secundarias sin comida sin extranjeros" | 
#            Categoria == "Escuelas secundarias sin comida con extranjeros")
# 
# print(tabla6)
# 
# porcentajes <- tabla6$Porcentaje
# sectores <- tabla6$Categoria
# 
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias que Ofrecen Comida o No,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# 
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# 
# png(paste0("beneficios_alimenticios/secundaria/escuelas_secundarias_con_o_sin_comida_con_o_sin_extranjeros_", year, ".png"), width = 800, height = 800)
# pie(porcentajes, labels = paste0(round(porcentajes, 2), "%"), col = colors, main = paste("Distribución Total de Escuelas Secundarias que Ofrecen Comida o No,\n con y sin Estudiantes Extranjeros en Argentina", year), border = colors)
# legend("bottomright", legend = sectores, fill = colors, cex = 0.8)
# dev.off()
