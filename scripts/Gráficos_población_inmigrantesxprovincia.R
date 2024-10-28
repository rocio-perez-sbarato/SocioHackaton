library(dplyr)
library(readr)

poblacion <- read.csv("sociales-hack/bd/2020/Base 6- Población.csv", sep = ";",fileEncoding = "ISO-8859-1")

str(data)

colnames(data) <- make.names(colnames(data)) # supuestamente hace que se vean bien los nombres que tienen tildes en las columnas pero no funca

print(colnames(data))

resultado <- poblacion %>%
  rowwise() %>% 
  mutate(
    extranjeros = sum(c_across(c(Bolivia...Primaria, Brasil...Primaria, Chile...Primaria, Colombia...Primaria, Ecuador...Primaria, Paraguay...Primaria, Perú...Primaria, Uruguay...Primaria,
                          Venezuela...Primaria, Otros.países.de.América...Primaria, Europa...Primaria, Asia...Primaria, Otros...Primaria, Bolivia...Secundaria, Brasil...Secundaria, Chile...Secundaria, Colombia...Secundaria, Ecuador...Secundaria, Paraguay...Secundaria, Perú...Secundaria, Uruguay...Secundaria,
                                                 Venezuela...Secundaria, Otros.países.de.América...Secundaria, Europa...Secundaria, Asia...Secundaria, Otros...Secundaria
                          )), na.rm = TRUE),
    bolivia=sum(c_across(c(Bolivia...Primaria, Bolivia...Secundaria)), na.rm = TRUE),
    brasil=sum(c_across(c(Brasil...Primaria, Brasil...Secundaria)), na.rm = TRUE),
    chile=sum(c_across(c(Chile...Primaria, Chile...Secundaria)), na.rm = TRUE),
    colombia=sum(c_across(c(Colombia...Primaria, Colombia...Secundaria)), na.rm = TRUE),
    ecuador=sum(c_across(c(Ecuador...Primaria, Ecuador...Secundaria)), na.rm = TRUE),
    paraguay=sum(c_across(c(Paraguay...Primaria, Paraguay...Secundaria)), na.rm = TRUE),
    peru=sum(c_across(c(Perú...Primaria, Perú...Secundaria)), na.rm = TRUE),
    uruguay=sum(c_across(c(Uruguay...Primaria, Uruguay...Secundaria)), na.rm = TRUE),
    venezuela=sum(c_across(c(Venezuela...Primaria, Venezuela...Secundaria)), na.rm = TRUE),
    otros_america=sum(c_across(c(Otros.países.de.América...Primaria, Otros.países.de.América...Secundaria)), na.rm = TRUE),
    europa=sum(c_across(c(Europa...Primaria, Europa...Secundaria)), na.rm = TRUE),
    asia=sum(c_across(c(Asia...Primaria, Asia...Secundaria)), na.rm = TRUE),
    otros=sum(c_across(c(Otros...Primaria, Otros...Secundaria)), na.rm = TRUE)
  )

print(resultado)

ii <- resultado %>%
  select(ID1,provincia,extranjeros,bolivia,brasil,chile,colombia,ecuador,paraguay,peru,uruguay,venezuela,otros_america,europa,asia,otros) %>%
  print()

write.csv(ii, "resultado1.csv", row.names = FALSE)


porcentaje_nacionalidades_por_provincia <- ii %>%
  group_by(provincia) %>%  
  summarise(
    total_extranjeros = sum(extranjeros, na.rm = TRUE),
    bolivia = sum(bolivia, na.rm = TRUE),
    brasil = sum(brasil, na.rm = TRUE),
    chile = sum(chile, na.rm = TRUE),
    colombia = sum(colombia, na.rm = TRUE),
    ecuador = sum(ecuador, na.rm = TRUE),
    paraguay = sum(paraguay, na.rm = TRUE),
    peru = sum(peru, na.rm = TRUE),
    uruguay = sum(uruguay, na.rm = TRUE),
    venezuela = sum(venezuela, na.rm = TRUE),
    otros_america = sum(otros_america, na.rm = TRUE),
    europa = sum(europa, na.rm = TRUE),
    asia = sum(asia, na.rm = TRUE),
    otros = sum(otros, na.rm = TRUE)
  ) %>%
  mutate(
    porcentaje_bolivia = (bolivia / total_extranjeros) * 100,
    porcentaje_brasil = (brasil / total_extranjeros) * 100,
    porcentaje_chile = (chile / total_extranjeros) * 100,
    porcentaje_colombia = (colombia / total_extranjeros) * 100,
    porcentaje_ecuador = (ecuador / total_extranjeros) * 100,
    porcentaje_paraguay = (paraguay / total_extranjeros) * 100,
    porcentaje_peru = (peru / total_extranjeros) * 100,
    porcentaje_uruguay = (uruguay / total_extranjeros) * 100,
    porcentaje_venezuela = (venezuela / total_extranjeros) * 100,
    porcentaje_otros_america = (otros_america / total_extranjeros) * 100,
    porcentaje_europa = (europa / total_extranjeros) * 100,
    porcentaje_asia = (asia / total_extranjeros) * 100,
    porcentaje_otros = (otros / total_extranjeros) * 100
  )

# porcentajes a 2 decimales
porcentaje_nacionalidades_por_provincia <- porcentaje_nacionalidades_por_provincia %>%
  mutate(
    porcentaje_bolivia = round(porcentaje_bolivia, 2),
    porcentaje_brasil = round(porcentaje_brasil, 2),
    porcentaje_chile = round(porcentaje_chile, 2),
    porcentaje_colombia = round(porcentaje_colombia, 2),
    porcentaje_ecuador = round(porcentaje_ecuador, 2),
    porcentaje_paraguay = round(porcentaje_paraguay, 2),
    porcentaje_peru = round(porcentaje_peru, 2),
    porcentaje_uruguay = round(porcentaje_uruguay, 2),
    porcentaje_venezuela = round(porcentaje_venezuela, 2),
    porcentaje_otros_america = round(porcentaje_otros_america, 2),
    porcentaje_europa = round(porcentaje_europa, 2),
    porcentaje_asia = round(porcentaje_asia, 2),
    porcentaje_otros = round(porcentaje_otros, 2)
  )


print(porcentaje_nacionalidades_por_provincia)


porcentaje_nacionalidades_por_provincia <- porcentaje_nacionalidades_por_provincia %>%
  select(provincia, total_extranjeros, 
         porcentaje_bolivia, porcentaje_brasil, porcentaje_chile, 
         porcentaje_colombia, porcentaje_ecuador, porcentaje_paraguay, 
         porcentaje_peru, porcentaje_uruguay, porcentaje_venezuela, 
         porcentaje_otros_america, porcentaje_europa, porcentaje_asia, 
         porcentaje_otros)


print(porcentaje_nacionalidades_por_provincia)


write.csv(porcentaje_nacionalidades_por_provincia, "porcentaje_extranjeros_por_provincia.csv", row.names = FALSE)

###########################################


library(ggplot2)
library(tidyr)
library(dplyr)

# Convertir los datos de ancho a largo para usar en ggplot
porcentaje_largo <- porcentaje_nacionalidades_por_provincia %>%
  pivot_longer(
    cols = starts_with("porcentaje_"),
    names_to = "nacionalidad",
    values_to = "porcentaje"
  ) %>%
  mutate(nacionalidad = gsub("porcentaje_", "", nacionalidad)) %>%
  select(provincia, nacionalidad, porcentaje) 


# Obtener la lista de provincias
provincias <- unique(porcentaje_largo$provincia)


colores <- c(
  "bolivia" = "#E24E42", 
  "brasil" = "#3357FF", 
  "chile" = "#A5D55C", 
  "colombia" = "#F9A300", 
  "ecuador" = "#1D3557", 
  "paraguay" = "#F75C7E",
  "peru" = "#808000", 
  "uruguay" = "#E7BBA0", 
  "venezuela" = "#A8DADC",
  "otros_america" = "#457B9D", 
  "europa" = "#7C4A88", 
  "asia" = "#F6D371", 
  "otros" = "#6F9B8A"
)


for (provincia in provincias) {
  # Filtrar los datos para la provincia actual
  datos_provincia <- porcentaje_largo %>% filter(provincia == !!provincia)

  # Convertir 'nacionalidad' a factor con el nivel correspondiente a la paleta de colores
  datos_provincia$nacionalidad <- factor(datos_provincia$nacionalidad, levels = names(colores))

  # Crear una vector de etiquetas con nombres y porcentajes
  etiquetas <- paste0(
    datos_provincia$nacionalidad, ": ", 
    round(datos_provincia$porcentaje, 1), 
    "%"
  )

  
  # Crear el gráfico
  p <- ggplot(datos_provincia, aes(x = "", y = porcentaje, fill = nacionalidad)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = paste("Distribución de Nacionalidades Extranjeras en", provincia), fill = "Nacionalidad") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black"),  # Fondo blanco con borde negro
      legend.position = "right"
    ) +
    scale_fill_manual(values = colores, labels = etiquetas) +  # Usar los colores personalizados y las etiquetas
    guides(fill = guide_legend(title = "Nacionalidad"))
  
  # Guardar el gráfico como imagen PNG
  ggsave(filename = paste0("grafico_torta_", gsub(" ", "_", provincia), ".png"), plot = p, width = 8, height = 6)
}








###########################################

total_europeos_por_provincia <- porcentaje_nacionalidades_por_provincia %>%
  group_by(provincia) %>%
  summarise(total_europeos = sum(europa, na.rm = TRUE))

# 2. Calcular el total de europeos en el país
total_europeos_nacional <- sum(total_europeos_por_provincia$total_europeos, na.rm = TRUE)

# 3. Calcular el porcentaje de europeos en cada provincia
porcentaje_europeos_por_provincia <- total_europeos_por_provincia %>%
  mutate(porcentaje = (total_europeos / total_europeos_nacional) * 100)

# 4. Redondear el porcentaje a 2 decimales
porcentaje_europeos_por_provincia <- porcentaje_europeos_por_provincia %>%
  mutate(porcentaje = round(porcentaje, 2))

# 5. Ver el resultado
print(porcentaje_europeos_por_provincia)

# 6. Guardar el resultado en un CSV
write.csv(porcentaje_europeos_por_provincia, "porcentaje_europeos_por_provincia.csv", row.names = FALSE)