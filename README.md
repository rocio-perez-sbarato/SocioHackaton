# SocioHackaton

/SocioHackaton
/proyecto-escuelas-inmigrantes
│
├── README.md                        # Descripción del proyecto, objetivos, instrucciones de uso
├── requirements.txt                 # Librerías necesarias si usás Python o R (por ejemplo: ggplot2, matplotlib, pandas)
├── data/                            # Carpeta para datos crudos y procesados, organizada por años
│   ├── 2011/
│   │   ├── poblacion_2011.csv       # Base de datos de población (2011)
│   │   ├── caracteristicas_2011.csv # Base de datos de características de las escuelas (2011)
│   │   └── ...     # Otros datos relevantes de 2011
│   ├── 2012/
│   │   ├── poblacion_2012.csv       # Base de datos de población (2012)
│   │   ├── caracteristicas_2012.csv # Base de datos de características de las escuelas (2012)
│   │   └── ...     # Otros datos relevantes de 2012
│   ├── 2013/
│   │   ├── poblacion_2013.csv       # Base de datos de población (2013)
│   │   ├── caracteristicas_2013.csv # Base de datos de características de las escuelas (2013)
│   │   └── ...    # Otros datos relevantes de 2013
│   ├── ...                          # Repetir para años 2014 a 2023
│   ├── 2023/
│   │   ├── poblacion_2023.csv       # Base de datos de población (2023)
│   │   ├── caracteristicas_2023.csv # Base de datos de características de las escuelas (2023)
│   │   └── ...   # Otros datos relevantes de 2023
│
├── scripts/                         # Scripts de generación de gráficos y análisis
│   ├── graficos_sectores.R          # Script para gráficos de sectores generales
│   ├── graficos_sectores_provincia.R # Script para gráficos de sectores por provincia
│   ├── graficos_internet.R          # Script para gráficos sobre internet en escuelas
│   ├── graficos_internet_provincia.R # Script para gráficos de internet por provincia
│   ├── graficos_electricidad.R      # Script para gráficos de electricidad
│   ├── graficos_electricidad_provincia.R # Script para gráficos de electricidad por provincia
│   ├── graficos_biblioteca.R        # Script para gráficos de bibliotecas
│   ├── graficos_biblioteca_provincia.R # Script para gráficos de bibliotecas por provincia
│   ├── cruce_bases.R                # Script para el cruce de bases de datos
│   ├── filtrado_extranjeros.R       # Script para filtrar las escuelas con y sin extranjeros
│   └── helpers.R                    # Funciones auxiliares si las necesitas
│
├── output/                          # Carpeta para los gráficos generados
│   ├── sectores/                    # Carpeta para gráficos de sectores
│   ├── internet/                    # Carpeta para gráficos de internet
│   ├── electricidad/                # Carpeta para gráficos de electricidad
│   ├── biblioteca/                  # Carpeta para gráficos de bibliotecas
│   └── cruce_bases/                 # Carpeta para resultados del cruce de bases de datos
│
└── docs/                            # Documentación adicional
    ├── informe.pdf                  # Informe 
    └── instrucciones.md             # Instrucciones detalladas para ejecutar los scripts


