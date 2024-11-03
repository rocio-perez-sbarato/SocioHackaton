<!-- TOC start (generated with https://github.com/derlin/bitdowntoc) -->

- [La permanencia de la desigualdad. Del derecho al hecho.](#la-permanencia-de-la-desigualdad-del-derecho-al-hecho)
  - [Descripción del proyecto](#descripción-del-proyecto)
    - [Análisis realizados](#análisis-realizados)
      - [Población](#población)
      - [Matrícula](#matrícula)
      - [Características](#características)
    - [Narración gráfica](#narración-gráfica)
  - [Estructura del repositorio](#estructura-del-repositorio)
  - [Instrucciones de Uso](#instrucciones-de-uso)
  - [Funciones y técnicas relevantes](#funciones-y-técnicas-relevantes)
  - [Trabajo futuro](#trabajo-futuro)

<!-- TOC end -->
<!-- TOC --><a name="la-permanencia-de-la-desigualdad-del-derecho-al-hecho"></a>
# La permanencia de la desigualdad. Del derecho al hecho.
**Análisis de la distribución de niños/as y adolescentes migrantes en las escuelas argentinas y las condiciones estructurales de los establecimientos educativos, entre 2011 y 2023.**

**Integrantes**

- Benitez Siciliano Paola. Estudiante de la Licenciatura en Sociología, Facultad de Ciencias Sociales.
- Castillo Paulina, Estudiante de la Licenciatura en Ciencia Política, Facultad de Ciencias Sociales.
- Carrara María Ángeles. Estudiante de la Licenciatura en Ciencias de la Computación, Facultad de Matemática, Astronomía, Física y Computación.
- Giletta Antonella Giuliana. Estudiante de la Licenciatura en Sociología, Facultad de Ciencias Sociales.
- Perez Sbarato Rocío. Estudiante de la Licenciatura en Ciencias de la Computación, Facultad de Matemática, Astronomía, Física y Computación.

<!-- TOC --><a name="descripción-del-proyecto"></a>
## Descripción del proyecto

Este repositorio consiste en el anexo de código del trabajo "La permanencia de la desigualdad. Del derecho al hecho: Análisis de la distribución de niños/as y adolescentes migrantes en las escuelas argentinas y las condiciones estructurales de los establecimientos educativos, entre 2011 y 2023" presentado en el Socio-Hackaton "Investigar en Sociales". Aquí encontrarás el código que usamos para analizar el acceso a derechos y recursos en escuelas primarias y secundarias de Argentina que cuentan con estudiantes extranjeros. 

Para llevar a cabo este análisis, usamos datos abiertos proporcionados por la Dirección de Información y Evaluación Educativa del Ministerio de Capital Humano del Gobierno Nacional. En particular, trabajamos con la "Base de Datos por Escuela", que recoge información de las escuelas de todo el país desde el relevamiento anual de educación común. De cada año de la base (2011-2023), tomamos las siguientes bases de datos:

- **Base Usuaria 2**: matrícula escolar.
- **Base Usuaria 5**: características de los establecimientos educativos.
- **Base Usuaria 6**: población escolar.

En este [documento](https://shimmering-aphid-642.notion.site/Bases-de-datos-11123fc8fd2180e2b5d0fa34473f7067?pvs=4) se encuentra una descripción de los contenidos de estas bases de datos y se señala cuáles utilizamos para el proyecto. 

<!-- TOC --><a name="análisis-realizados"></a>
### Análisis realizados

A lo largo de un mes de trabajo y con un equipo de dos personas, logramos realizar los siguientes análisis:

<!-- TOC --><a name="población"></a>
#### Población
- **Cantidad de extranjeros por provincia:** Vimos cuántos estudiantes extranjeros hay en cada provincia y específicamente qué nacionalidades de origen tienen.
- **Cantidad de extranjeros por nacionalidad:** Analizamos cuántos estudiantes hay según su país de origen.
- **Cantidad de escuelas con estudiantes extranjeros:** Contamos cuántas escuelas tienen estudiantes que vienen de otros países.
- **Cantidad de escuelas con estudiantes extranjeros según sector:** Revisamos cuántas de estas escuelas son estatales y cuántas son privadas.
- **Cantidad de escuelas con estudiantes extranjeros con servicio de alimentación:** Mostramos cuántas escuelas (primarias y secundarias) con estudiantes extranjeros proveen servicio de alimentación.

<!-- TOC --><a name="matrícula"></a>
#### Matrícula
- **División de extranjeros por nivel educativo**: Contamos cuántos estudiantes de nivel primario y secundario extranjeros hay en las escuelas de Argentina.

<!-- TOC --><a name="características"></a>
#### Características
- **Acceso a internet:** Miramos cuántas escuelas extranjeras tienen acceso a internet y si es pago o gratuito.
- **Acceso a electricidad:** Analizamos cuántas tienen electricidad y si es de la red pública u otra fuente.
- **Disponibilidad de biblioteca:** Contamos cuántas escuelas extranjeras disponen de una biblioteca.

> Son alrededor de 1000 gráficos.

<!-- TOC --><a name="narración-gráfica"></a>
### Narración gráfica

Para presentar el análisis de datos mediante una visualización interactiva y amigable utilizamos una librería del lenguaje de programación Python llamada [`streamlit`](https://streamlit.io/). 

- Para acceder al tablero desde el navegador (sin necesidad de instalar Python ni `streamlit`), pueden hacer click en el botón de abajo: 
  
  [![Streamlit App](https://static.streamlit.io/badges/streamlit_badge_black_white.svg)](https://sociohackatonapp.streamlit.app/)

- Para acceder al código en Python de nuestro tablero, pueden ir [este](https://github.com/angelescch/SocioHackatonApp/) repositorio de Github. Ahí están las instrucciones sobre cómo ejecutar el tablero de manera local y las dependencias que se deben instalar.

<!-- TOC --><a name="estructura-del-repositorio"></a>
## Estructura del repositorio

```plaintext
/SocioHackaton
│
├── README.md                        # Descripción del proyecto e instrucciones de uso
├── bd/                              # Datos crudos organizados por años
├── data/                            # Datos procesados organizados por años
│   ├── 2011/
│   │   ├── Poblacion.csv       # Base de datos de población (2011)
│   │   ├── Caracteristicas.csv # Base de datos de características (2011)
│   │   └── ...                      # Otras bases de datos relevantes de 2011
│   ├── ...                          # Repetir para años 2012 a 2023
│
├── scripts/                         # Scripts de generación de gráficos y análisis
│   ├── ...
│
├── notebooks/                       # Notebooks/Colabs con el mismo código que los scripts
│   ├── graficos_sector.ipynb
│   ├── graficos_caracteristicas.ipynb
│
├── output/                          # Carpeta para los gráficos generados
│   ├── ...
│
└── docs/                            # Documentación adicional
    ├── trabajo_final.pdf            # Trabajo

```

<!-- TOC --><a name="instrucciones-de-uso"></a>
## Instrucciones de Uso

**Clonar el repositorio**

Para clonar este repositorio, abrí tu terminal y ejecutá este comando:

```bash
git clone https://github.com/rocio-perez-sbarato/SocioHackaton.git
```

Después, entrá a la carpeta del proyecto:

```bash
cd SocioHackaton
```

**Ejecutar los Scripts**
   - Cada archivo se corre por separado y genera las tablas y gráficos. Si necesitás ejecutar un archivo que depende de otro, eso estará indicado al principio del archivo. 
     - En esos casos, recomendamos usar los Colabs que están en la carpeta de notebooks, donde el flujo del código está armado de forma secuencial.

**Documentación**
   - En la carpeta de `docs` se encuentra el trabajo entregado en el SocioHackaton.
  
<!-- TOC --><a name="funciones-y-técnicas-relevantes"></a>
## Funciones y técnicas relevantes

En este proyecto, usamos el lenguaje R, que es muy común para el análisis de datos. Nos familiarizarnos con este y decidimos utilizar las siguientes librerías y funciones para crear nuestros gráficos:

- **Librerías necesarias:**
  ```r
  library(ggplot2)   # Para crear gráficos
  library(dplyr)     # Para manipulación de datos
  library(tidyr)     # Para transformar datos
  library(writexlsx) # Para guardar resultados en Excel
  ```
- **Gráficos:**
  - **`pie()`**: para hacer gráficos de torta.
  - **`ggplot()`**: lo usamos para crear gráficos de barras.
- **Procesar datos:**
  - **`read.csv()`** y **`write.csv()`**: para leer y escribir archivos CSV.
  - **`write_xlsx()`**: para guardar nuestros resultados en formato Excel.
- **Operaciones de tablas:** 
  - **`select()`**: para elegir columnas específicas.
  - **`filter()`**: para filtrar filas según ciertas condiciones.
  - **`mutate()`**: para agregar nuevas columnas o modificar las que ya tenemos.
  - **`group_by()`**: para agrupar los datos.
  - **`left_join()`** y **`semi_join()`**: para combinar datos de distintas tablas.

Recomendamos echarle un vistazo a estas páginas para aprender más sobre estas funciones y entender mejor nuestro código.

- [Gráficos de sectores en R](https://r-coder.com/grafico-sectores-r/)
- [Gráfico de barras agrupadas en R](https://www.statology.org/grouped-barplot-in-r/)
- [Introducción a dplyr](https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/dplyr.html)
- [Uniones de datos con dplyr](https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti)

<!-- TOC --><a name="trabajo-futuro"></a>
## Trabajo futuro

Si tuvieramos más tiempo y más personas en el equipo, sería interesante realizar los siguientes ajustes:

- **Gráficos**
  - Cantidad de escuelas extranjeras en ámbitos urbanos y rurales
  - Cantidad de escuelas extranjeras con acceso a laboratorio
  - Cantidad de escuelas rurales y urbanas con electricidad
  - Análisis de tendencias año a año para el acceso a derechos y recursos de las escuelas con extranjeros
- **Unificación de código:** Incorporar una variable `year` en cada archivo de análisis de datos para simplificar el código y hacerlo más eficiente. 
- **Unificación de entornos de trabajo:** Incorporar un Jupyter Notebook para cada análisis, logrando mayor accesibilidad en el código para personas interesadas pero sin formación técnica.
- **Unificación de orden de barras:** Definir el criterio óptimo para ordenar las barras de los gráficos de barras e incorporar este criterio en los gráficos ya realizados.
