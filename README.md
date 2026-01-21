# <img src="PresRep/Images/ATC26.png" alt="ATC26 Logo" width="5%"/> PreWorkshop 

Estos son los materiales del preworkshop. El objetivo es introducir al uso de algunas herramientas de positron para el trabajo sobre datos. 

## Estructura del Proyecto

### `Code/`
Contiene mis scripts de análisis en R organizados en dos archivos principales:

- **`01_configuracion.R`**: Configuración inicial del entorno, carga de librerías, definición de paletas de colores personalizadas (ATC26), funciones auxiliares para limpieza de datos y temas personalizados para gráficos.
- **`02_analisis.R`**: Script principal de análisis que ejecuta la exploración de datos, genera tablas de distribución y crea visualizaciones tipo Likert del nivel de conocimiento en diferentes áreas de ciencia abierta.

### `Input/`
Directorio con los datos de entrada del proyecto:

- **`Data/`**: Contiene el archivo `full_data_atc26.xlsx` con los datos crudos de la encuesta que utilizo para el análisis.
- **`Images/`**: Imágenes y recursos visuales que utilizo en el proyecto.

### `Output/`
Archivos generados por mis scripts de análisis:

- **`tabla_distribucion_cargo.xlsx`**: Tabla con la distribución de participantes según su cargo/posición actual.
- **`resumen_nivel_conocimiento.xlsx`**: Tabla resumen con estadísticas descriptivas (porcentajes, promedio, mediana, desviación estándar) del nivel de conocimiento por área.
- **`grafico_nivel_conocimiento.png`**: Gráfico tipo Likert (barras apiladas) mostrando la distribución del nivel de conocimiento en las diferentes áreas evaluadas.

### `Reporte/`
Documento completo del análisis que genero con Quarto:

- **`reporte_atc26.qmd`**: Documento principal en formato Quarto que integra tablas y gráficos del análisis.
- **`_quarto.yml`**: Configuración de formatos de salida (PDF, Word, HTML).
- **`custom.scss`**: Estilos personalizados para la versión HTML del reporte.
- **`render_reporte.R`**: Script que uso para renderizar el reporte en todos los formatos o uno específico.

Para generar el reporte, ejecuto:
```bash
cd Reporte
Rscript render_reporte.R
```

O usando Quarto directamente:
```bash
cd Reporte
quarto render reporte_atc26.qmd
```

### `PresRep/`
Presentación sobre Positron y herramientas de ciencia abierta que preparé:

- **`intro_positron.qmd`**: Presentación en formato Quarto/Reveal.js sobre introducción a Positron.
- **`custom.scss`**: Estilos personalizados para la presentación.
- **`Images/`**: Imágenes que utilizo en la presentación, incluyendo el logo ATC26.

## Requisitos

- **R** (versión 4.0 o superior)
- **Quarto** (para generar reportes y presentaciones)
- **LaTeX** (solo necesario para generar PDFs)

### Paquetes R necesarios

Los principales paquetes utilizados son:
- `tidyverse` (dplyr, ggplot2, tidyr, stringr, forcats)
- `readxl` / `rio` (lectura de archivos Excel)
- `writexl` / `rio` (escritura de archivos Excel)
- `flextable` / `officer` (tablas compatibles cross-format)
- `quarto` (renderizado de documentos)

Instalar con:
```r
install.packages(c("tidyverse", "rio", "flextable", "officer", "quarto"))
```

## Uso

1. **Ejecutar análisis**: 
   ```r
   source("Code/01_configuracion.R")
   source("Code/02_analisis.R")
   ```

2. **Generar reporte**:
   ```bash
   cd Reporte
   Rscript render_reporte.R
   ```
   
Los scripts generan automáticamente las tablas y gráficos en la carpeta `Output/`, que luego se integran en el reporte final.

## Autor

**José Daniel Conejeros**  
ATC26  
jdconejeros@uc.cl

---

*Este proyecto fue desarrollado como parte del análisis de la encuesta ATC26 sobre conocimiento en ciencia abierta.*
