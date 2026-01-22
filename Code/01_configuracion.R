# ==============================================================================
# CONFIGURACIÓN Y ESTILOS ------
# ==============================================================================

# Cargar librerías necesarias (instala si es necesario)
load_install <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  }
  invisible(pkgs)
}

paquetes <- c(
  "rio",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "scales",
  "forcats"
)

load_install(paquetes)

# Configuración de opciones globales
options(scipen = 999) # Desactiva la notación científica

# ==============================================================================
# PALETA DE COLORES ------
# ==============================================================================

# Colores para gráficos tipo Likert (stacked bar)
# Orden: Ninguno, Básico, Intermedio, Avanzado
# Ordenamos los colores de forma inversa para que coincida con el orden en ggplot2
colores_likert <- c(
  "#2c5282",   # Azul oscuro (avanzado) - combina con el celeste
  "#4299e1",  # Azul celeste (intermedio)
  "#ed8936",  # Naranja (básico)
  "#c53030"  # Rojo (ninguno)
)

# ==============================================================================
# FUNCIONES AUXILIARES -------
# ==============================================================================

# Función para limpiar nombres de columnas usando stringr
limpiar_nombres <- function(x) {
  x |>
    stringr::str_remove("¿Qué nivel de conocimiento crees que tienes sobre ") |>
    stringr::str_remove("\\?") |>
    stringr::str_trim()
}

# Función para ordenar niveles de conocimiento (solo 4 niveles)
# Orden: Ninguno (abajo) a Avanzado (arriba) para barras apiladas
ordenar_niveles <- function(x) {
  niveles <- c("Ninguno", "Básico", "Intermedio", "Avanzado")
  factor(x, levels = niveles, ordered = TRUE)
}

# Función para mapear nombres de preguntas a etiquetas cortas
# Tengo que ser consistente con estas etiquetas
mapear_etiquetas <- function(x) {
  resultado <- x |>
    stringr::str_replace("Pre-Registros.*OSF.*", "Pre-registros (OSF)") |>
    stringr::str_replace(".*prácticas de ciencia abierta.*", "Prácticas de Ciencia Abierta") |>
    stringr::str_replace(".*repositorios de Datos Abiertos.*", "Repositorios de Datos Abiertos") |>
    stringr::str_replace(".*lenguajes de programación.*R.*PYTHON.*JULIA.*", "Lenguajes de Programación") |>
    stringr::str_replace(".*GIT.*GITHUB.*", "Git/Github") |>
    stringr::str_replace(".*protocolos de apertura y reproducibilidad de código.*", "Protocolos de Código") |>
    stringr::str_replace(".*uso de Inteligencia Artificial.*ciencia abierta.*", "I.A. en Ciencia Abierta")
  
  return(resultado)
}
