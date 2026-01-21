# ==============================================================================
# CONFIGURACIÓN Y ESTILOS ------
# ==============================================================================

# Cargar librerías necesarias
library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

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
    str_remove("¿Qué nivel de conocimiento crees que tienes sobre ") |>
    str_remove("\\?") |>
    str_trim()
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
    str_replace("Pre-Registros.*OSF.*", "Pre-registros (OSF)") |>
    str_replace(".*prácticas de ciencia abierta.*", "Prácticas de Ciencia Abierta") |>
    str_replace(".*repositorios de Datos Abiertos.*", "Repositorios de Datos Abiertos") |>
    str_replace(".*lenguajes de programación.*R.*PYTHON.*JULIA.*", "Lenguajes de Programación") |>
    str_replace(".*GIT.*GITHUB.*", "Git/Github") |>
    str_replace(".*protocolos de apertura y reproducibilidad de código.*", "Protocolos de Código") |>
    str_replace(".*uso de Inteligencia Artificial.*ciencia abierta.*", "I.A. en Ciencia Abierta")
  
  return(resultado)
}
