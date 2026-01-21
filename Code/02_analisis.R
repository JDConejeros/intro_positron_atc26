# ==============================================================================
# ANÁLISIS ------
# ==============================================================================

# Cargar configuración y estilos
source("Code/01_configuracion.R")

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS ----
# ==============================================================================

# Cargamos los datos
datos <- rio::import("Input/Data/full_data_atc26.xlsx")

# Exploración inicial
n_total <- nrow(datos)
n_variables <- ncol(datos)

# Identificamos las columnas de interés
col_cargo <- "Cargo/posición actual"

# Identificamos columnas de conocimiento (todas las que empiezan con "¿Qué nivel")
columnas_conocimiento <- names(datos)[str_detect(names(datos), "^¿Qué nivel")]; columnas_conocimiento 

# ==============================================================================
# 2. TABLA DE DISTRIBUCIÓN DE CARGO/POSICIÓN ----
# ==============================================================================

# Calculamos las distribuciones
tabla_cargo <- datos |>
  count(`Cargo/posición actual`, name = "n") |>
  mutate(
    porcentaje = round((n / sum(n)) * 100, 1),
    n_porcentaje = paste0(n, " (", porcentaje, "%)")
  ) |>
  arrange(desc(n))

tabla_cargo

# Guardamos la tabla en formato xlsx
rio::export(
  tabla_cargo,
  "Output/tabla_distribucion_cargo.xlsx"
)

# ==============================================================================
# 3. PREPARACIÓN DE DATOS PARA EL GRÁFICO -----
# ==============================================================================

# Creamos un dataset en formato largo con todas las preguntas de conocimiento
datos_likert <- datos |>
  select(ID, all_of(columnas_conocimiento)) |>
  pivot_longer(
    cols = -ID,
    names_to = "pregunta",
    values_to = "nivel"
  ) |>
  filter(!is.na(nivel)) |>
  mutate(
    pregunta_limpia = limpiar_nombres(pregunta),
    pregunta_etiqueta = mapear_etiquetas(pregunta_limpia),
    nivel_ordenado = ordenar_niveles(nivel)
  ) |>
  filter(!is.na(nivel_ordenado), !is.na(pregunta_etiqueta))

# Definimos un orden de preguntas según especificación (Pre-registros arriba, IA abajo)
orden_preguntas <- c(
  "Prácticas de Ciencia Abierta",
  "Repositorios de Datos Abiertos",
  "Lenguajes de Programación",
  "Git/Github",
  "Protocolos de Código",
  "I.A. en Ciencia Abierta",
  "Pre-registros (OSF)"
)

# Aplicamos un orden a las etiquetas vía factor
datos_likert$pregunta_etiqueta <- factor(
  datos_likert$pregunta_etiqueta,
  levels = orden_preguntas
)

# Calculamos las frecuencias por pregunta y nivel
# Orden: Ninguno (abajo) a Avanzado (arriba)
niveles_completos <- factor(c("Ninguno", "Básico", "Intermedio", "Avanzado"), 
                            levels = c("Ninguno", "Básico", "Intermedio", "Avanzado"),
                            ordered = TRUE)

# Generamos la tabla 
frecuencias <- datos_likert |>
  count(pregunta_etiqueta, nivel_ordenado) |>
  # Validamos que están todos los niveles
  # Los niveles que no tengan casos se completa con 0.
  complete(
    pregunta_etiqueta = unique(pregunta_etiqueta[!is.na(pregunta_etiqueta)]),
    nivel_ordenado = niveles_completos,
    fill = list(n = 0)
  ) |>
  filter(!is.na(pregunta_etiqueta), !is.na(nivel_ordenado)) |>
  group_by(pregunta_etiqueta) |>
  mutate(
    total = sum(n),
    porcentaje = ifelse(total > 0, (n / total) * 100, 0)
  ) |>
  ungroup()

frecuencias

# Calculamos posiciones acumuladas para barras apiladas (stacked bar)
# Esto es principalmente para que el gráfico tenga etiquetas en los valores centradas
# Orden normal: Ninguno (izquierda/0%) → Avanzado (derecha/100%)
frecuencias <- frecuencias |>
  arrange(pregunta_etiqueta, nivel_ordenado) |>
  group_by(pregunta_etiqueta) |>
  mutate(
    # Calcular posiciones: Ninguno desde 0%, Avanzado hasta 100%
    ymin = cumsum(lag(porcentaje, default = 0)),
    ymax = cumsum(porcentaje),
    y_texto = (ymin + ymax) / 2
  ) |>
  ungroup() |>
  # Verificar que todas las barras sumen 100%
  group_by(pregunta_etiqueta) |>
  mutate(
    suma_total = sum(porcentaje)
  ) |>
  ungroup()

frecuencias

# ==============================================================================
# 4. CREACIÓN DEL GRÁFICO TIPO LIKERT CON GGPLOT2 ------
# ==============================================================================

# Último ajuste a los datos para geom_col (stacked bar)
# Orden: Ninguno (izquierda/0%) → Avanzado (derecha/100%)
frecuencias_col <- frecuencias |>
  mutate(
    pregunta_num = as.numeric(pregunta_etiqueta)
  )

# Usamos fct_rev para cambiar la orientación de los niveles en el gráfico
grafico_likert <- ggplot(frecuencias_col, aes(x = pregunta_etiqueta, y = porcentaje, fill = fct_rev(nivel_ordenado))) +
  # Barras apiladas: Ninguno (izquierda/0%) → Avanzado (derecha/100%)
  geom_col(position = "stack", color = "white", linewidth = 0.3, width = 0.7) +
  # Texto con porcentajes sobre cada segmento
  # Usar las posiciones originales (y_texto) que ya están calculadas correctamente
  geom_text(
    data = frecuencias_col |>
      filter(porcentaje > 2) |> # Sacamos los % muy pequeños
      arrange(pregunta_etiqueta, nivel_ordenado),
    aes(
      x = pregunta_etiqueta,
      y = y_texto,
      label = paste0(round(porcentaje, 1), "%")
    ),
    size = 4.5,
    color = "white",
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  # Escala de colores (usando colores_likert)
  # Orden: Ninguno, Básico, Intermedio, Avanzado
  scale_fill_manual(
    name = "Nivel de conocimiento",
    values = colores_likert,
    guide = guide_legend(
      reverse = TRUE,
      direction = "horizontal",
      title.position = "top"
    ),
    drop = FALSE
  ) +
  # Escala del eje Y
  scale_y_continuous(
    name = "Porcentaje (%)",
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  # Coordenadas invertidas para que las preguntas estén arriba
  coord_flip() +
  # Tema light
  theme_light() +
  theme(
    # Ejes - texto más grande en eje Y
    axis.text.x = element_text(color = "#2d3748", size = 11),
    axis.text.y = element_text(color = "#2d3748", size = 11),
    axis.title = element_text(color = "#1a365d", size = 12, face = "bold"),
    # Background blanco
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Títulos
    plot.title = element_text(
      color = "#1a365d",
      size = 14,
      face = "bold",
      hjust = 0,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = "#4a5568",
      size = 11,
      hjust = 0,
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      color = "#718096",
      size = 8,
      hjust = 1,
      margin = margin(t = 10)
    ),
    # Leyenda arriba con rectángulos delgados (letras más grandes)
    # Orden: Ninguno (izquierda) → Avanzado (derecha)
    legend.position = "top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(color = "#1a365d", size = 11, face = "bold"),
    legend.text = element_text(color = "#2d3748", size = 10),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.2, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.x = unit(0.5, "cm"),
    legend.margin = margin(b = 15),
    # Ordenar leyenda de izquierda a derecha: Ninguno → Avanzado
    legend.box = "horizontal",
    # Márgenes
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Etiquetas
  labs(
    title = "Distribución del nivel de conocimiento sobre ciencia abierta",
    subtitle = paste0("Análisis de respuestas de participantes ATC26 (N = ", n_total, ")"),
    x = NULL
  )

grafico_likert 

# Guardamos el gráfico en PNG de buena calidad (se puede guardar en otros formatos)
ggsave(
  "Output/grafico_nivel_conocimiento.png",
  plot = grafico_likert,
  width = 14,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ==============================================================================
# 5. RESUMEN ESTADÍSTICO EN XLSX ----
# ==============================================================================

# Preparamos los datos para un resumen estadístico
datos_resumen <- datos_likert |>
  mutate(
    valor_numerico = case_when(
      nivel_ordenado == "Ninguno" ~ 0,
      nivel_ordenado == "Básico" ~ 1,
      nivel_ordenado == "Intermedio" ~ 2,
      nivel_ordenado == "Avanzado" ~ 3
    )
  )

# Calculamos estadísticas por pregunta
resumen_conocimiento <- datos_resumen |>
  group_by(pregunta_etiqueta) |>
  summarise(
    `Ninguno (0) %` = round(sum(valor_numerico == 0) / n() * 100, 1),
    `Básico (1) %` = round(sum(valor_numerico == 1) / n() * 100, 1),
    `Intermedio (2) %` = round(sum(valor_numerico == 2) / n() * 100, 1),
    `Avanzado (3) %` = round(sum(valor_numerico == 3) / n() * 100, 1),
    Promedio = round(mean(valor_numerico, na.rm = TRUE), 2),
    Mediana = median(valor_numerico, na.rm = TRUE),
    DE = round(sd(valor_numerico, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(match(pregunta_etiqueta, orden_preguntas))

# Guardar resumen en formato xlsx
rio::export(
  resumen_conocimiento,
  "Output/resumen_nivel_conocimiento.xlsx"
)
