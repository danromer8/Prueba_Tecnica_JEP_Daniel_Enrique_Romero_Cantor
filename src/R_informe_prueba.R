# Cargar las librerías necesarias
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(tidyr) # Asegurar que tidyr está disponible para pivot_longer

# Cargar los datos de las fuentes A y B
A <- read_excel("C:/Users/danromer8/Downloads/Prueba_Tecnica/input/A.xlsx", 
                col_types = c("text", "text", "text", 
                              "text", "text", "text", "date", "text", 
                              "text", "numeric", "text"))

B <- read_excel("C:/Users/danromer8/Downloads/Prueba_Tecnica/input/B.xlsx", 
                col_types = c("text", "text", "text", 
                              "text", "text", "text", "date", "text", 
                              "text", "numeric", "text"))

# Normalizar los datos
normalize_data <- function(df) {
  df %>% mutate(
    NOMBRE1 = tolower(trimws(NOMBRE1)),
    APELLIDO1 = tolower(trimws(APELLIDO1)),
    MUNICIPIO = tolower(trimws(MUNICIPIO)),
    FECHA_HECHOS = as.Date(FECHA_HECHOS, format = "%Y-%m-%d")
  )
}

A <- normalize_data(A)
B <- normalize_data(B)

# Combinar los datos en una sola tabla
datos_consolidados <- bind_rows(A %>% mutate(fuente = "A"), 
                                B %>% mutate(fuente = "B")) %>%
  arrange(NUMERO_DOCUMENTO, fuente)

# Crear identificadores basados en coincidencias por número de documento
datos_consolidados <- datos_consolidados %>%
  mutate(
    Identificador = as.integer(as.factor(NUMERO_DOCUMENTO))
  )

# Filtrar datos únicos por Identificador
datos_unicos <- datos_consolidados %>%
  group_by(Identificador) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Análisis de datos relevantes incluyendo grupos etarios
datos_unicos <- datos_unicos %>% mutate(
  Grupo_Etario = case_when(
    as.numeric(EDAD) < 18 ~ "Menor de 18",
    as.numeric(EDAD) >= 18 & as.numeric(EDAD) <= 30 ~ "18-30",
    as.numeric(EDAD) > 30 & as.numeric(EDAD) <= 50 ~ "31-50",
    as.numeric(EDAD) > 50 ~ "Más de 50",
    TRUE ~ "Desconocido"
  )
)

# Tabla de contingencia para reporte por fuente
resumen_fuentes <- datos_consolidados %>%
  group_by(Identificador) %>%
  summarise(
    fuentes = paste(unique(fuente), collapse = ", "),
    .groups = 'drop'
  ) %>%
  mutate(
    categoria = case_when(
      fuentes == "A, B" ~ "Ambas",
      fuentes == "A" ~ "Solo A",
      fuentes == "B" ~ "Solo B",
      TRUE ~ "Desconocido"
    )
  )

# Resumen de las categorías
tabla_categorias <- resumen_fuentes %>%
  group_by(categoria) %>%
  summarise(
    Total = n(),
    .groups = 'drop'
  )

# Resumen para gráfica por sexo
resumen_por_sexo <- datos_unicos %>%
  group_by(DEPARTAMENTO, SEXO) %>%
  summarise(
    Total = n(),
    .groups = 'drop'
  )

# Resumen para gráfica por grupo etario
resumen_por_grupo_etario <- datos_unicos %>%
  group_by(DEPARTAMENTO, Grupo_Etario) %>%
  summarise(
    Total = n(),
    .groups = 'drop'
  )

# Crear gráfica de víctimas por fuente
grafica_categorias <- ggplot(tabla_categorias, aes(x = categoria, y = Total, fill = categoria)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribución de Víctimas por Fuentes",
    x = "Categoría",
    y = "Número de Víctimas",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = Total), vjust = -0.5, size = 4)

# Guardar gráfica de víctimas por fuente
ggsave("C:/Users/danromer8/Downloads/Prueba_Tecnica/output/grafica_categorias.png", plot = grafica_categorias, width = 10, height = 6)

# Crear gráfica dividida por sexo
grafica_por_sexo <- ggplot(resumen_por_sexo, aes(x = reorder(DEPARTAMENTO, -Total), y = Total, fill = SEXO)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(
    title = "Homicidios por Departamento y Sexo",
    x = "Departamento",
    y = "Número de Víctimas",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top"
  ) +
  geom_text(aes(label = Total), position = position_dodge(width = 0.9), size = 3, color = "black")

# Guardar gráfica por sexo
ggsave("C:/Users/danromer8/Downloads/Prueba_Tecnica/output/grafica_por_sexo.png", plot = grafica_por_sexo, width = 10, height = 6)

# Crear gráfica dividida por grupo etario
grafica_por_grupo_etario <- ggplot(resumen_por_grupo_etario, aes(x = reorder(DEPARTAMENTO, -Total), y = Total, fill = Grupo_Etario)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Homicidios por Departamento y Grupo Etario",
    x = "Departamento",
    y = "Número de Víctimas",
    fill = "Grupo Etario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top"
  ) +
  geom_text(aes(label = Total), position = position_stack(vjust = 0.5), size = 3, color = "black")

# Guardar gráfica por grupo etario
ggsave("C:/Users/danromer8/Downloads/Prueba_Tecnica/output/grafica_por_grupo_etario.png", plot = grafica_por_grupo_etario, width = 10, height = 6)

