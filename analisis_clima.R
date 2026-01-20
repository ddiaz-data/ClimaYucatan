# -----------------------------------------------------------------------------
# PROYECTO: Automatización de Datos Climáticos (NASA POWER API)
# SCRIPT: analisis_clima.R
# AUTOR: M. en C. Dylan Díaz
# DESCRIPCIÓN: Comparativa de temperaturas entre zona costera (Progreso) y 
#              urbana (Mérida) para análisis de impacto ambiental.
# -----------------------------------------------------------------------------

# 1. SETUP Y LIBRERÍAS
# --------------------
# Verificamos e instalamos nasapower si no existe
if (!require("nasapower")) install.packages("nasapower")

library(nasapower) # Conexión API NASA
library(tidyverse) # Manipulación de datos y ggplot2
library(purrr)     # Iteración funcional

# 2. DEFINICIÓN DE PUNTOS DE MUESTREO
# -----------------------------------
ubicaciones <- list(
  Merida   = c(lon = -89.62, lat = 20.96),
  Progreso = c(lon = -89.66, lat = 21.28)
)

# 3. FUNCIÓN DE EXTRACCIÓN (ETL)
# ------------------------------
descargar_clima <- function(nombre_ciudad, coords) {
  message(paste(">> Conectando a NASA POWER para:", nombre_ciudad, "..."))
  
  get_power(
    community = "ag",           # Agroclimatología
    lonlat = coords,            
    pars = c("T2M", "PRECTOTCORR"), # Temperatura y Lluvia
    dates = c("2023-01-01", "2023-12-31"), 
    temporal_api = "daily"      
  ) %>%
    mutate(ciudad = nombre_ciudad) 
}

# 4. EJECUCIÓN DEL FLUJO
# ----------------------
datos_clima <- map2_dfr(names(ubicaciones), ubicaciones, descargar_clima)

# Inspección rápida
glimpse(datos_clima)

# 5. VISUALIZACIÓN DE DATOS
# -------------------------
colores <- c("Merida" = "#E63946", "Progreso" = "#457B9D")

p_clima <- ggplot(datos_clima, aes(x = YYYYMMDD, y = T2M, color = ciudad)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(se = FALSE, size = 1.2, method = "loess") +
  scale_color_manual(values = colores) +
  theme_minimal() +
  labs(
    title = "Comparación de Temperatura: Ciudad vs. Costa",
    subtitle = "Datos diarios del 2023 obtenidos vía NASA POWER",
    x = "Fecha",
    y = "Temperatura Promedio (°C)",
    color = "Ubicación",
    caption = "Fuente: NASA POWER API | Autor: Dylan Diaz"
  ) +
  theme(legend.position = "top")

print(p_clima)

# Guardamos la gráfica en alta resolución
ggsave("comparacion_clima.png", plot = p_clima, width = 10, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# NOTAS PARA FUTUROS ANÁLISIS (BIOLÓGICOS):
# Variables adicionales de interés para el estudio de FANs:
# - "RH2M": Humedad Relativa (Factor de estrés térmico).
# - "WS2M": Velocidad del Viento (Importante para dispersión de algas/nutrientes).
# - "ALLSKY_SFC_SW_DWN": Radiación Solar (Limitante para fotosíntesis fitoplanctónica).
# -----------------------------------------------------------------------------
