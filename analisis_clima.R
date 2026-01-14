# ---------------------------------------------
# PROYECTO: Análisis de Clima en Yucatán
# AUTOR: Dylan Diaz
# ---------------------------------------------

# 1. Instalamos el paquete de la NASA (solo una vez)
if (!require("nasapower")) install.packages("nasapower")

library(nasapower) # Conexión a la NASA
library(tidyverse) # Manejo de datos
library(purrr)

# 2. Definimos las coordenadas de las ciudades
ubicaciones <- list(
  Merida   = c(lon = -89.62, lat = 20.96),
  Progreso = c(lon = -89.66, lat = 21.28)
)

# 3. Función para descargar los datos de una ciudad
descargar_clima <- function(nombre_ciudad, coords) {
  message(paste("Descargando datos para:", nombre_ciudad, "..."))
  
  get_power(
    community = "ag",           # Agroclimatología
    lonlat = coords,            # Coordenadas
    pars = c("T2M", "PRECTOTCORR"), # Temperatura y Lluvia
    dates = c("2023-01-01", "2023-12-31"), 
    temporal_api = "daily"      # <--- AQUÍ ESTABA EL CAMBIO (antes era temporal_average)
  ) %>%
    mutate(ciudad = nombre_ciudad) 
}

# 4. VOLVEMOS A EJECUTAR LA DESCARGA
datos_clima <- map2_dfr(names(ubicaciones), ubicaciones, descargar_clima)

# 5. Vemos el resultado
glimpse(datos_clima)

# Definimos colores personalizados (Estilo científico)
colores <- c("Merida" = "#E63946", "Progreso" = "#457B9D")

ggplot(datos_clima, aes(x = YYYYMMDD, y = T2M, color = ciudad)) +
  # 1. Ponemos los puntos (los datos reales) pero un poco transparentes
  geom_point(alpha = 0.4, size = 1) +
  
  # 2. Agregamos una línea de tendencia suave para ver el patrón claro
  geom_smooth(se = FALSE, size = 1.2) +
  
  # 3. Personalizamos colores y tema
  scale_color_manual(values = colores) +
  theme_minimal() +
  
  # 4. Etiquetas profesionales
  labs(
    title = "Comparación de Temperatura: Ciudad vs. Costa",
    subtitle = "Datos diarios del 2023 obtenidos vía NASA POWER",
    x = "Fecha",
    y = "Temperatura Promedio (°C)",
    color = "Ubicación",
    caption = "Fuente: NASA POWER API | Autor: Dylan Diaz"
  ) +
  
  # 5. Ajustes finos (poner la leyenda arriba)
  theme(legend.position = "top")

#Otras variables que podrian ser funcionales
#"RH2M" -> Humedad Relativa (Vital para sensación térmica)
#"WS2M" -> Velocidad del Viento (Importante para dispersión de algas/FANs)
#"ALLSKY_SFC_SW_DWN" -> Radiación Solar (Clave para fotosíntesis)

# Guardamos la gráfica en la carpeta del proyecto
ggsave("comparacion_clima.png", width = 10, height = 6, dpi = 300)
