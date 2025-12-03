
rm(list = ls())
require(pacman)
p_load(
  tidyverse,   # Conjunto de paquetes para manipulaci?n y visualizaci?n de datos (dplyr, ggplot2, etc.)
  rio,         # Importa y exporta datos en m?ltiples formatos (CSV, Excel, RDS, etc.)
  osmdata,     # Descarga datos geoespaciales desde OpenStreetMap
  sf,          # Trabaja con datos espaciales bajo el est?ndar "Simple Features"
  SpatialKDE,  # Permite hacer estimaciones de densidad de kernel (KDE) espaciales
  tmap,        # Visualizaci?n de datos espaciales: mapas est?ticos o interactivos
  gridExtra,   # Permite organizar m?ltiples gr?ficos en una misma figura
  evmix,       # Modelos de colas y extremos en distribuciones estad?sticas
  units        # Manejo de unidades f?sicas (km, m?, etc.)
)

# Directorio
setwd("C:/Users/samel/OneDrive/Datos adjuntos/Universidad de los Andes/Tesis")
#data <- import("data/em2021.csv")
data_med <- import("data/med_ecv_2024.xlsx")

