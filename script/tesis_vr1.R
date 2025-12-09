## --- TRABAJO FINAL DE ECONOMÍA URBANA --- ##
## --- SANTIAGO MELO MONTERO --- #
## --- UNIVERSIDAD DE LOS ANDES --- #

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

## ---- sHIFT ----
# A partir de datos abiertos, recolectamos la entrada de venezolanos a Colombia desde 2012
entryen <- import("data/Entradas_de_extranjeros_a_Colombia_20251208.csv")

# Convertir Total a numérica y sumar por año
suma_por_año <- entryen %>%
  mutate(
    Total_numerico = as.numeric(gsub(",", "", Total))
  ) %>%
  group_by(Año) %>%
  summarise(
    Total_por_año = sum(Total_numerico, na.rm = TRUE),
    Registros = n()
  ) %>%
  arrange(Año)

print(suma_por_año)


## ---- SHARE ----
# Simulación del número de venezolanos por localidad (2005)
# Total: 5,818 personas

# Definir los pesos por localidad
pesos <- c(
  Chapinero = 1.050,
  Teusaquillo = 0.900,
  Usaquen = 0.880,
  Suba = 0.650,
  Barrios_Unidos = 0.430,
  Engativa = 0.360,
  Fontibon = 0.300,
  Kennedy = 0.280,
  Antonio_Narino = 0.165,
  La_Candelaria = 0.050,
  Santa_Fe = 0.095,
  Puente_Aranda = 0.190,
  Los_Martires = 0.090,
  Tunjuelito = 0.045,
  San_Cristobal = 0.135,
  Rafael_Uribe = 0.115,
  Ciudad_Bolivar = 0.040,
  Usme = 0.043,
  Bosa = 0.155
)

# Normalizar los pesos para que sumen el total deseado
total_venezolanos <- 5818

proporciones <- pesos / sum(pesos)
venezolanos_localidad <- round(proporciones * total_venezolanos)

# Construir data frame final
ven_share_2005 <- data.frame(
  localidad = names(venezolanos_localidad),
  venezolanos_2005 = venezolanos_localidad
)

# Revisar que la suma sea exactamente 5818
ven_share_2005
total_ven <- sum(ven_share_2005$venezolanos_2005)

# --- LOCALIDADES ----

library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

# Calculamos la población de Bogotá por localidad en 2005 (si la quieres para otras cosas)
pob_bog_05 <- import("data/pob_loc_2005.csv")

poblacion_localidad_2005 <- pob_bog_05 %>%
  filter(ANO == 2005) %>%
  filter(NOMBRE_LOCALIDAD != "Bogotá") %>%  
  group_by(CODIGO_LOCALIDAD, NOMBRE_LOCALIDAD) %>%
  summarise(
    Poblacion_Total = sum(POBLACION_7, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Poblacion_Total))

print(poblacion_localidad_2005)

# Shapefile de localidades
localidades <- st_read("data/Loca.shp")

# --- Estandarizamos nombres ----

estandarizar_nombre <- function(nombre) {
  nombre <- as.character(nombre)
  nombre <- toupper(nombre)
  nombre <- gsub("Á", "A", nombre)
  nombre <- gsub("É", "E", nombre)
  nombre <- gsub("Í", "I", nombre)
  nombre <- gsub("Ó", "O", nombre)
  nombre <- gsub("Ú", "U", nombre)
  nombre <- gsub("Ü", "U", nombre)
  nombre <- gsub("Ñ", "N", nombre)
  nombre <- gsub("^LA ", "", nombre)  # Quitar "LA " al inicio si existe
  nombre <- trimws(nombre)
  return(nombre)
}

# Shapefile: crear NOMBRE_ESTANDAR
localidades <- localidades %>%
  mutate(NOMBRE_ESTANDAR = estandarizar_nombre(LocNombre))

# Traemos la base de share
ven_share_2005 <- ven_share_2005 %>%
  mutate(
    # Reemplazar "_" por espacio para empatar con el shapefile
    localidad_clean = gsub("_", " ", localidad),
    NOMBRE_ESTANDAR = estandarizar_nombre(localidad_clean)
  )

# Unir shapefile + venezolanos
localidades_con_venezolanos <- localidades %>%
  left_join(ven_share_2005, by = "NOMBRE_ESTANDAR")

# --- MAPA VENEZOLANOS 2005 ----

# Excluir Sumapaz
localidades_ven_sin_sumapaz <- localidades_con_venezolanos %>%
  filter(!grepl("SUMAPAZ", NOMBRE_ESTANDAR)) %>% 
  mutate(pct_ven = venezolanos_2005 / total_ven)

ggplot(localidades_ven_sin_sumapaz) +
  geom_sf(aes(fill = pct_ven), color = "white", size = 0.3) +
  
  # ---- Escala azul ----
scale_fill_gradient(
  low = "#deebf7", high = "#08519c",
  name = "% venezolanos 2005",
  labels = scales::percent_format(accuracy = 0.1),
  na.value = "gray90"
) +
  
  # ---- Etiquetas como porcentaje ----
geom_sf_text(
  aes(label = ifelse(pct_ven >= 0.02,  # solo etiquetas >=2%
                     scales::percent(pct_ven, accuracy = 0.1), "")),
  size = 2.8,
  color = "black",
  fontface = "bold"
) +
  
  theme_minimal() +
  labs(
    title = "Distribución porcentual de población venezolana por localidad - Bogotá 2005",
    subtitle = "Simulación a partir de pesos por localidad (excluye Sumapaz)",
    caption = "Fuente: Cálculos propios con base en 5.818 venezolanos simulados"
  ) +
  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),   # <-- elimina títulos de ejes
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  )

# Extraer coordenadas del centroide
coordenadas <- localidades %>%
  st_transform(4326) %>%
  st_centroid() %>%
  mutate(
    longitud = st_coordinates(.)[,1],
    latitud = st_coordinates(.)[,2]
  ) %>%
  dplyr::select(Localidad = LocNombre, longitud, latitud) %>%
  st_drop_geometry()

print(coordenadas)

# Exportamos la tabla
write.csv2(
  coordenadas,
  file = "coord_loc.csv",
  row.names = FALSE
)

# ---- MIGRACIÓN (VARIABLE DEPENDIENTE) ----
EM2021 <- import("data/bog_em2021.csv")

EM2021 <- EM2021 %>%
  mutate(
    NOMBRE_LOCALIDAD = iconv(NOMBRE_LOCALIDAD,
                             from = "latin1",
                             to   = "UTF-8")
  )

# Filtramos nacidos en otros país y que sea Venezuela
ven_loc_2021 <- EM2021 %>%
  filter(
    NPCEP11A == 3,    # 3 = Otro país
    NPCEP11D == 1     # 1 = Venezuela
  ) %>%
  group_by(COD_LOCALIDAD, NOMBRE_LOCALIDAD) %>%
  summarise(
    venezolanos_2021 = sum(FEX_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(venezolanos_2021))

# --- FIGURA 1 (Mapa comparativo composición venezolana) ---- 
# Estandarizamos ambos años
ven_2005_std <- ven_share_2005 %>%
  mutate(
    localidad_clean = gsub("_", " ", localidad),
    NOMBRE_ESTANDAR = estandarizar_nombre(localidad_clean)
  ) %>%
  group_by(NOMBRE_ESTANDAR) %>%
  summarise(ven_2005 = sum(venezolanos_2005))

ven_2021_std <- ven_loc_2021 %>%
  mutate(
    NOMBRE_ESTANDAR = estandarizar_nombre(NOMBRE_LOCALIDAD)
  ) %>%
  filter(!is.na(NOMBRE_ESTANDAR)) %>%
  group_by(NOMBRE_ESTANDAR) %>%
  summarise(ven_2021 = sum(venezolanos_2021, na.rm = TRUE))

ven_2021_std_clean <- ven_2021_std %>%
  filter(NOMBRE_ESTANDAR != "")

# Shapefile ya tenía NOMBRE_ESTANDAR
localidades_std <- localidades %>%
  mutate(NOMBRE_ESTANDAR = estandarizar_nombre(LocNombre))

# Unimos
localidades_ven <- localidades_std %>%
  left_join(ven_2005_std, by = "NOMBRE_ESTANDAR") %>%
  left_join(ven_2021_std, by = "NOMBRE_ESTANDAR")

total_2005 <- sum(ven_2005_std$ven_2005, na.rm = TRUE)
total_2021 <- sum(ven_2021_std_clean$ven_2021, na.rm = TRUE)

localidades_ven <- localidades_ven %>%
  mutate(
    pct_2005 = ven_2005 / total_2005,
    pct_2021 = ven_2021 / total_2021
  )

loc_ven_no_sumapaz <- localidades_ven %>%
  filter(!grepl("SUMAPAZ", NOMBRE_ESTANDAR))

library(ggplot2)

# Pasar a formato largo
map_data <- loc_ven_no_sumapaz %>%
  dplyr::select(NOMBRE_ESTANDAR, geometry, pct_2005, pct_2021) %>%
  tidyr::pivot_longer(
    cols = starts_with("pct_"),
    names_to = "anio",
    values_to = "porcentaje"
  ) %>%
  mutate(
    anio = ifelse(anio == "pct_2005", "2005", "2021")
  )

ggplot(map_data) +
  geom_sf(aes(fill = porcentaje), color = "white", size = 0.25) +
  scale_fill_gradient(
    low = "#deebf7", high = "#08519c",
    name = "% venezolanos",
    labels = scales::percent_format(accuracy = 0.1),
    na.value = "grey90"
  ) +
  geom_sf_text(
    aes(
      label = ifelse(porcentaje >= 0.02,
                     scales::percent(porcentaje, accuracy = 0.1), "")
    ),
    size = 2.5, color = "black", fontface = "bold"
  ) +
  facet_wrap(~ anio) +
  theme_minimal() +
  labs(
    title = "Distribución porcentual de población venezolana en Bogotá (2005 vs 2021)",
    subtitle = "Mapas construidos con porcentajes respecto al total de venezolanos en cada año",
    caption = "Fuente: Cálculos propios con simulación 2005 y microdatos 2021"
  ) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# CREACIÓN DE LA VAR MIGRACIÓN

# Cargamos la población total para Bogotá por año (Fuente: DANE)
pob_bog_total <- tribble(
  ~Año, ~pob_bog,
  2012, 7175553,
  2013, 7207289,
  2014, 7231097,
  2015, 7250675,
  2016, 7277543,
  2017, 7313244,
  2018, 7366187,
  2019, 7507403,
  2020, 7690578,
  2021, 7777174,
  2022, 7820740,
  2023, 7854783,
  2024, 7888813
)

# Base con todos los años x todas las localidades
share_bogota <- 0.21

suma_por_año_bogota <- suma_por_año %>%
  mutate(
    total_bogota = Total_por_año * share_bogota   # <-- AQUÍ VA EL 21%
  )

pct_2021_loc <- ven_2021_std_clean %>%
  mutate(
    pct_2021 = ven_2021 / sum(ven_2021, na.rm = TRUE)
  ) %>%
  dplyr::select(NOMBRE_ESTANDAR, pct_2021)

base_migr <- tidyr::crossing(
  Año = suma_por_año_bogota$Año,
  NOMBRE_ESTANDAR = pct_2021_loc$NOMBRE_ESTANDAR
) %>%
  left_join(suma_por_año_bogota, by = "Año") %>%
  left_join(pct_2021_loc, by = "NOMBRE_ESTANDAR") %>%
  mutate(
    ven_estimados = total_bogota * pct_2021   # YA INCLUYE EL 21%
  )

migr_loc_anio <- base_migr %>%
  left_join(pob_bog_total, by = c("Año")) %>%
  mutate(
    tasa_ven_1000 = 1000 * ven_estimados / pob_bog
  ) %>%
  filter(
    Año >= 2014,
    Año <= 2024,
    NOMBRE_ESTANDAR != "SUMAPAZ"
  )

# Exportamos la tabla
write.csv2(
  migr_loc_anio,
  file = "migr_loc_anio.csv",
  row.names = FALSE
)


# --- DÉFICIT HABITACIONAL ---- 

# Déficit base por localidad según la gráfica
deficit_vals <- tribble(
  ~NOMBRE_ESTANDAR, ~deficit_base,
  "CIUDAD BOLIVAR", 47419,
  "KENNEDY",         34065,
  "BOSA",            30077,
  "SUBA",            25665,
  "SAN CRISTOBAL",   25061,
  "USME",            24473,
  "RAFAEL URIBE URIBE", 20697,
  "ENGATIVA",        17618,
  "TUNJUELITO",       9694,
  "FONTIBON",         6585,
  "SANTA FE",         6201,
  "USAQUEN",          5650,
  "PUENTE ARANDA",    4501,
  "LOS MARTIRES",     2756,
  "BARRIOS UNIDOS",   2227,
  "ANTONIO NARINO",   2116,
  "CHAPINERO",         723,
  "CANDELARIA",         58,
  "TEUSAQUILLO",        58
)

# Convertimos en proporción
deficit_shares <- deficit_vals %>%
  mutate(share_deficit = deficit_base / sum(deficit_base))

# Trayectoria temporal del déficit total Bogotá (plausible)
deficit_total_anual <- tibble(
  Año = 2014:2024,
  deficit_total = c(
    210000,  # 2014
    205000,  # 2015
    200000,  # 2016
    195000,  # 2017
    190000,  # 2018
    185000,  # 2019 (baja)
    205000,  # 2020 (sube por Covid)
    205000,  # 2021 (se mantiene)
    195000,  # 2022 (baja otra vez)
    185000,  # 2023
    175000   # 2024
  )
)

# Población total por año (DANE)
pob_bog_total <- tribble(
  ~Año, ~pob_bog,
  2012, 7175553,
  2013, 7207289,
  2014, 7231097,
  2015, 7250675,
  2016, 7277543,
  2017, 7313244,
  2018, 7366187,
  2019, 7507403,
  2020, 7690578,
  2021, 7777174,
  2022, 7820740,
  2023, 7854783,
  2024, 7888813
)

# Shares de población por localidad (usando base 2005 que ya tienes)
pob_loc_share <- poblacion_localidad_2005 %>%
  mutate(
    NOMBRE_ESTANDAR = estandarizar_nombre(NOMBRE_LOCALIDAD),
    share_pob = Poblacion_Total / sum(Poblacion_Total)
  ) %>%
  dplyr::select(NOMBRE_ESTANDAR, share_pob)


# Estimar población por localidad para cada año 2014–2024
pob_loc_anio <- tidyr::crossing(
  Año = 2014:2024,
  NOMBRE_ESTANDAR = pob_loc_share$NOMBRE_ESTANDAR
) %>%
  left_join(pob_bog_total, by = "Año") %>%
  left_join(pob_loc_share, by = "NOMBRE_ESTANDAR") %>%
  mutate(
    poblacion_localidad = pob_bog * share_pob,
    hogares_localidad   = poblacion_localidad / 2.8      # hogares totales
  )

# Simulación déficit por localidad y año
deficit_simulado <- tidyr::crossing(
  Año = deficit_total_anual$Año,
  NOMBRE_ESTANDAR = deficit_shares$NOMBRE_ESTANDAR
) %>%
  left_join(deficit_total_anual, by = "Año") %>%
  left_join(deficit_shares, by = "NOMBRE_ESTANDAR") %>%
  mutate(deficit_est = deficit_total * share_deficit)     # déficit en PERSONAS

# Convertir déficit a hogares y calcular tasa por localidad
deficit_final <- deficit_simulado %>%
  mutate(hogares_deficit = deficit_est / 2.8) %>%         # hogares en déficit
  left_join(pob_loc_anio, by = c("Año", "NOMBRE_ESTANDAR")) %>%
  mutate(
    deficit_rate = hogares_deficit / hogares_localidad
  ) %>%
  filter(NOMBRE_ESTANDAR != "SUMAPAZ") %>%                # por si está en el shapefile
  arrange(Año, NOMBRE_ESTANDAR)

write.csv2(
  deficit_final,
  file = "deficit_final.csv",
  row.names = FALSE
)

# --- CONTROLES ----
ipo <- st_read("data/IPO.shp")

# La columna LocNombre está en número, pasamos con códigos a string
codigos_loc <- tibble::tribble(
  ~cod_loc, ~loc,
  8, "Kennedy",
  11, "Suba",
  10, "Engativá",
  19, "Ciudad Bolívar",
  7, "Bosa",
  1, "Usaquén",
  4, "San Cristóbal",
  18, "Rafael Uribe",
  5, "Usme",
  9, "Fontibón",
  16, "Puente Aranda",
  12, "Barrios Unidos",
  6, "Tunjuelito",
  13, "Teusaquillo",
  2, "Chapinero",
  15, "Antonio Nariño",
  3, "Santa Fe",
  14, "Los Mártires",
  17, "La Candelaria"
)

ipo <- ipo %>%
  mutate(cod_loc = as.numeric(LocNombre)) %>%         
  left_join(codigos_loc, by = "cod_loc") %>%          
  mutate(
    NOMBRE_ESTANDAR = estandarizar_nombre(loc)        
  )

# Definimos los valores de inflación para simular el crecimiento del ingreso promedio de los hogares
# según este
inflacion <- tribble(
  ~anio, ~infl,
  2015, 6.77,
  2016, 5.96,
  2017, 4.12,
  2018, 3.27,
  2019, 3.84,
  2020, 1.49,
  2021, 5.26,
  2022, 12.53,
  2023, 10.15,
  2024, 5.20
)

inf <- setNames(inflacion$infl, inflacion$anio)
ipo_proy <- ipo %>%
  mutate(
    # 2015 y 2016: desde 2014
    ipo2015 = ipo2014 * (1 + inf["2015"]/100),
    ipo2016 = ipo2015 * (1 + inf["2016"]/100),
    
    # 2018–2024: desde 2017
    ipo2018 = ipo2017 * (1 + inf["2018"]/100),
    ipo2019 = ipo2018 * (1 + inf["2019"]/100),
    ipo2020 = ipo2019 * (1 + inf["2020"]/100),
    ipo2021 = ipo2020 * (1 + inf["2021"]/100),
    ipo2022 = ipo2021 * (1 + inf["2022"]/100),
    ipo2023 = ipo2022 * (1 + inf["2023"]/100),
    ipo2024 = ipo2023 * (1 + inf["2024"]/100)
  )

# Pasamos a tabla: NOMBRE_ESTANDAR, Año, Ingreso
ipo_long <- ipo_proy %>%
  st_drop_geometry() %>%              
  tidyr::pivot_longer(
    cols = starts_with("ipo"),
    names_to = "Año",
    values_to = "Ingreso"
  ) %>%
  mutate(
    Año = as.numeric(gsub("ipo", "", Año))
  ) %>%
  arrange(NOMBRE_ESTANDAR, Año)

ipo_long

write.csv2(
  ipo_long,
  file = "ipo_long.csv",
  row.names = FALSE
)

# HACEMOS MAPA DE INGRESOS
ipo_2021 <- ipo_proy %>%
  filter(NOMBRE_ESTANDAR != "SUMAPAZ")

ipo_2021 <- ipo_2021 %>%
  mutate(
    ipo2021_mill = ipo2021 / 1e6,                     # ingreso en millones
    label_mill   = format(round(ipo2021_mill, 1),     # ej: 3,1
                          decimal.mark = ",",
                          big.mark = ".")
  )

ggplot(ipo_2021) +
  geom_sf(aes(fill = ipo2021_mill), color = "white", size = 0.3) +
  scale_fill_gradient(
    low  = "#deebf7",
    high = "#08519c",
    name = "Ingreso (millones)",
    labels = function(x) format(round(x, 1), big.mark=".", decimal.mark=",")
  ) +
  geom_sf_text(
    aes(label = label_mill),
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  theme_minimal() +
  labs(
    title    = "Ingreso promedio por localidad - Bogotá 2021 (millones de pesos)",
    subtitle = "Simulación basada sobre los ingresos promedio de ocupados en Bogotá",
    caption  = "Fuente: Datos Abiertos | Elaboración propia"
  ) +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  )


# --- TABLAS DESCRIPTIVAS ----

# --- TABLA: Población vs Migración 2005–2024 (pero migración solo existe desde 2014) ---

pob_2005 <- poblacion_localidad_2005 %>%
  summarise(poblacion_bog_2005 = sum(Poblacion_Total, na.rm = TRUE))

# --- Población Bogotá por año (incluyendo 2005) ---

tabla_1 <- bind_rows(
  pob_bog_total %>% 
    mutate(tipo = "Población Bogotá"),  # pob_bog_total: Año, pob_bog
  tibble(
    Año    = 2005,
    pob_bog = pob_2005$poblacion_bog_2005,
    tipo   = "Población Bogotá"
  )
) %>%
  arrange(Año)

# --- Migración total a Colombia por año (ya la tienes como suma_por_año) ---
# Aseguramos que suma_por_año es un tibble con Año y Total_por_año

suma_por_año_bogota <- suma_por_año_bogota %>%
  as_tibble() %>%
  dplyr::select(Año, Total_por_año)

# 21% de los venezolanos que entran a Colombia llegan a Bogotá

share_bogota <- 0.21

suma_por_año_bogota <- suma_por_año %>%
  mutate(total_bog = Total_por_año * share_bogota)

# --- Agregar migración total a Bogotá (solo años que existan en suma_por_año_bogota) ---

tabla_1 <- tabla_1 %>%
  left_join(
    suma_por_año_bogota %>% dplyr::select(Año, total_bog),
    by = "Año"
  )

tabla_1

# TABLA: relación migrantes / población total (sin 2005) ---

tabla_2 <- tabla_1 %>%
  filter(Año != 2005) %>%
  mutate(
    rel_migracion_pob = total_bog / pob_bog
  ) %>%
  dplyr::select(Año, pob_bog, total_bog, rel_migracion_pob)

tabla_2

# TABLA: promedio déficit habitacional (excluye 2005) ---

tabla_3 <- deficit_final %>%
  group_by(Año) %>%
  summarise(
    deficit_promedio = mean(deficit_rate, na.rm = TRUE)
  ) %>%
  filter(Año != 2005)

tabla_3

# TABLA 4: Ingreso promedio por año (2014–2024)
ingreso_prom_anual <- ipo_long %>%
  filter(Año >= 2014, Año <= 2024) %>%
  group_by(Año) %>%
  summarise(
    ingreso_promedio = mean(Ingreso, na.rm = TRUE),
    .groups = "drop"
  )

ingreso_prom_anual

# --- UNIFICAR LAS TRES TABLAS EN MISMAS FILAS ---

# Tabla maestra por año
tabla_descriptiva <- tabla_1 %>%
  dplyr::select(Año, pob_bog, total_bog) %>%
  left_join(tabla_2 %>% dplyr::select(Año, rel_migracion_pob), by = "Año") %>%
  left_join(tabla_3, by = "Año") %>%
  left_join(ingreso_prom_anual, by = "Año") %>%     # <-- aquí entra el ingreso promedio
  arrange(Año)

tabla_descriptiva

# Número de localidades y observaciones ---

num_localidades <- deficit_final %>%
  distinct(NOMBRE_ESTANDAR) %>%
  nrow()

num_observaciones <- nrow(deficit_final)

list(
  "Número de localidades" = num_localidades,
  "Número de observaciones" = num_observaciones
)

# EN LATEX
latex_tabla_descriptiva <- tabla_descriptiva %>%
  mutate(
    pob_bog = format(round(pob_bog, 0), big.mark = "."),
    total_bog = ifelse(is.na(total_bog), "-", format(round(total_bog, 0), big.mark = ".")),
    rel_migracion_pob = ifelse(is.na(rel_migracion_pob), "-", sprintf("%.4f", rel_migracion_pob)),
    deficit_promedio = sprintf("%.4f", deficit_promedio),
    ingreso_promedio = sprintf("%.2f", ingreso_promedio)
  ) %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Estadísticas descriptivas anuales: población, migración, déficit e ingreso promedio (2014--2024).",
    label = "tab:descriptiva",
    align = "c"
  )

cat(latex_tabla_descriptiva)

# -------------------------------------------- #
### ANÁLISIS DE VARIABLE INSTRUMENTAL ###
# -------------------------------------------- #

library(readxl)
library(dplyr)
library(fixest) 

# Cambia "data/archivo.xlsx" por la ruta real del archivo
data <- "data/data.xlsx"

# Leer la primera hoja
df <- read_excel(data, sheet = 1)

df <- df %>% dplyr::select(-aux) %>%
  mutate(
    localidad = factor(localidad),
    year      = factor(year)
  )

# --- PRIMERA ETAPA ----
# sin control
fs_1_nocontrol <- feols(
  migr ~ z | localidad + year,
  data    = df,
  cluster = ~ localidad
)


# --- SEGUNDA ETAPA ----
# Sin control
iv_2_nocontrol <- feols(
  y ~ 1 | localidad + year | migr ~ z,
  data    = df,
  cluster = ~ localidad
)


etable(
  fs_1_nocontrol,
  iv_2_nocontrol,
  dict = c(
    migr = "Migración",
    z = "Instrumento (Z)"
  ),
  se.below = TRUE,
  signif.code = NA
)

