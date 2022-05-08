# Table for spatial layers info
# 
# PBH April 2022



# Load Data ------
source("Scripts/00-Common.R", encoding = "UTF-8")
info_spatialLayers <- readRDS("Data/info_spatialLayers.rds")

# Data wrangling and filtering  ------

# layers of interest
info_spatialLayers <- info_spatialLayers %>% 
  filter(layer_name %in% c(
    "aeropuertos",
    "areas_silvestres_protegidas",
    "glaciares",
    "atractivos_turisticos",
    "planta_aguas_servidas",
    "humedales",
    "industria_forestal",
    "meteorologica",
    "establecimiento_salud",
    "comunidad_indigena"))

# Change layer names
info_spatialLayers <- info_spatialLayers %>% 
  mutate(layer_name=case_when(
    layer_name=="aeropuertos" ~ "Airports",
    layer_name=="areas_silvestres_protegidas" ~ "Protected Wild Areas",
    layer_name=="glaciares" ~ "Glaciers",
    layer_name=="atractivos_turisticos" ~ "Tourist Attractions",
    layer_name=="planta_aguas_servidas" ~ "Wastewater Treatment Plants",
    layer_name=="humedales" ~ "Wetlands",
    layer_name=="industria_forestal" ~ "Forest Industry",
    layer_name=="meteorologica" ~"Meteorological Stations",
    layer_name=="establecimiento_salud" ~ "Health Facilities",
    layer_name=="comunidad_indigena" ~ "Indigenous Communities"))


info_spatialLayers <- info_spatialLayers %>% 
  mutate(layer_name=layer_name %>% 
           str_replace_all("_"," ") %>% 
           str_to_title())

# columns of interest
info_spatialLayers$note <- NULL
info_spatialLayers$filter_layer <- NULL

# calculate percentage and change name
info_spatialLayers <- info_spatialLayers %>% 
  mutate(`% Chile Total`=n_zone/n_chile*100) %>% 
  rename(Metric=layer_name,
         `Lakes Zone`=n_zone)

# reorder row

# levels <- c(
#   "Comunidad Indigena",
#   "Areas Silvestres Protegidas",
#   "Glaciares",
#   "Humedales",
#   "Atractivos Turisticos",
#   "Aeropuertos",
#   "Establecimiento Salud",
#   "Meteorologica",
#   "Planta Aguas Servidas",
#   "Industria Forestal")

levels <- c(
  "Indigenous Communities",
  "Protected Wild Areas",
  "Glaciers",
  "Wetlands",
  "Tourist Attractions",
  "Airports",
  "Health Facilities", 
  "Meteorological Stations",
  "Wastewater Treatment Plants",
  "Forest Industry")

info_spatialLayers <- info_spatialLayers %>% 
  mutate(Metric=factor(Metric,levels = levels)) %>% 
  arrange(Metric)


# Table ------
table_spatial <- info_spatialLayers %>% 
  select(-n_chile) %>% 
  flextable() %>% autofit() %>% 
  colformat_double(j=3,digits=1,suffix = "%") %>% 
  bold(part="header")
# table_spatial

saveRDS(table_spatial,"Data/table_spatial.rds")


rm(levels)
# EoF