## Loading and saving Spatial Layers (shapefile)
## Loading  of Spatial Layers (shapefile)
## COnverting to a sf object with a common reference unit
## Subsetting the shapefile to the area of interest
## Saving into rds objects for posterior more easy use
## PBH Dec. 2021

# Load libraries and other useful common functions
source("Scripts/00-Common.R", encoding = "UTF-8")

# Folder directions -----
url_file_shp <- sprintf(url_file,
                        "Spatial Data/%s/%s/%s.shp")
file_rds <- "Data/Spatial Data/%s.rds"


# Common spatial overlay - 14 communes -----
# We can use the 14 communes of interest to overlay spatially all objects
# First we need to create it and visualize it

map_commune2 <- st_transform(map_commune,"EPSG:4326") %>% 
  st_make_valid()

folder <- "Biblioteca Congreso Nacional"
  
# MASAS LACUSTRES ------
lagos <- st_read(sprintf(url_file_shp,
                         folder,"Masas_Lacustres","masas_lacustres"))

lagos <- st_transform(lagos,"EPSG:4326") %>% st_make_valid()

lagos_zone <- lagos %>% filter(st_area_sh>1.7*1e6) # to avoid small water bodies

# Spatial filter using 14 communes - to obtain the 23 lakes
lagos_zone <- st_filter(lagos_zone,map_commune2) %>% 
  filter(!is.na(Nombre)) %>%
  filter(!(Nombre %in% c("Laguna Gualalafquen", # Border cases
                         "Laguna de Los Patos",
                         "Laguna Pichilaguna",
                         "Laguna Sargazo",
                         "Lago Siete ISlas",
                         "Estero Reloncavi",
                         "Lago Paraiso",
                         "Laguna Bonita",
                         "Lago Cayutué"))) %>% 
  filter(objectid != 14388) # lago Calafquen is repeated

# Save layer for use in other scripts
saveRDS(lagos_zone,sprintf(file_rds,"lagos_zone"))

rm(lagos,lagos_zone)

## Areas_Pobladas --------
areas_pobladas <- st_read(sprintf(url_file_shp,
                                  folder,"Areas_Pobladas","Areas_Pobladas"))
areas_pobladas <- st_transform(areas_pobladas,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
areas_pobladas <- st_filter(areas_pobladas,map_commune2)

# Save layer for use in other scripts
saveRDS(areas_pobladas,sprintf(file_rds,"areas_pobladas"))
rm(areas_pobladas)

### Glaciares -----
folder <- "IDE"
glaciares <- st_read(sprintf(url_file_shp,folder,
                             "IPG2014","IPG2014"))

# filter by region first
glaciares <- glaciares %>% 
  filter(REGION %in% c("LOS LAGOS","LOS RIOS","ARAUCANIA"))
# table(glaciares$REGION)

glaciares <- st_transform(glaciares,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
glaciares <- st_filter(glaciares,map_commune2)

# Save layer for use in other scripts
saveRDS(glaciares,sprintf(file_rds,"glaciares"))
rm(glaciares)

### Atractivos Turisticos Nacionales -----
atractivos_turisticos <- st_read(sprintf(url_file_shp,folder,
                                         "atractivos_turisticos_nacional_2020",
                                         "ATRACTIVOS_TUR+STICOS_NACIONAL_2020"))
atractivos_turisticos <- st_transform(atractivos_turisticos,"EPSG:4326") %>% 
  st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
atractivos_turisticos <- st_filter(atractivos_turisticos,map_commune2)

# Save layer for use in other scripts
saveRDS(atractivos_turisticos,sprintf(file_rds,"atractivos_turisticos"))
rm(atractivos_turisticos)


### Catastros de Uso de Suelo y Vegetacion -----
uso_suelo <- st_read(sprintf(url_file_shp,folder,
                             "Catastro_uso_suelo_y_vegetacion",
                             "Catastro_RV_R09_2014"))
uso_suelo <- st_transform(uso_suelo,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
uso_suelo <- st_filter(uso_suelo,map_commune2)

# Save layer for use in other scripts
saveRDS(uso_suelo,sprintf(file_rds,"uso_suelo"))
rm(uso_suelo)


# clean WS ----
rm(folder,file_rds,url_file_shp)

## EoF