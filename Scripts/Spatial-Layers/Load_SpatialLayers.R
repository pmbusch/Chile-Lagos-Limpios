## Loading and saving Spatial Layers (shapefile)
## Loading  of Spatial Layers (shapefile)
## Converting to a sf object with a common reference unit
## Subsetting the shapefile to the area of interest
## Saving into rds objects for posterior more easy use
## Excel contains detail on the source of each shapefile
## PBH Dec. 2021

# LIBRARIES + PARAMETERS -----

# Load libraries and other useful common functions
source("Scripts/00-Common.R", encoding = "UTF-8")

## Folder directions -----
url_file_shp <- sprintf(url_file,
                        "Spatial Data/%s/%s/%s.shp")
file_rds <- "Data/Spatial Data/%s.rds"

# spatial layers info for lake characterization
info_spatialLayers <- tibble(layer_name=as.character(),
                             n_zone=as.numeric(), # number of features in lakes zone
                             n_chile=as.numeric(), # number of features in lakes zone
                             filter_layer=as.character(), # filter method
                             note=as.character())  # additional notes


# BOUNDARIES -----

## Commune ----
# Common spatial overlay - 14 communes
# We can use the 14 communes of interest to overlay spatially all objects
# First we need to create it and visualize it
map_commune2 <- st_transform(map_commune,"EPSG:4326") %>% 
  st_make_valid()
saveRDS(map_commune2,sprintf(file_rds,"map_commune"))


## Distrito censal -----
map_distrito <- mapa_zonas %>% st_as_sf() %>% 
  filter((codigo_comuna %in% comunes_cll_codes|
            codigo_comuna %in% comunes_cll_codes2)) %>% 
  mutate(area_distrito=st_area(geometry) %>% as.numeric(),
         perimeter_distrito=st_length(geometry) %>% as.numeric(),
         latitude_distrito=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_transform("EPSG:4326") %>% 
  st_make_valid()

map_distrito <- map_distrito %>% 
  left_join(codigos_territoriales)

saveRDS(map_distrito,sprintf(file_rds,"map_distrito"))
rm(map_distrito)


# BIBLIOTECA CONGRESO NACIONAL -----
folder <- "Biblioteca Congreso Nacional"
  
## Masa Lacustre ------
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


## Aeropuertos -----
aeropuertos <- st_read(sprintf(url_file_shp,folder,
                             "Aeropuertos",
                             "Aeropuertos"))
aeropuertos <- st_transform(aeropuertos,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(aeropuertos)

# Spatial filter using 14 communes - to obtain the 23 lakes
aeropuertos <- st_filter(aeropuertos,map_commune2)


# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="aeropuertos",n_zone=nrow(aeropuertos),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(aeropuertos,sprintf(file_rds,"aeropuertos"))
rm(aeropuertos)


## Areas silvestres protegidas por el estado para todo Chile -----
areas_silvestres_protegidas <- st_read(sprintf(url_file_shp,folder,
                             "Snaspe",
                             "snaspe"))
areas_silvestres_protegidas <- st_transform(areas_silvestres_protegidas,
                                            "EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(areas_silvestres_protegidas)

# Spatial filter using 14 communes - to obtain the 23 lakes
areas_silvestres_protegidas <- st_filter(areas_silvestres_protegidas,
                                         map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="areas_silvestres_protegidas",
          n_zone=nrow(areas_silvestres_protegidas),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(areas_silvestres_protegidas,sprintf(file_rds,
                                            "areas_silvestres_protegidas"))
rm(areas_silvestres_protegidas)

# IDE ----
folder <- "IDE"

## Glaciares -----
glaciares <- st_read(sprintf(url_file_shp,folder,
                             "IPG2014","IPG2014"))

n_chileTotal <- nrow(glaciares)

# filter by region first
glaciares <- glaciares %>% 
  filter(REGION %in% c("LOS LAGOS","LOS RIOS","ARAUCANIA"))
# table(glaciares$REGION)

glaciares <- st_transform(glaciares,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
glaciares <- st_filter(glaciares,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="glaciares",
          n_zone=nrow(glaciares),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(glaciares,sprintf(file_rds,"glaciares"))
rm(glaciares)

## Atractivos Turisticos Nacionales -----
atractivos_turisticos <- st_read(sprintf(url_file_shp,folder,
                                         "atractivos_turisticos_nacional_2020",
                                         "ATRACTIVOS_TUR+STICOS_NACIONAL_2020"))
atractivos_turisticos <- st_transform(atractivos_turisticos,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(atractivos_turisticos)

# Spatial filter using 14 communes - to obtain the 23 lakes
atractivos_turisticos <- st_filter(atractivos_turisticos,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="atractivos_turisticos",
          n_zone=nrow(atractivos_turisticos),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(atractivos_turisticos,sprintf(file_rds,"atractivos_turisticos"))
rm(atractivos_turisticos)

## Plantas de Tratamiento de Aguas Servidas -----
planta_aguas_servidas <- st_read(sprintf(url_file_shp,folder,
                                         "PTAS_Sirgas19s",
                                         "PTAS_Sirgas19s_VF"))
planta_aguas_servidas <- st_transform(planta_aguas_servidas,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(planta_aguas_servidas)

# Spatial filter using 14 communes - to obtain the 23 lakes
planta_aguas_servidas <- st_filter(planta_aguas_servidas,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="planta_aguas_servidas",
          n_zone=nrow(planta_aguas_servidas),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(planta_aguas_servidas,sprintf(file_rds,"planta_aguas_servidas"))
rm(planta_aguas_servidas)

## Ptos Descargas Plantas de Tratamiento de Aguas Servidas -----
planta_aguas_servidas_ptoDescarga <- st_read(sprintf(url_file_shp,folder,
                                         "PTAS_PTODESCARGA_Sirgas19s",
                                         "PTAS_PTODESCARGA_Sirgas19s_VF"))
planta_aguas_servidas_ptoDescarga <- st_transform(planta_aguas_servidas_ptoDescarga,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(planta_aguas_servidas_ptoDescarga)

# Spatial filter using 14 communes - to obtain the 23 lakes
planta_aguas_servidas_ptoDescarga <- st_filter(planta_aguas_servidas_ptoDescarga,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="planta_aguas_servidas_ptoDescarga",
          n_zone=nrow(planta_aguas_servidas_ptoDescarga),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(planta_aguas_servidas_ptoDescarga,sprintf(file_rds,"planta_aguas_servidas_ptoDescarga"))
rm(planta_aguas_servidas_ptoDescarga)


## Humedales 2015-----
humedales <- st_read(sprintf(url_file_shp,folder,
                                        "Humedales_2015",
                                        "Inventario_plataforma"))
humedales <- st_transform(humedales,"EPSG:4326") %>% 
  st_make_valid()

# Layer contains all body waters, we are only interested on wetlands (humedal)
humedales <- humedales %>% 
  filter(Clase=="Otros humedales")

n_chileTotal <- nrow(humedales)

# Spatial filter using 14 communes - to obtain the 23 lakes
humedales <- st_filter(humedales,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="humedales",
          n_zone=nrow(humedales),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(humedales,sprintf(file_rds,"humedales"))
rm(humedales)

## industria_forestal_2018-----
industria_forestal <- st_read(sprintf(url_file_shp,folder,
                             "industria_forestal_2018",
                             "industria_forestal_2018"))
industria_forestal <- st_transform(industria_forestal,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(industria_forestal)

# Spatial filter using 14 communes - to obtain the 23 lakes
industria_forestal <- st_filter(industria_forestal,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="industria_forestal",
          n_zone=nrow(industria_forestal),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(industria_forestal,sprintf(file_rds,"industria_forestal"))
rm(industria_forestal)

## Incendios_Forestales_2017-----
incendio_forestal <- st_read(sprintf(url_file_shp,folder,
                                      "Incendios_Forestales_2017",
                                      "IF_temporada2017"))
incendio_forestal <- st_transform(incendio_forestal,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(incendio_forestal)

# Spatial filter using 14 communes - to obtain the 23 lakes
incendio_forestal <- st_filter(incendio_forestal,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="incendio_forestal",
          n_zone=nrow(incendio_forestal),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(incendio_forestal,sprintf(file_rds,"incendio_forestal"))
rm(incendio_forestal)



## Estaciones Meteorologicas-----
meteorologica <- st_read(sprintf(url_file_shp,folder,
                                 "Meteorologicas",
                                 "Meteorologicas"))
meteorologica <- st_transform(meteorologica,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(meteorologica)

# Spatial filter using 14 communes - to obtain the 23 lakes
meteorologica <- st_filter(meteorologica,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="meteorologica",
          n_zone=nrow(meteorologica),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(meteorologica,sprintf(file_rds,"meteorologica"))
rm(meteorologica)

## Embalses 2016-----
embalses <- st_read(sprintf(url_file_shp,folder,
                             "Embalses_DGA_DOH_2016",
                             "Embalses_DGA_DOH_2016"))
embalses <- st_transform(embalses,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(embalses)

# Spatial filter using 14 communes - to obtain the 23 lakes
embalses <- st_filter(embalses,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="embalses",
          n_zone=nrow(embalses),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(embalses,sprintf(file_rds,"embalses"))
rm(embalses)

## Establecimientos Salud-----
establecimiento_salud <- st_read(sprintf(url_file_shp,folder,
                             "establec_salud_14_mayo_2021",
                             "Establec_Salud_14_Mayo_2021"))
establecimiento_salud <- st_transform(establecimiento_salud,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(establecimiento_salud)

# Spatial filter using 14 communes - to obtain the 23 lakes
establecimiento_salud <- st_filter(establecimiento_salud,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="establecimiento_salud",
          n_zone=nrow(establecimiento_salud),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(establecimiento_salud,sprintf(file_rds,"establecimiento_salud"))
rm(establecimiento_salud)

## Estaciones sedimentometricas -----
sedimentometricas <- st_read(sprintf(url_file_shp,folder,
                             "Sedimentometricas",
                             "Sedimentometricas"))
sedimentometricas <- st_transform(sedimentometricas,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(sedimentometricas)

# Spatial filter using 14 communes - to obtain the 23 lakes
sedimentometricas <- st_filter(sedimentometricas,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="sedimentometricas",
          n_zone=nrow(sedimentometricas),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(sedimentometricas,sprintf(file_rds,"sedimentometricas"))
rm(sedimentometricas)


## Estaciones Fluviometricas -----
fluviometricas <- st_read(sprintf(url_file_shp,folder,
                             "Fluviometricas",
                             "Fluviometricas"))
fluviometricas <- st_transform(fluviometricas,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(fluviometricas)

# Spatial filter using 14 communes - to obtain the 23 lakes
fluviometricas <- st_filter(fluviometricas,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="fluviometricas",
          n_zone=nrow(fluviometricas),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(fluviometricas,sprintf(file_rds,"fluviometricas"))
rm(fluviometricas)


## Sitios prioritarios -----
sitios_prioritarios <- st_read(sprintf(url_file_shp,folder,
                             "sitios_prioritarios",
                             "Sitios_Prioritarios"))
sitios_prioritarios <- st_transform(sitios_prioritarios,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(sitios_prioritarios)

# Spatial filter using 14 communes - to obtain the 23 lakes
sitios_prioritarios <- st_filter(sitios_prioritarios,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="sitios_prioritarios",
          n_zone=nrow(sitios_prioritarios),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(sitios_prioritarios,sprintf(file_rds,"sitios_prioritarios"))
rm(sitios_prioritarios)


## Sello de Calidad Turistica Alojamiento -----
sello_calidad_turistica <- st_read(sprintf(url_file_shp,folder,
                             "Sellos_de_calidad_turistica_alojamiento",
                             "SELLO CALIDAD TURÍSTICA_ALOJAMIENTO_WGS198419S_ABRIL18"))
sello_calidad_turistica <- st_transform(sello_calidad_turistica,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(sello_calidad_turistica)

# Spatial filter using 14 communes - to obtain the 23 lakes
sello_calidad_turistica <- st_filter(sello_calidad_turistica,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="sello_calidad_turistica",
          n_zone=nrow(sello_calidad_turistica),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(sello_calidad_turistica,sprintf(file_rds,"sello_calidad_turistica"))
rm(sello_calidad_turistica)


## Sendero de Chile -----
sendero_chile <- st_read(sprintf(url_file_shp,folder,
                             "Sendero_Chile",
                             "Sendero_de_Chile"))
sendero_chile <- st_transform(sendero_chile,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(sendero_chile)

# Spatial filter using 14 communes - to obtain the 23 lakes
sendero_chile <- st_filter(sendero_chile,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="sendero_chile",
          n_zone=nrow(sendero_chile),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(sendero_chile,sprintf(file_rds,"sendero_chile"))
rm(sendero_chile)


## Circuitos_Turisticos -----
circuitos_turisticos <- st_read(sprintf(url_file_shp,folder,
                             "Circuitos_Turísticos",
                             "Circuitos_Turisticos_2015"))

circuitos_turisticos <- st_transform(circuitos_turisticos,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(circuitos_turisticos)

# Spatial filter using 14 communes - to obtain the 23 lakes
circuitos_turisticos <- st_filter(circuitos_turisticos,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="circuitos_turisticos",
          n_zone=nrow(circuitos_turisticos),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(circuitos_turisticos,sprintf(file_rds,"circuitos_turisticos"))
rm(circuitos_turisticos)


## Censo 2007 Plantaciones Forestales-----
plantaciones_forestales <- st_read(sprintf(url_file_shp,folder,
                             "cc_forestal",
                             "cc_forestal"))
plantaciones_forestales <- st_transform(plantaciones_forestales,"EPSG:4326") %>% 
  st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
plantaciones_forestales <- st_filter(plantaciones_forestales,map_commune2)

# Save layer for use in other scripts
saveRDS(plantaciones_forestales,sprintf(file_rds,"plantaciones_forestales"))
rm(plantaciones_forestales)


## Censo 2007 Maquinaria Forestal ----
maquinaria_forestal <- st_read(sprintf(url_file_shp,folder,
                             "cc_mforest",
                             "cc_mforest"))
maquinaria_forestal <- st_transform(maquinaria_forestal,"EPSG:4326") %>% 
  st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
maquinaria_forestal <- st_filter(maquinaria_forestal,map_commune2)

# Save layer for use in other scripts
saveRDS(maquinaria_forestal,sprintf(file_rds,"maquinaria_forestal"))
rm(maquinaria_forestal)


## Predios ----
# create buffer around lake polygon
lagos_zone <- readRDS("Data/Spatial Data/lagos_zone.rds")
dist_filter <- 10*1e3 # 10 km
lagos_zone <- st_buffer(lagos_zone, dist_filter)

# function for each region
read_predio_filter <- function(foldername,filename){
  # load
  predios <- st_read(sprintf(url_file_shp,folder,
                             foldername,filename))
  predios <- st_transform(predios,"EPSG:4326") %>% 
    st_make_valid()
# Spatial filter using 14 communes - to obtain the 23 lakes
  predios <- st_filter(predios,map_commune2)
  # filter based on proximity to lake
  # filter predios that fall inside buffer zone
  predios <- st_filter(predios,lagos_zone)
  return(predios)
}
#get predios
predios_1 <- read_predio_filter("PREDIOS","PREDIOSARAUCANIA")
predios_2 <- read_predio_filter("PREDIOS","PREDIOSLOSRIOS")
predios_3 <- read_predio_filter("PREDIOS","PREDIOSLOSLAGOS")

# join
predios <- rbind(predios_1,predios_2,predios_3)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="predios",
          n_zone=nrow(predios),
          n_chile=0,
          filter_layer="10km from lake boundary")

# Save layer for use in other scripts
# mapview(predios)
saveRDS(predios,sprintf(file_rds,"predios"))
rm(predios,predios_1,predios_2,predios_3)

## Catastros de Uso de Suelo y Vegetacion -----
# same method as before, filter based on buffer of lakes
# we can use even the same function!
uso_suelo_1 <- read_predio_filter("Catastro_uso_suelo_y_vegetacion",
                                  "Catastro_RV_R09_2014")
uso_suelo_2 <- read_predio_filter("Catastro_uso_suelo_y_vegetacion",
                                  "CBN_XIV_2013")
uso_suelo_3 <- read_predio_filter("Catastro_uso_suelo_y_vegetacion",
                                  "CBN_10REG_2013")

# play with names to make it coincide
names(uso_suelo_2)[1:47] <- names(uso_suelo_2)[1:47] %>% str_to_upper()
# remove extra names
uso_suelo_1$COOR_ESTE <- NULL; uso_suelo_1$COOR_NORTE <- NULL
uso_suelo_3$COOR_ESTE <- NULL; uso_suelo_3$COOR_NORTE <- NULL


table(uso_suelo_1$USO)

uso_suelo_1$USO %>% unique

# join
uso_suelo <- rbind(uso_suelo_1,uso_suelo_2,uso_suelo_3)
table(uso_suelo$USO)

# get only terrenos agricolas 
# other types:
# Areas Sin Vegetacion Areas 556+526
# Urbanas-Industriales 178+54       
# Bosques 21843
# Cuerpos de Agua   911
# Humedales 143
# Praderas y Matorrales 9187
# Terrenos Agricolas  177+84
uso_suelo <- uso_suelo %>% 
  filter(USO %in% c("Terrenos Agricolas","Terrenos Agrícolas"))

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="uso_suelo",
          n_zone=nrow(uso_suelo),
          n_chile=0,
          filter_layer="10km from lake boundary")


# Save layer for use in other scripts
saveRDS(uso_suelo,sprintf(file_rds,"uso_suelo"))
rm(uso_suelo,uso_suelo_1,uso_suelo_2,uso_suelo_3,lagos_zone)


# MOP ----
folder <- "MOP"

## Puentes ----------
## Read as gdb to get attributes directly
## Get attributes in KML: https://stackoverflow.com/questions/50775357/how-to-read-in-kml-file-properly-in-r-or-separate-out-lumped-variables-into-col
puentes <- st_read(sprintf(url_file_shp,folder,"Puentes","Puentes") %>% 
                     str_replace("shp","gdb"),
                   layer = "Puentes")

puentes <- st_transform(puentes,"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- nrow(puentes)

# Spatial filter using 14 communes - to obtain the 23 lakes
puentes <- st_filter(puentes,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="puentes",
          n_zone=nrow(puentes),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(puentes,sprintf(file_rds,"puentes"))
rm(puentes)

## Red Vial Chile (Caminos) ----------
red_vial <- st_read(sprintf(url_file_shp,folder,"Red_Vial_Chile","Red_Vial_Chile_10_11_2021") %>% 
                     str_replace("shp","gdb"),
                   layer = "Red_Vial_Chile")

# need to add st_zm to remove Z and M (additional dimensions)
# source: https://gis.stackexchange.com/questions/346855/remove-z-value-from-xyz-point-in-sf
red_vial <- st_transform(st_zm(red_vial),"EPSG:4326") %>% 
  st_make_valid()

n_chileTotal <- sum(red_vial$KM_TRAMO)

# Spatial filter using 14 communes - to obtain the 23 lakes
red_vial <- st_filter(red_vial,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="red_vial",
          n_zone=sum(red_vial$KM_TRAMO),
          n_chile=n_chileTotal,
          filter_layer="Commune-Values in meters (length of road)")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(red_vial,sprintf(file_rds,"red_vial"))
rm(red_vial)


# CONADI -----

folder <- "Conadi"

## Comunidades Indigenas -----
comunidad_indigena <- st_read(sprintf(url_file_shp,folder,
                             "comunidades_act__julio_2021","comunidades_act__julio_2021"))

n_chileTotal <- nrow(comunidad_indigena)

# filter by region first
comunidad_indigena <- comunidad_indigena %>% 
  filter(REGI_N %in% c("X REGIÓN DE LOS LAGOS",
                       "XIV REGIÓN DE LOS RÍOS",
                       "IX REGION DE LA ARAUCANIA"))
# table(comunidad_indigena$REGI_N)

comunidad_indigena <- st_transform(comunidad_indigena,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
comunidad_indigena <- st_filter(comunidad_indigena,map_commune2)

# store info of number of features
info_spatialLayers <- info_spatialLayers %>% 
  add_row(layer_name="comunidad_indigena",
          n_zone=nrow(comunidad_indigena),
          n_chile=n_chileTotal,
          filter_layer="Commune")
rm(n_chileTotal)

# Save layer for use in other scripts
saveRDS(comunidad_indigena,sprintf(file_rds,"comunidad_indigena"))
rm(comunidad_indigena)


# save table
saveRDS(info_spatialLayers,
        "Data/info_spatialLayers.rds")

# CLEAN WS ----
rm(folder,file_rds,url_file_shp)

## EoF