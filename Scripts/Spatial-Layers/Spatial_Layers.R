## Spatial Layers
## Visualizationg of Spatial Layers
## All previous shapefile layers are loaded and filtered in a different script
## PBH Dec. 2021

# Load libraries and other useful common functions -----
source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/00-Functions_Spatial.R",encoding = "UTF-8")
url_load_shp <- "Data/Spatial Data/%s.rds"


## Communes  and labels ------

map_commune2 <- st_transform(map_commune,"EPSG:4326") %>% 
  st_make_valid()
lab_commune <- map_commune2 %>% 
  left_join(codigos_territoriales) %>% 
  pull(nombre_comuna)
lab_commune <- paste0("<strong>",lab_commune,"</strong>") %>% 
  lapply(HTML)


map_provincia <- readRDS(sprintf(url_load_shp,"map_provincia"))
map_region <- readRDS(sprintf(url_load_shp,"map_region"))
codigos_territoriales_region <- codigos_territoriales %>% 
  group_by(codigo_region,nombre_region) %>% tally
lab_region <- map_region %>% 
  left_join(codigos_territoriales_region) %>% 
  pull(nombre_region)
lab_region <- paste0("<strong>",lab_region,"</strong>") %>% 
  lapply(HTML)
rm(codigos_territoriales_region)

codigos_territoriales_provincia <- codigos_territoriales %>% 
  group_by(codigo_provincia,nombre_provincia) %>% tally
lab_provincia <- map_provincia %>% 
  left_join(codigos_territoriales_provincia) %>% 
  pull(nombre_provincia)
lab_provincia <- paste0("<strong>",lab_provincia,"</strong>") %>% 
  lapply(HTML)
rm(codigos_territoriales_provincia)

m_commune <- f.interactive.map(map_commune2,lab_commune) 
# rm(lab_commune)
m_commune


## LOAD LAYERS -------------

lagos_zone <- readRDS(sprintf(url_load_shp,"lagos_zone")) # MASAS LACUSTRES
areas_pobladas <- readRDS(sprintf(url_load_shp,"areas_pobladas"))
glaciares <- readRDS(sprintf(url_load_shp,"glaciares"))
atractivos_turisticos <- readRDS(sprintf(url_load_shp,"atractivos_turisticos"))
aeropuertos <- readRDS(sprintf(url_load_shp,"aeropuertos"))
areas_silvestres_protegidas <- readRDS(sprintf(url_load_shp,"areas_silvestres_protegidas"))
circuitos_turisticos <- readRDS(sprintf(url_load_shp,"circuitos_turisticos"))
embalses <- readRDS(sprintf(url_load_shp,"embalses"))
establecimiento_salud <- readRDS(sprintf(url_load_shp,"establecimiento_salud"))
fluviometricas <- readRDS(sprintf(url_load_shp,"fluviometricas"))
humedales <- readRDS(sprintf(url_load_shp,"humedales"))
industria_forestal <- readRDS(sprintf(url_load_shp,"industria_forestal"))
map_distrito <- readRDS(sprintf(url_load_shp,"map_distrito"))
meteorologica <- readRDS(sprintf(url_load_shp,"meteorologica"))
planta_aguas_servidas <- readRDS(sprintf(url_load_shp,"planta_aguas_servidas"))
puentes <- readRDS(sprintf(url_load_shp,"puentes"))
sedimentometricas <- readRDS(sprintf(url_load_shp,"sedimentometricas"))
sello_calidad_turistica <- readRDS(sprintf(url_load_shp,"sello_calidad_turistica"))
sendero_chile <- readRDS(sprintf(url_load_shp,"sendero_chile"))
sitios_prioritarios <- readRDS(sprintf(url_load_shp,"sitios_prioritarios"))
comunidad_indigena <- readRDS(sprintf(url_load_shp,"comunidad_indigena"))
humedales <- readRDS(sprintf(url_load_shp,"humedales"))


## FEATURE DATA -------

## Map with all layers ------
# Tiles available: see https://leaflet-extras.github.io/leaflet-providers/preview/

m <- leaflet() %>% 
  addTiles() %>% 
  # addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}') %>% 
  add.Layer(lagos_zone,lagos_zone$Nombre,
            features=c("Tipo"),
            group_l = "North-Patagonian Lakes",
            color="blue") %>% 
  addPolygons(data=map_commune2,label = lab_commune,
              group = "Commune",color = "grey") %>% 
  addLegend(values = 1, group = "Commune",position = "bottomright", 
            labels = "Commune",colors= "grey") %>% 
  addPolygons(data=map_provincia,label = lab_provincia,
              group = "Province",color = "grey") %>% 
  addLegend(values = 1, group = "Province",position = "bottomright", 
            labels = "Province",colors= "grey") %>%
  addPolygons(data=map_region,label = lab_region,
              group = "Region",color = "grey") %>% 
  addLegend(values = 1, group = "Region",position = "bottomright", 
            labels = "Region",colors= "grey") %>%
  add.Layer(areas_pobladas,"Areas Pobladas",
            features = c("Localidad","Entidad"),
            source="Biblioteca Congreso Nacional",
            color="red") %>% 
  add.Layer(humedales,"Humedales",
            features = c("Nombre","Clase","SubClase","AreaProteg",
                         "Nombre_AP","Designacio"),
            source="IDE-Bienes Nacionales, 2015",
            color="green") %>%
  add.Layer(comunidad_indigena,"Comunidades Indigenas",
            features = c("COMUNIDAD","REGISTRO","FECHA"),
            source="CONADI, 2021",
            color="blue",
            polygon_form = "p") %>%
  add.Layer(glaciares,"Glaciares",
            features = c("NOMBRE","CLASIFICA","VOL_M3","NOMB_CUEN"),
            source="IDE-Bienes Nacionales, 2002",
            color="#00008B") %>%
  add.Layer(atractivos_turisticos,"Atractivos Turisticos",
            features = c("NOMBRE","CATEGORIA","TIPO","PROPIEDAD"),
            polygon_form = "p",
            source="IDE-Bienes Nacionales",
            color="green") %>% 
  add.Layer(planta_aguas_servidas,"Planta Aguas Servidas",
            features = c("DESCRIPCIO","SISTEMA","TIP_TRATAM",
                         "RECEPTOR","TIPO_RECEP","CARGA_NKT"),
            polygon_form = "p",
            source="IDE-Bienes Nacionales",
            color="brown") %>% 
  add.Layer(aeropuertos,"Aeropuertos y aerodromos",
            features = c("Aerodromo","categoría"),
            polygon_form = "p",
            source="Biblioteca Congreso Nacional",
            color="black") %>% 
  add.Layer(circuitos_turisticos,"Circuitos Turisticos",
            features = c("Circuito","Exten_km"),
            polygon_form = "l",
            source="IDE-Bienes Nacionales",
            color="red") %>% 
  add.Layer(embalses,"Embalses",
            features=c("NOMBRE"),
            polygon_form = "p",
            source="IDE-Bienes Nacionales, 2016",
            color="blue") %>% 
  add.Layer(meteorologica,"Estaciones Meteorologicas",
            features = c("NOMBRE","NOM_CUEN","NOM_SUBC"),
            source="IDE-Bienes Nacionales",
            polygon_form = "p",color="black") %>%
  add.Layer(fluviometricas,"Estaciones Fluviometricas",
            features = c("NOMBRE","NOM_CUEN","NOM_SUBC"),
            source="IDE-Bienes Nacionales",
            polygon_form = "p",color="black") %>%
  add.Layer(sedimentometricas,"Estaciones Sedimentometricas",
            features = c("NOMBRE","NOM_CUEN","NOM_SUBC"),
            source="IDE-Bienes Nacionales",
            polygon_form = "p",color="black")

  
# Could automatize this part, don't know how yet
groupsId <- c("North-Patagonian Lakes",
              "Commune","Province","Region",
              "Areas Pobladas",
              "Humedales",
              "Comunidades Indigenas",
              "Glaciares",
              "Atractivos Turisticos",
              "Planta Aguas Servidas",
              "Aeropuertos y aerodromos",
              "Circuitos Turisticos",
              "Embalses",
              "Estaciones Meteorologicas",
              "Estaciones Fluviometricas",
              "Estaciones Sedimentometricas")

# create selectable layers
m <- m %>% 
  addLayersControl(baseGroups = c("OpenStreetMap"),
                   overlayGroups = groupsId) %>% 
  hideGroup(groupsId[-1])

m


mapshot(m, "Figures/Maps/SpatialLayers.html", selfcontained=F)
rm(m)

# Clean WS -----
rm(url_load_shp)

## EoF