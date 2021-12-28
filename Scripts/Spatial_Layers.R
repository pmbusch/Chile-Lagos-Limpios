## Spatial Layers
## Visualizationg of Spatial Layers
## All previous shapefile layers are loaded and filtered in a different script
## PBH Dec. 2021

# Load libraries and other useful common functions
source("Scripts/00-Common.R", encoding = "UTF-8")

url_load_shp <- "Data/Spatial Data/%s.rds"



## Spatial Interactive Visualization function ----
# Create spatial layer map with labels, to quickly visualize it
# Input: sf object (spatial vector)
f.interactive.map <- function(map_object, label_vector, polygon_form=T){
  
  # Create labels for the vectors
  labels <- sprintf(
    "<strong>%s</strong><br/>",
    label_vector) %>% 
    lapply(HTML)
  
  # create interactive map object
  if (polygon_form){
    map_interactive <- leaflet(map_object) %>% 
      addTiles() %>% 
      addPolygons(label = labels)
  } else {
    map_interactive <- leaflet(map_object) %>% 
      addTiles() %>% 
      addMarkers(label = labels)
  }
  # return map
  return(map_interactive)
}

## Communes  ------

map_commune2 <- st_transform(map_commune,"EPSG:4326") %>% 
  st_make_valid()
lab_commune <- map_commune2 %>% 
  left_join(codigos_territoriales) %>% 
  pull(nombre_comuna)
m_commune <- f.interactive.map(map_commune2,lab_commune) 
# rm(lab_commune)
m_commune


# MASAS LACUSTRES ------
lagos_zone <- readRDS(sprintf(url_load_shp,"lagos_zone"))
# map
m_lakes <- f.interactive.map(lagos_zone, lagos_zone$Nombre)
m_lakes

## Areas_Pobladas --------

areas_pobladas <- readRDS(sprintf(url_load_shp,"areas_pobladas"))
m_areas_pobladas <- f.interactive.map(areas_pobladas, areas_pobladas$Localidad)
m_areas_pobladas

### Glaciares -----

glaciares <- readRDS(sprintf(url_load_shp,"glaciares"))
m_glaciares <- f.interactive.map(glaciares, glaciares$NOMBRE)
m_glaciares


### Atractivos Turisticos Nacionales -----

atractivos_turisticos <- readRDS(sprintf(url_load_shp,"atractivos_turisticos"))
m_atractivos_turisticos <- f.interactive.map(atractivos_turisticos, 
                                             atractivos_turisticos$NOMBRE,
                                             polygon_form = F)
m_atractivos_turisticos


### Catastros de Uso de Suelo y Vegetacion -----
uso_suelo <- readRDS(sprintf(url_load_shp,"uso_suelo"))

table(uso_suelo$USO)
m_uso_suelo <- f.interactive.map(uso_suelo, uso_suelo$USO)
m_uso_suelo



## Map with all layers ------

m <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=map_commune2,label = lab_commune,
              group = "Commune",color = "grey") %>% 
  addPolygons(data=lagos_zone,label = lagos_zone$Nombre, 
              group="North-Patagonian Lakes") %>% 
  addPolygons(data=areas_pobladas,label = areas_pobladas$Localidad, 
            group="Areas Pobladas",color="red") %>% 
  addPolygons(data=glaciares, label = glaciares$NOMBRE, 
            group="Glaciares", color="#00008B") %>% 
  addCircles(data=atractivos_turisticos,label = atractivos_turisticos$NOMBRE, 
            group="Atractivos Turisticos", color="green")


# create selectable layers
m <- m %>% 
  addLayersControl(baseGroups = c("OpenStreetMap"),
                   overlayGroups = c("Commune","North-Patagonian Lakes",
                                     "Areas Pobladas","Glaciares",
                                     "Atractivos Turisticos")) %>% 
  hideGroup(c("Commune","North-Patagonian Lakes",
              "Areas Pobladas","Glaciares",
              "Atractivos Turisticos"))
m

mapshot()

# Clean WS -----
rm(url_load_shp)

## EoF