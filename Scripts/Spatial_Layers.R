## Spatial Layers
## Loading and visualizationg of Spatial Layers
## PBH Dec. 2021


# Load libraries and other useful common functions
source("Scripts/00-Common.R", encoding = "UTF-8")


## Spatial Interactive Visualization function ----
# Create spatial layer map with labels, to quickly visualize it
# Input: sf object (spatial vector)
f.interactive.map <- function(map_object, label_vector){
  
  # Create labels for the vectors
  labels <- sprintf(
    "<strong>%s</strong><br/>",
    label_vector) %>% 
    lapply(HTML)
  
  # create interactive map object
  map_interactive <- leaflet(map_object) %>% 
    addTiles() %>% 
    addPolygons(label = labels)
  
  # return map
  return(map_interactive)
}

# Common spatial overlay - 14 communes -----
# We can use the 14 communes of interest to overlay spatially all objects
# First we need to create it and visualize it

map_commune2 <- st_transform(map_commune,"EPSG:4326") %>% 
  st_make_valid()

lab_commune <- map_commune2 %>% 
  left_join(codigos_territoriales) %>% 
  pull(nombre_comuna)

m_commune <- f.interactive.map(map_commune2,lab_commune) 
# rm(lab_commune)
m_commune


## Catastro Lagos -----
# Note: Catrastro Lagos from IDE was incomplete
# 
# lagos <- st_read(sprintf(url_file,
#                          "Spatial Data/IDE/catastro_de_lagos/catastro_de_lagos.shp"))
# lagos <- st_transform(lagos,"EPSG:4326") %>% st_make_valid()
# 
# lagos_zone <- lagos %>% filter(AREA_KM2>1.8) # to avoid small water bodies
# 
# # Spatial filter using 14 communes
# lagos_zone <- st_filter(lagos_zone,map_commune2) %>% 
#   filter(!is.na(NOMBRE) & !(NOMBRE %in% c("LAGUNA CAYUTUE", # border case
#                                         "LAGUNA BONITA",
#                                         "LAGUNA PARAISO")))
# # NOTE: LAGO TINQUILCO is missing from the original data set
# 
# # map
# m_lakes <- f.interactive.map(lagos_zone, lagos_zone$NOMBRE)
# m_lakes
# 
# rm(lagos)



# mapshot(m_lakes, "Figures/Maps/Lakes.html", 
#         selfcontained=F)



# MASAS LACUSTRES ------
lagos <- st_read(sprintf(url_file,
                         "Spatial Data/Biblioteca Congreso Nacional/Masas_Lacustres/masas_lacustres.shp"))
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

# map
m_lakes <- f.interactive.map(lagos_zone, lagos_zone$Nombre)
m_lakes

rm(lagos)


## Areas_Pobladas --------
areas_pobladas <- st_read(sprintf(url_file,
                         "Spatial Data/Biblioteca Congreso Nacional/Areas_Pobladas/Areas_Pobladas.shp"))
areas_pobladas <- st_transform(areas_pobladas,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
areas_pobladas <- st_filter(areas_pobladas,map_commune2)

m_areas_pobladas <- f.interactive.map(areas_pobladas, areas_pobladas$Localidad)
m_areas_pobladas


### Estacion Calidad de agua ------
estacionCalidadAgua <- st_read(sprintf(url_file,
                                  "Spatial Data/IDE/Estadodeestacindecalidaddeaguas/Estadodeestacindecalidaddeaguas.shp"))
estacionCalidadAgua <- st_transform(estacionCalidadAgua,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
estacionCalidadAgua <- st_filter(estacionCalidadAgua,map_commune2)
# 0 IN THE ZONE

### Vertederos Ilegales -----
VertederosIlegales <- st_read(sprintf(url_file,
                                       "Spatial Data/IDE/VertederosIlegales/VertederosIlegales.shp"))
VertederosIlegales <- st_transform(VertederosIlegales,"EPSG:4326")

# Spatial filter using 14 communes - to obtain the 23 lakes
VertederosIlegales <- st_within(VertederosIlegales,map_commune2)
# 0 IN THE ZONE

# leaflet(VertederosIlegales) %>% 
#   addTiles() %>% 
#   addMarkers()


### Area Vulnerable -----
## IS BASICALLY A CONTOUR FOR CHILE


### Glaciares -----
glaciares <- st_read(sprintf(url_file,
                                      "Spatial Data/IDE/IPG2014/IPG2014.shp"))
glaciares <- st_transform(glaciares,"EPSG:4326") %>% st_make_valid()

# Spatial filter using 14 communes - to obtain the 23 lakes
glaciares <- st_filter(glaciares,map_commune2)

m_glaciares <- f.interactive.map(glaciares, glaciares$NOMBRE)
m_glaciares

### Atractivos Turisticos Nacionales -----

### Zonas de Interes Turistico -----

### Catastros de Uso de Suelo y Vegetacion -----







## JOIN ALL ------


m <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=map_commune2,
              label = lab_commune,group = "Commune") %>% 
  addPolygons(data=lagos_zone,
              label = lagos_zone$Nombre, group="North-Patagonian Lakes")

# create selectable layers
m <- m %>% 
  addLayersControl(baseGroups = c("OpenStreetMap"),
                   overlayGroups = c("Commune","North-Patagonian Lakes")) %>% 
  hideGroup(c("Commune","North-Patagonian Lakes"))
m



# Clean WS -----
rm(m_lakes, lagos_zone, m_commune, areas_pobladas, m_areas_pobladas,
   estacionCalidadAgua,VertederosIlegales,areavulnerable)

## EoF