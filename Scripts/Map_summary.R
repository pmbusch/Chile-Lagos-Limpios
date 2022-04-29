## Summary map for policy brief

## PBH April 2022


source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/00-Functions_Spatial.R",encoding = "UTF-8")
url_load_shp <- "Data/Spatial Data/%s.rds"


## Communes  and labels ------

map_commune2 <- readRDS(sprintf(url_load_shp,"commune_pop"))
map_provincia <- readRDS(sprintf(url_load_shp,"map_provincia"))
map_region <- readRDS(sprintf(url_load_shp,"map_region"))
map_census <- readRDS(sprintf(url_load_shp,"map_census"))


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
incendio_forestal <- readRDS(sprintf(url_load_shp,"incendio_forestal"))
map_distrito <- readRDS(sprintf(url_load_shp,"map_distrito"))
meteorologica <- readRDS(sprintf(url_load_shp,"meteorologica"))
planta_aguas_servidas <- readRDS(sprintf(url_load_shp,"planta_aguas_servidas"))
planta_aguas_servidas_ptoDescarga <- readRDS(sprintf(url_load_shp,"planta_aguas_servidas_ptoDescarga"))
puentes <- readRDS(sprintf(url_load_shp,"puentes"))
sedimentometricas <- readRDS(sprintf(url_load_shp,"sedimentometricas"))
sello_calidad_turistica <- readRDS(sprintf(url_load_shp,"sello_calidad_turistica"))
sendero_chile <- readRDS(sprintf(url_load_shp,"sendero_chile"))
sitios_prioritarios <- readRDS(sprintf(url_load_shp,"sitios_prioritarios"))
comunidad_indigena <- readRDS(sprintf(url_load_shp,"comunidad_indigena"))
humedales <- readRDS(sprintf(url_load_shp,"humedales"))
uf <- readRDS(sprintf(url_load_shp,"uf"))
emisiones_uf <- readRDS(sprintf(url_load_shp,"emisiones_uf"))
puentes <- readRDS(sprintf(url_load_shp,"puentes"))
red_vial <- readRDS(sprintf(url_load_shp,"red_vial"))
predios <- readRDS(sprintf(url_load_shp,"predios"))
uso_suelo <- readRDS(sprintf(url_load_shp,"uso_suelo"))

# GGMAP ------------

# coordinates for the box
coord_extent <- st_bbox(lagos_zone)
names(coord_extent) <- c("left","bottom","right","top")

## add more margins
coord_extent <- coord_extent + c(-1.5,-0.8,1.5,0.8)


map_base <- get_map(location = coord_extent,
                # zoom=12,
                source = "osm",
                # maptype = "terrain", crop=F
                
                )

# saveRDS(map_base,"Data/map_base.rds")
# map_base <- readRDS("Data/map_base.rds")

add_coordinates <- function(df){
  return(df %>% 
           mutate(Longitude=map_dbl(geometry, ~st_centroid(.x)[[1]]),
                  Latitude=map_dbl(geometry, ~st_centroid(.x)[[2]])))
}

comunidad_indigena <- comunidad_indigena %>% 
  mutate(Longitude=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         Latitude=map_dbl(geometry, ~st_centroid(.x)[[2]]))
lagos_zone <- add_coordinates(lagos_zone)


ggmap(map_base)+
  geom_label(data=lagos_zone,
             aes(Longitude,Latitude,label=Nombre))+
  geom_point(data=comunidad_indigena,
             mapping=aes(Longitude,Latitude))



# leaflet
m <- leaflet(
  options = leafletOptions(attributionControl=FALSE) # hide source OSM
  ) %>% 
  addTiles() %>% 
  # addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}') %>% 
  add.Layer(lagos_zone,lagos_zone$Nombre,
            pos_leg = "bottomright",
            features=c("Tipo"),
            group_l = "North-Patagonian Lakes",
            weight=2,
            opacity=1,
            fillOpacity=0.3,
            color="blue") %>% 
  add.Layer(map_census,map_census$nombre_comuna,
            pos_leg = "bottomright",
            features=c("Poblacion_2017","geocodigo"),
            group_l = "Urban Population",
            source="Census, 2017",
            weight = 2,
            opacity=1,
            fillOpacity=0.7,
            color="red") %>% 
  add.Layer(humedales,"Wetlands",
            pos_leg = "bottomright",
            features = c("Nombre","Clase","SubClase","AreaProteg",
                         "Nombre_AP","Designacio"),
            source="MMA, 2015",
            weight = 2,
            opacity=0.9,
            fillOpacity=0.5,
            color="brown") %>%
  add.Layer(comunidad_indigena,"Indigenous Communities",
            pos_leg = "bottomright",
            features = c("COMUNIDAD","REGISTRO","FECHA"),
            source="CONADI, 2021",
            color="purple",
            weight = 2,
            radius=5,
            opacity=0.5,
            polygon_form = "p") %>%

  add.Layer(planta_aguas_servidas_ptoDescarga,"Waste water plants",
            pos_leg = "bottomright",
            features = c("EMPRESA","NOM_OBRA","ESTADO_USO",
                         "RECEPTOR","TIPO_RECEP"),
            polygon_form = "p",
            weight = 6,
            radius=8,
            opacity=0.9,
            source="SISS, 2017",
            color="black") %>% 
  add.Layer(industria_forestal,"Forest industry",
            pos_leg = "bottomright",
            features = c("Industria","Movilidad","Rsocial",
                         "Productos","Especies","Residuos"),
            source="MINAGRI, 2018",
            weight = 3,
            radius=6,
            opacity=0.8,
            polygon_form = "p",color="green") %>% 
  add.Layer(emisiones_uf,"Industrial water emissions",
            pos_leg = "bottomright",
            features = c("NombreCategoria","RazonSocial","Planta",
                         "PuntoDeDescarga","ComunaNombre","CodigoRETC",
                         "Parametro"),
            weight = 6,
            radius=8,
            opacity=0.9,
            source="SMA - SNIFA, 2021",
            polygon_form = "p",color="#636363") %>% 
  add.Layer(red_vial,"Road Infrastructure",
            pos_leg = "bottomright",
            features = c("NOMBRE_CAMINO","CLASIFICACION","CARPETA","KM_F",
                         "ENROLADO","CONCESIONADO","ROL","CALZADA","ORIENTACION"),
            source="MOP, 2021",
            weight = 2,
            opacity=0.7,
            polygon_form = "l",color="grey")

# Could automatize this part, don't know how yet
groupsId <- c("North-Patagonian Lakes",
              "Urban Population",
              "Indigenous Communities",
              "Waste water plants",
              "Forest industry",
              "Wetlands",
              "Industrial water emissions",
              "Road Infrastructure")

# create selectable layers
m <- m %>% 
  addLayersControl(baseGroups = c("OpenStreetMap"),
                   overlayGroups = groupsId,
                   position = "topleft")

m


mapshot(m, "Figures/Maps/PolicyBrief_Map.html", selfcontained=F)
rm(m)





