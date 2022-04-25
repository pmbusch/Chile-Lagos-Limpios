## Spatial Layers
## Visualizationg of Spatial Layers
## All previous shapefile layers are loaded and filtered in a different script
## PBH Dec. 2021

# Load libraries and other useful common functions -----
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
  add.Layer(map_region,map_region$nombre_region,
            features=c("Poblacion_2017"),
            group_l = "Region",
            source="Census, 2017",
            color="grey") %>% 
  add.Layer(map_provincia,map_provincia$nombre_provincia,
            features=c("Poblacion_2017"),
            group_l = "Province",
            source="Census, 2017",
            color="grey") %>% 
  add.Layer(map_commune2,map_commune2$nombre_comuna,
            features=c("Poblacion_2017"),
            group_l = "Commune",
            source="Census, 2017",
            color="grey") %>% 
  add.Layer(map_census,map_census$nombre_comuna,
            features=c("Poblacion_2017","geocodigo"),
            group_l = "Poblacion Urbana",
            source="Census, 2017",
            color="red") %>% 
  add.Layer(areas_pobladas,"Areas Pobladas",
            features = c("Localidad","Entidad"),
            source="Biblioteca Congreso Nacional",
            color="red") %>% 
  add.Layer(establecimiento_salud,"Establecimientos Salud",
            features = c("TIPO","AMBITO","DEPEN","CERTIFICA",
            "NIVEL","PRESTADOR","ESTADO","NIVEL_COM","MODALIDAD"),
            source="MINSAL, 2021",
            polygon_form = "p",
            color="orange") %>%
  add.Layer(humedales,"Humedales",
            features = c("Nombre","Clase","SubClase","AreaProteg",
                         "Nombre_AP","Designacio"),
            source="MMA, 2015",
            color="green") %>%
  add.Layer(comunidad_indigena,"Comunidades Indigenas",
            features = c("COMUNIDAD","REGISTRO","FECHA"),
            source="CONADI, 2021",
            color="blue",
            polygon_form = "p") %>%
  add.Layer(glaciares,"Glaciares",
            features = c("NOMBRE","CLASIFICA","VOL_M3","NOMB_CUEN"),
            source="DGA, 2014",
            color="#00008B") %>%
  add.Layer(atractivos_turisticos,"Atractivos Turisticos",
            features = c("NOMBRE","CATEGORIA","TIPO","PROPIEDAD"),
            polygon_form = "p",
            source="SERNATUR, 2020",
            color="green") %>% 
  add.Layer(planta_aguas_servidas,"Planta Aguas Servidas",
            features = c("DESCRIPCIO","SISTEMA","TIP_TRATAM",
                         "RECEPTOR","TIPO_RECEP","CARGA_NKT"),
            polygon_form = "p",
            source="SISS, 2016",
            color="brown") %>% 
  add.Layer(planta_aguas_servidas_ptoDescarga,"Punto Descarga Aguas Servidas",
            features = c("EMPRESA","NOM_OBRA","ESTADO_USO",
                         "RECEPTOR","TIPO_RECEP"),
            polygon_form = "p",
            source="SISS, 2017",
            color="black") %>% 
  add.Layer(aeropuertos,"Aeropuertos y aerodromos",
            features = c("Aerodromo","categoría"),
            polygon_form = "p",
            source="Biblioteca Congreso Nacional",
            color="black") %>% 
  add.Layer(circuitos_turisticos,"Circuitos Turisticos",
            features = c("Circuito","Exten_km"),
            polygon_form = "l",
            source="SERNATUR, 2015",
            color="red") %>% 
  add.Layer(embalses,"Embalses",
            features=c("NOMBRE"),
            polygon_form = "p",
            source="DGA, 2016",
            color="blue") %>% 
  add.Layer(meteorologica,"Estaciones Meteorologicas",
            features = c("NOMBRE","NOM_CUEN","NOM_SUBC"),
            source="DGA, 2019",
            polygon_form = "p",color="black") %>%
  add.Layer(fluviometricas,"Estaciones Fluviometricas",
            features = c("NOMBRE","NOM_CUEN","NOM_SUBC"),
            source="DGA, 2017",
            polygon_form = "p",color="black") %>%
  add.Layer(sedimentometricas,"Estaciones Sedimentometricas",
            features = c("NOMBRE","NOM_CUEN","NOM_SUBC"),
            source="DGA, 2017",
            polygon_form = "p",color="black") %>% 
  add.Layer(industria_forestal,"Industria Forestal",
            features = c("Industria","Movilidad","Rsocial",
                         "Productos","Especies","Residuos"),
            source="MINAGRI, 2018",
            polygon_form = "p",color="green") %>% 
  add.Layer(incendio_forestal,"Incendios Forestales",
            features = c("temporada","ambito","inicio_c",
                         "combus_i"),
            source="CONAF, 2017",
            polygon_form = "p",color="black") %>% 
  add.Layer(uf,"SMA - Proyectos con RCA aprobada (UF)",
          features = c("Nombre","UnidadFiscalizableId","ComunaNombre",
                       "CategoriaEconomicaNombre","SubCategoriaEconomicaNombre",
                       "SiglaInstrumento","FechaActualizacion"),
          source="SMA - SNIFA, 2021",
          polygon_form = "p",color="orange") %>% 
  add.Layer(emisiones_uf,"SMA - Proyectos con RILES declarados",
          features = c("NombreCategoria","RazonSocial","Planta",
                       "PuntoDeDescarga","ComunaNombre","CodigoRETC",
                       "Parametro"),
          source="SMA - SNIFA, 2021",
          polygon_form = "p",color="brown") %>% 
  add.Layer(puentes,"Puentes",
            features = c("NOMBRE_PUENTE","ROL",
                         "LARGO","ANCHO_TOTAL","MAT_ESTRIB","PISO",
                         "MAT_VIGAS"),
            source="MOP, 2018",
            polygon_form = "p",color="black") %>% 
  add.Layer(red_vial,"Red Vial",
            features = c("NOMBRE_CAMINO","CLASIFICACION","CARPETA","KM_F",
                         "ENROLADO","CONCESIONADO","ROL","CALZADA","ORIENTACION"),
            source="MOP, 2021",
            polygon_form = "l",color="grey") %>% 
  add.Layer(predios,"Predios (<10km)",
            features = c("COMUNA"),
            source="MINVU, 2019",
            color="brown") %>%
  add.Layer(uso_suelo,"Uso de Suelo: Terrenos Agricolas (<10km)",
            features = c("USO","USO_TIERRA","SUBUSO","SUPERF_HA"),
            source="CONAF, 2016",
            color="green")

# menu of layers: https://stackoverflow.com/questions/67496042/leaflet-groupedlayercontrol-using-group-layers-in-r
  
# Could automatize this part, don't know how yet
groupsId <- c("North-Patagonian Lakes",
              # administrative boundaries
              "Region","Province","Commune",
              # demography
              "Poblacion Urbana",
              "Areas Pobladas",
              "Comunidades Indigenas",
              "Predios (<10km)",
              # socioeconomic
              "Establecimientos Salud",
              # industry
              "Planta Aguas Servidas",
              "Industria Forestal",
              "SMA - Proyectos con RCA aprobada (UF)",
              # tourism
              "Atractivos Turisticos",
              "Circuitos Turisticos",
              # environment
              "Uso de Suelo: Terrenos Agricolas (<10km)",
              "Humedales",
              "Glaciares",
              "Punto Descarga Aguas Servidas",
              "SMA - Proyectos con RILES declarados",
              "Incendios Forestales",
              "Estaciones Meteorologicas",
              "Estaciones Fluviometricas",
              "Estaciones Sedimentometricas",
              # infrastructure
              "Red Vial",
              "Puentes",
              "Embalses",
              "Aeropuertos y aerodromos")

# create selectable layers
m <- m %>% 
  addLayersControl(baseGroups = c("OpenStreetMap"),
                   overlayGroups = groupsId) %>% 
  hideGroup(groupsId[-1])

m


mapshot(m, "Figures/Maps/CLL-Map.html", selfcontained=F)
rm(m)

# Clean WS -----
rm(url_load_shp)


# test: 
# https://stackoverflow.com/questions/67496042/leaflet-groupedlayercontrol-using-group-layers-in-r

## EoF