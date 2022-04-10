## Maps
## Some useful maps
## PBH Oct. 2021

source("Scripts/00-Common.R", encoding = "UTF-8")

## Population by different aggregations ------

# Poulation by commune
pop <- censo_2017_comunas %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  group_by(codigo_comuna) %>% 
  summarise(population2017=sum(poblacion,na.rm=T),
            Poblacion_2017=population2017 %>% 
              formatC(format="f", big.mark = " ", digits=0)) %>% ungroup()

sum(pop$population2017)

# Population by Census District
pop_census <- censo_2017_zonas %>% 
  left_join(mapa_zonas) %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  group_by(geocodigo) %>% 
  summarise(population2017=sum(poblacion,na.rm=T),
            Poblacion_2017=population2017 %>% 
              formatC(format="f", big.mark = " ", digits=0)) %>% ungroup()
sum(pop_census$population2017)

# Poulation by province
provinces_cll <- codigos_territoriales %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  pull(nombre_provincia) %>% unique()

pop_prov <- censo_2017_comunas %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_provincia %in% provinces_cll) %>% 
  group_by(codigo_provincia) %>% 
  summarise(population2017=sum(poblacion,na.rm=T),
            Poblacion_2017=population2017 %>% 
              formatC(format="f", big.mark = " ", digits=0)) %>% ungroup()

# Poulation by region
region_cll <- codigos_territoriales %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  pull(nombre_region) %>% unique()

pop_reg <- censo_2017_comunas %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_region %in% region_cll) %>% 
  group_by(codigo_region) %>% 
  summarise(population2017=sum(poblacion,na.rm=T),
            Poblacion_2017=population2017 %>% 
              formatC(format="f", big.mark = " ", digits=0)) %>% ungroup()


# Maps ------
map_commune <- mapa_comunas %>% st_as_sf() %>%
  left_join(codigos_territoriales) %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  left_join(pop)

saveRDS(map_commune,"Data/Spatial Data/commune_pop.rds")

st_crs(map_commune) <- "EPSG:4326"

labels <- sprintf(
  "<strong>%s</strong><br/> %s - %s hab",
  map_commune$nombre_comuna, map_commune$nombre_comuna,
  map_commune$population2017) %>% 
  lapply(HTML)

pal <- colorBin("YlOrRd", bins = 9, domain=map_commune$population2017)


# Interactive Map
m <- leaflet(map_commune) %>% 
  addTiles() %>% 
  addPolygons(
    group = "Population",
    # fill
    fillColor   = ~pal(population2017),
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels) %>% 
  addLegend(
    group = "Population",
    pal = pal, 
    values = ~population2017, 
    opacity = 0.7,
    title = HTML(paste("Population", " [","habs.","]",sep="")),
    position = "bottomleft") %>% 
  # Layers control
  addLayersControl(
    # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Population"),
    options = layersControlOptions(collapsed = FALSE)
  )

mapshot(m, "Figures/Maps/PopCommune.html", 
        selfcontained=F)

# Map Zones -------
map_zone <- mapa_zonas %>% st_as_sf() %>%
  right_join(pop_census) %>% left_join(codigos_territoriales)

saveRDS(map_zone,"Data/Spatial Data/map_census.rds")

st_crs(map_zone) <- "EPSG:4326"

labels <- sprintf(
  "<strong>%s</strong><br/> %s - %s hab",
  map_zone$geocodigo, map_zone$geocodigo,
  map_zone$population2017) %>% 
  lapply(HTML)

pal <- colorBin("YlOrRd", bins = 9, domain=map_zone$population2017)


# Interactive Map
m2 <- leaflet(map_zone) %>% 
  addTiles() %>% 
  addPolygons(
    group = "Population",
    # fill
    fillColor   = ~pal(population2017),
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels) %>% 
  addLegend(
    group = "Population",
    pal = pal, 
    values = ~population2017, 
    opacity = 0.7,
    title = HTML(paste("Population", " [","habs.","]",sep="")),
    position = "bottomleft") %>% 
  # Layers control
  addLayersControl(
    # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Population"),
    options = layersControlOptions(collapsed = FALSE)
  )

mapshot(m2, "Figures/Maps/PopDistrict.html", 
        selfcontained=F)


## region and prov ----
## Region -----
map_region <- chilemapas::generar_regiones() %>% st_as_sf() %>% 
  filter(codigo_region %in% c("09","10","14")) %>% 
  mutate(area_region=st_area(geometry) %>% as.numeric(),
         perimeter_region=st_length(geometry) %>% as.numeric(),
         latitude_region=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_transform("EPSG:4326") %>% 
  st_make_valid()

cod_territoriales_region <- codigos_territoriales %>% 
  group_by(codigo_region,nombre_region) %>% tally() %>% 
  dplyr::select(-n)
map_region <- map_region %>% 
  left_join(cod_territoriales_region) %>% 
  left_join(pop_reg)
rm(cod_territoriales_region)

saveRDS(map_region,"Data/Spatial Data/map_region.rds")
rm(map_region)


## Provincia -----
map_provincia <- chilemapas::generar_provincias() %>% st_as_sf() %>% 
  filter(codigo_region %in% c("09","10","14")) %>% 
  mutate(area_provincia=st_area(geometry) %>% as.numeric(),
         perimeter_provincia=st_length(geometry) %>% as.numeric(),
         latitude_provincia=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_transform("EPSG:4326") %>% 
  st_make_valid()

cod_territoriales_provincia <- codigos_territoriales %>% 
  group_by(codigo_region,nombre_region, 
           codigo_provincia,nombre_provincia) %>% tally() %>% 
  dplyr::select(-n)
map_provincia <- map_provincia %>% 
  left_join(cod_territoriales_provincia) %>% 
  left_join(pop_prov)
rm(cod_territoriales_provincia)


saveRDS(map_provincia,"Data/Spatial Data/map_provincia.rds")
rm(map_provincia)



## ggmap ------

leaflet(physical_ind) %>%
  addTiles() %>%
  addCircleMarkers(lng=~Longitude,
                   lat=~Latitude,
                   label = ~as.character(Lake))



e <- 1
location <- c(min(physical_ind$Longitude)-e, min(physical_ind$Latitude)-e, 
              max(physical_ind$Longitude)+e, max(physical_ind$Latitude)+e)

map1 <- get_map(location = location,
                # zoom=12,
                source = "osm", 
                # maptype = "terrain", crop=F
                )

ggmap(map1)+
  geom_point(data=physical_ind,
             mapping=aes(Longitude,Latitude))+
  geom_text_repel(data=physical_ind,
                   mapping=aes(Longitude,Latitude,label=Lake),
                  nudge_x = ifelse(physical_ind$Longitude>-72,2,-3))+
  labs(x="Long.",y="Lat.")+xlim(-75,-69)


f_savePlot(last_plot(),"Figures/map.png")

## EoF