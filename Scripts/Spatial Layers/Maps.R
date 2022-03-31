## Maps
## Some useful maps
## PBH Oct. 2021

source("Scripts/00-Common.R", encoding = "UTF-8")

# Poulation by commune
pop <- censo_2017_comunas %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  group_by(codigo_comuna) %>% 
  summarise(population2017=sum(poblacion,na.rm=T)) %>% ungroup()

sum(pop$population2017)

# Population by Census District
pop_census <- censo_2017_zonas %>% 
  left_join(mapa_zonas) %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  group_by(geocodigo) %>% 
  summarise(population2017=sum(poblacion,na.rm=T)) %>% ungroup()
sum(pop_census$population2017)

# Maps ------
map_commune <- mapa_comunas %>% st_as_sf() %>%
  left_join(codigos_territoriales) %>% 
  filter(nombre_comuna %in% comunes_cll) %>% 
  left_join(pop)

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
  right_join(pop_census)

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