## Analysis on SMA Data on UF
## PBH March 2022
# UF: Unidad Fiscalizable (basic unit of a project)

# LIBRARY AND PARAMETERS ------
source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/00-Functions_Spatial.R",encoding = "UTF-8")

## Folder directions
url_file_sma <- sprintf(url_file,
                        "SMA/%s")


# LOAD DATA ----

uf <- read_delim(sprintf(url_file_sma,"UF/UF_Instrumentos_Act2021-10-28.csv"),
                 delim=";",
                 locale = locale(encoding = "windows-1252"))
# names(uf)

# DATA WRANGLING ----

# filter by commune
uf <- uf %>%
  filter((ComunaNombre %in% comunes_cll)|
           (str_detect(ComunaNombre, "Bueno$")))
# unique(uf$ComunaNombre)
# unique(uf$RegionNombre)

# filter only places with LatLong valid
# uf %>% select(Latitud,Longitud) %>% skim_without_charts()
# Some places have the lat/long mixed, we fixed that
latlong_mixed <- c("CONSERVERA SACRAMENTO","CAJA DE COMPENSACION LOS HEROES",
                   "CAMPING TOTORAL","EMPRESTITO PELLINES")
uf <- uf %>% 
  mutate(lat_aux=Latitud,
         Latitud=if_else(Nombre %in% latlong_mixed,Longitud,Latitud),
         Longitud=if_else(Nombre %in% latlong_mixed,lat_aux,Longitud),
         lat_aux=NULL)
rm(latlong_mixed)

# create sf object
uf <- uf %>% st_as_sf(coords=c("Longitud","Latitud"),crs=4326)

# Factors
# unique(uf$CategoriaEconomicaNombre)
# unique(uf$SubCategoriaEconomicaNombre)
uf <- uf %>% mutate(CategoriaEconomicaNombre=as.factor(CategoriaEconomicaNombre))


# MAP ----

# If you want to use predefined palettes in the RColorBrewer package:
# Call RColorBrewer::display.brewer.all() to see all possible palettes
pal <- colorFactor(palette = 'Paired',domain = uf$CategoriaEconomicaNombre)
labels_uf <- f.create.labels(uf,"UF",
                             c("Nombre",
                               "SiglaInstrumento",
                               "CategoriaEconomicaNombre",
                               "SubCategoriaEconomicaNombre",
                               "UnidadFiscalizableId",
                               "ComunaNombre"))
map_uf <- leaflet(uf) %>%
  addTiles() %>%
  addCircles(label=labels_uf,
             color=~pal(CategoriaEconomicaNombre),
             labelOptions=label_options) %>% 
  addLegend(pal = pal, 
            values = ~CategoriaEconomicaNombre,
            title = "Categoria Economica")
# map_uf
mapshot(map_uf, "Figures/Maps/UF.html", selfcontained=F)
rm(map_uf,labels_uf)

## EoF