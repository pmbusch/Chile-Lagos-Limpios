## Analysis on SMA Data on RILES
## PBH March 2022
# Riles: Residuos líiuidos industriales (water emissions)
# UF: Unidad Fiscalizable (basic unit of a project)

# LIBRARY AND PARAMETERS ------
source("Scripts/00-Common.R", encoding = "UTF-8")

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
  filter((ComunaNombre %in% comunes_cll))
# unique(uf$ComunaNombre)


# create sf object

# MAP ----
leaflet(uf) %>%
  addTiles() %>%
  addCircleMarkers(lng=~Longitud,
                   lat=~Latitud,
                   label = ~as.character(Nombre))





## EoF