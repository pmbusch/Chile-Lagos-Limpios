## Load Data - Boundaries and administrative information
## Data about boundaries and physical information about lakes
## PBH Oct. 2021

# source("Scripts/00-Common.R", encoding = "UTF-8")


# Load Physical information -------
# Source: Excel by previous work of Chile Lagos Limpios

# Load Indicators
physical_ind <- read_excel(sprintf(url_file,"Inventario Lagos Nor-Patagonicos.xlsx"),
                           "Physical Indicators",
                           range = "B3:AD26")

# remove accents and spaces
names(physical_ind) <- physical_ind %>% names() %>% f_remover_acentos() %>% 
  str_remove_all("\\*|\\^|º|\\[|\\]") %>% str_replace_all(" ","_")

# Function to convert form the string format to lat.long coordinates
f_extract_latLong <- function(x){
  degree <- x %>% str_split(" ", simplify=T) %>% .[,1] %>% str_remove("G")
  minutes <- x %>% str_split(" ", simplify=T) %>% .[,2] %>% str_remove("M")
  return(-1*as.numeric(degree)-as.numeric(minutes)/60)
}

## Adjust the latitude and longitude
physical_ind <- physical_ind %>% 
  mutate(Latitude=f_extract_latLong(Latitude),
         Longitude=f_extract_latLong(Longitude)) %>% 
  rename(Lake=Name,
         Commune=Comune)

# create categories ordered by latitude
levels_region <- reorder(physical_ind$Region,physical_ind$Latitude) %>% 
  unique() %>% as.character()
levels_province <- reorder(physical_ind$Province,physical_ind$Latitude) %>% 
  unique() %>% as.character()
levels_commune <- reorder(physical_ind$Commune,physical_ind$Latitude) %>% 
  unique() %>% as.character()
levels_lake <- reorder(physical_ind$Lake,physical_ind$Latitude) %>% 
  unique() %>% as.character()

physical_ind <- physical_ind %>% 
  mutate(Region=factor(Region,levels_region),
         Province=factor(Province,levels_province),
         Commune=factor(Commune,levels_commune),
         Lake=factor(Lake,levels_lake))

rm(levels_region,levels_province,levels_commune,levels_lake)

# Check map
# leaflet(physical_ind) %>%
#   addTiles() %>%
#   addCircleMarkers(lng=~Longitude,
#                    lat=~Latitude,
#                    label = ~as.character(Lake))


# Load cities associated with lakes -----
# Source: Based on Map inspection of District Census polygon in the shore of lakes
cities_lakes <- read_excel(sprintf(url_file,"Cities-Lakes.xlsx"), 
                           sheet = "Cities-Lakes", range = "A1:C13")
names(cities_lakes) <- cities_lakes %>% names() %>% f_remover_acentos() %>% 
  str_remove_all("\\*|\\^|º|\\[|\\]") %>% str_replace_all(" ","_")

# Lake Tahoe Characteristics ------
# Source: https://www.epa.gov/lake-tahoe/about-lake-tahoe
# https://www.webcitation.org/5yfL00HSt?url=http://tahoe.usgs.gov/facts.html
# conversion:s 1 mile square= 2.59 km2, 1ft=0.3048m
tahoe <- data.frame(Name="Lake Tahoe",
                    Watershed_km2=501*2.59,
                    Area_km2=192*2.59,
                    Avg_depth_m=1000*0.3048,
                    Max_depth_m=1645*0.3048) %>% 
  mutate(Volume_km3=Area_km2*Avg_depth_m/1000)


# EoF