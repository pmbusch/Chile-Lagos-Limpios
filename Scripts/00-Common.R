## Functions and Common Parameters
## Some useful functions, libraries and common parameters to load
## PBH Oct. 2021


## Libraries --------------
list_libraries <- c("tidyverse", "kableExtra","chilemapas","lwgeom","sf","htmltools",
                    "flextable", "filesstrings","purrr", "readr", "extrafont",
                    "stringr", "ggmap", "readxl", "leaflet", "mapview", "purrr",
                    "skimr", "readr", "patchwork", "ggridges", "lubridate",
                    "ggrepel","mapview","officer")
lapply(list_libraries, require, character.only = TRUE)
rm(list_libraries) 

## Functions ----------

f_remover_acentos <- function(x){
  x %>% 
    str_replace_all("á","a") %>% 
    str_replace_all("é","e") %>% 
    str_replace_all("í","i") %>% 
    str_replace_all("ó","o") %>% 
    str_replace_all("ú","u") %>% 
    str_replace_all("ñ","n") %>% 
    str_replace_all("Ñ","N") %>% 
    str_replace_all("Á","A") %>% 
    str_replace_all("É","E") %>% 
    str_replace_all("Í","I") %>% 
    str_replace_all("Ó","O") %>% 
    str_replace_all("Ú","U")
}

## Save Plot
f_savePlot <- function(p1, file_path, dpi=600){
  cat("Saving: ",file_path)
  ggsave(file_path, {{p1}},dpi=dpi,
         width = 14.87, height = 9.30, units = "in")
}

## Save CSV
f_saveCsv <- function(datos, file_path){
  cat("Saving: ",file_path)
  cat('sep=; \n',file = file_path)
  write.table(datos, file_path,
              sep=';',row.names = F, append = T)
}

## Parameters ---------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

# Communes of interest for the lakes
comunes_cll <- c("Cunco","Pucon","Villarrica","Puerto Montt","Puerto Varas",
                 "Puyehue","Puerto Octay","Llanquihue","Frutillar","Rio Bueno",
                 "Panguipulli","Futrono","Lago Ranco","Los Lagos")
comunes_cll_codes <- c("9103","9120","10101","10105","10107","10109","9115",
                       "10302","10304","14104","14108","14202","14203","14204")
comunes_cll_codes2 <- c("09103","09120","10101","10105","10107","10109","09115",
                       "10302","10304","14104","14108","14202","14203","14204")


# Maps with some spatial data
map_commune <- mapa_comunas %>% st_as_sf() %>% 
  filter((codigo_comuna %in% comunes_cll_codes|codigo_comuna %in% comunes_cll_codes2)) %>% 
  mutate(area_commune=st_area(geometry) %>% as.numeric(),
         perimeter_commune=st_length(geometry) %>% as.numeric(),
         latitude_commune=map_dbl(geometry, ~st_centroid(.x)[[2]]))

url_file <- "G:/My Drive/Pablo/Educacion/UC Davis/Practicum Chile Lagos Limpios/Objective 2 - Data Analysis/Data/%s"

## EoF