## Load SMA Data on RILES and prepare for Analysis
## PBH March 2022
# Riles: Residuos líiuidos industriales (water emissions)
# UF: Unidad Fiscalizable (basic unit of a project)

# LIBRARY AND PARAMETERS ------
source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/00-Functions_Spatial.R",encoding = "UTF-8")

## Folder directions
url_file_sma <- sprintf(url_file,
                        "SMA/%s")


# LOAD DATA ----

## Unidad Fiscalizable -----
# We need the name of the UF of interest
uf <- read_delim(sprintf(url_file_sma,"UF/UF_Instrumentos_Act2021-10-28.csv"),
                 delim=";",
                 locale = locale(encoding = "windows-1252"))



uf <- uf %>%
  filter((ComunaNombre %in% comunes_cll)|
           (str_detect(ComunaNombre, "Bueno$")))
nombres_uf <- unique(uf$Nombre)


## RILES -----
# Note: RPM: Resoluciones Programadas de Monitoreo (RPM)
# 2020

# All files
emisiones_files <- list.files(sprintf(url_file_sma,
                                      "RILES/2020/EMISIONES"))
emisiones <- data.frame()

# concatenate all files, should be 30 columns
# Filter to avoid a really big file (by zone of interest)
for (i in 1:length(emisiones_files)){
  # read file
  emisiones_aux <-read_delim(sprintf(url_file_sma,
                                      paste0(
                                        "RILES/2020/EMISIONES/",
                                        emisiones_files[i])),
                        delim=";",
                        # col_types = "Dccccccccccdddcccdcdcccdddcccc",
                        locale = locale(encoding = "UTF-16",
                                        decimal_mark = ","))
  # Filter by UF of interest 
  emisiones_aux <- emisiones_aux %>% 
    filter(UnidadFiscalizable %in% nombres_uf)
  
  names(emisiones_aux) <- names(emisiones_aux) %>% f_remover_acentos() %>% 
    str_replace_all(" ","_")
             
  # concatenate
  emisiones <- rbind(emisiones,emisiones_aux)
  rm(emisiones_aux)
  cat("File :", i,"\n")
}
rm(emisiones_files)

# check dates
emisiones$PeriodoInforme %>% unique()
  
# DATA WRANGLING ----

# table(emisiones$UnidadFiscalizable)
# emisiones$ComunaNombre %>% unique()

# emisiones %>% select(Latitud,Longitud) %>% skim_without_charts()

# Filter locations with no lat/long coordinates
emisiones <- emisiones %>% 
  filter(!is.na(Latitud)) %>% filter(!is.na(Longitud))

# month
emisiones <- emisiones %>% 
  mutate(month=month(PeriodoInforme))
# emisiones$month %>% unique()

# create sf object
emisiones <- emisiones %>% st_as_sf(coords=c("Longitud","Latitud"),crs=4326)

# Emisions work ------
emisiones$Parametro %>% table() %>% sort()


# Factors
# unique(uf$CategoriaEconomicaNombre)
# unique(uf$SubCategoriaEconomicaNombre)
uf <- uf %>% mutate(CategoriaEconomicaNombre=as.factor(CategoriaEconomicaNombre))
emisiones <- emisiones %>% 
  mutate(NombreCategoria=as.factor(NombreCategoria))


# Save Data ------------
saveRDS(emisiones,"Data/SMA/emisiones_RILES.rds")


## EoF