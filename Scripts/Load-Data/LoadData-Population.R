## Load Data - Population Census 2017
## Data about boundaries and physical information about lakes
## PBH Oct. 2021

# source("Scripts/00-Common.R", encoding = "UTF-8")

## 2017 Population by Census District ----
cod_commune <- read_csv2(
  sprintf(url_file,
          "Censo2017/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Identificacion_Geografica/Microdato_Censo2017-Comunas.csv"))

pop_district <- read_csv2(sprintf(url_file,
                                  "Censo2017/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas.csv"),
                          na = "*")

## Pop Trend --------
pop_proj <- read_csv2(
  sprintf(url_file,
          "Proyeccion Poblacion/estimaciones-y-proyecciones-2002-2035-comuna-y-area-urbana-y-rural.csv"))
names(pop_proj)


# DATA WRANGLING ------------
## Trend Population to 2035 by commune ---- 
pop_commune <- pop_proj %>% 
  filter(Comuna %in% comunes_cll_codes) %>% 
  mutate(codigo_comuna=if_else(str_length(Comuna)==4,
                               paste0("0",Comuna),
                               paste0("",Comuna)))

# pop_commune$codigo_comuna %>% unique()
names(pop_commune) <- names(pop_commune) %>% f_remover_acentos() %>% 
  str_replace_all(" ","_")

# Data wrangling
pop_commune_wide <- pop_commune %>% 
  pivot_longer((10:43),names_to ="year",values_to="population",) %>% 
  mutate(year=str_remove_all(year,"Poblacion_"),
         Nombre_Comuna=f_remover_acentos(Nombre_Comuna))

pop_commune_cll <- pop_commune_wide %>% 
  group_by(Nombre_Comuna,codigo_comuna,year) %>% 
  summarise(pop=sum(population,na.rm=T))

# EoF