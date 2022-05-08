## Population Analysis on the Zone
## Population Analysis at 2017
## PBH Oct. 2021

# Common functions and parameters
# source("Scripts/00-Common.R", encoding = "UTF-8")

file_save <- "Figures/Population/%s.png"


# LOAD DATA ---------

## 2017 Population by Census District ----
cod_commune <- read_csv2(
  sprintf(url_file,
          "Censo2017/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Identificacion_Geografica/Microdato_Censo2017-Comunas.csv"))

pop_district <- read_csv2(
  sprintf(url_file,"Censo2017/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas.csv"),
  na = "*")

# str(pop_district)
names(pop_district)
sum(pop_district$PERSONAS) # Check total population 17,574,003

# DATA WRANGLING ------------

## 2017 Population by commune ----
pop_commune <- pop_district %>% 
  group_by(COMUNA) %>%
  # mutate(PERSONAS=as.numeric(PUEBLO)) %>% # to compute number of PUEBLO easily
  summarize(pop=sum(PERSONAS,na.rm=T)) %>% 
  left_join(cod_commune)

pop_commune$pop %>% sum()

# pon region
pop_commune %>% 
  mutate(codigo_comuna=paste0(if_else(str_length(COMUNA)==4,"0",""),COMUNA)) %>% 
  left_join(codigos_territoriales) %>% 
  filter(nombre_region %in% c("Los Rios","La Araucania","Los Lagos")) %>% 
  pull(pop) %>% sum()

pop_commune_cll <- pop_commune %>% filter(COMUNA %in% comunes_cll_codes)
pop_commune_cll$pop %>% sum()



# FIGURES -------------
fig_pop <- pop_commune_cll %>% 
  mutate(pop=pop/1000) %>% 
  ggplot(aes(reorder(NOM_COMUNA,pop),pop))+
  geom_col(fill="brown")+
  labs(x="",y="Population 2017 [in thousands]")+
  geom_label(aes(label=round(pop,0)),nudge_y = 5)+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits=c(0,260))

# f_savePlot(fig_pop,sprintf(file_save,"Pop2017"))


