## Population Analysis on the Zone
## Population Trend to 2035
## PBH Oct. 2021

# Common functions and parameters
source("Scripts/00-Common.R", encoding = "UTF-8")
file_save <- "Figures/Population/%s.png"

# LOAD DATA ---------
## Trend Population to 2035 by commune ----
pop_proj <- read_csv2(
  sprintf(url_file,"Proyeccion Poblacion/estimaciones-y-proyecciones-2002-2035-comuna-y-area-urbana-y-rural.csv"))
names(pop_proj)


# DATA WRANGLING ------------
## Trend Population to 2035 by commune ---- 
pop_commune_proj <- pop_proj %>% 
  filter(Comuna %in% comunes_cll_codes) %>% 
  mutate(codigo_comuna=if_else(str_length(Comuna)==4,
                               paste0("0",Comuna),
                               paste0("",Comuna)))

pop_commune_proj$codigo_comuna %>% unique()
names(pop_commune) <- names(pop_commune_proj) %>% f_remover_acentos() %>% 
  str_replace_all(" ","_")

# Data wrangling
pop_commune_wide <- pop_commune_proj %>% 
  pivot_longer((10:43),names_to ="year",values_to="population",) %>% 
  mutate(year=str_remove_all(year,"Poblacion_"),
         Nombre_Comuna=f_remover_acentos(Nombre_Comuna))

pop_commune_cll_proj <- pop_commune_wide %>% 
  group_by(Nombre_Comuna,codigo_comuna,year) %>% 
  summarise(pop=sum(population,na.rm=T))

pop_commune_cll_proj$Nombre_Comuna %>% unique()

# FIGURES ------------

## Stacked area -----
pop_commune_cll_proj %>% 
  mutate(pop=pop/1000,
         year=as.numeric(year)) %>% 
  left_join(map_commune) %>%
  ggplot(aes(year,pop,fill=reorder(Nombre_Comuna,desc(latitude_commune))))+
  geom_area(alpha=.5,size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(x="",fill="",y="Population [in thousands]")+
  coord_cartesian(expand = F)

f_savePlot(last_plot(),
           sprintf(file_save,"PopTrend"))

## How much each population will grow 2035-2017? -----
pop_comune_cll_plot <- pop_commune_cll_proj %>% 
  mutate(pop=pop/1000,
         year=as.numeric(year)) %>% 
  filter(year %in% c("2017","2035")) %>% 
  pivot_wider(names_from = year, values_from = pop) %>% 
  mutate(dif=`2035`-`2017`)

ggplot(pop_comune_cll_plot,
       aes(reorder(Nombre_Comuna,dif),dif))+
  geom_col(fill="brown")+
  labs(x="",y="Population difference 2035-2017 [in thousands]")+
  geom_label(aes(label=round(dif,0)),
             nudge_y = 2*sign(pop_comune_cll_plot$dif))+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits=c(-10,50))

f_savePlot(last_plot(),
           sprintf(file_save,"PopDiff2017_2035"))

## Population growth by type of area
pop_comune_cll_plot <- pop_commune_wide %>% 
  group_by(Nombre_Comuna,codigo_comuna,year,`Area_(1=Urbano_2=Rural)`) %>% 
  summarise(pop=sum(population,na.rm=T)) %>% 
  mutate(pop=pop/1000,
         year=as.numeric(year),
         type=if_else(`Area_(1=Urbano_2=Rural)`==1,"Urbano","Rural")) %>% 
  filter(year %in% c("2017","2035")) %>% 
  pivot_wider(names_from = year, values_from = pop) %>% 
  mutate(dif=`2035`-`2017`)

ggplot(pop_comune_cll_plot,
       aes(reorder(Nombre_Comuna,dif),dif,fill=type))+
  geom_col(position="dodge")+
  labs(x="",y="Population difference 2035-2017 [in thousands]",fill="")+
  geom_label(aes(label=round(dif,0)),
             nudge_y = 2*sign(pop_comune_cll_plot$dif))+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits=c(-10,50))

f_savePlot(last_plot(),
           sprintf(file_save,"PopDiff2017_2035_Urban"))
