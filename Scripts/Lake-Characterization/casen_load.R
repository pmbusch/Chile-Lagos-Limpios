### Chile Lagos Limpios
## CASEN: CASEN - National Socioeconomic Characterization Survey
## PBH Nov 2020

library(casen)
library(spatstat) #weighted median

# Download data
# casen::descargar_casen_github(anios=2017, carpeta = "Data/Data_Original/Casen")
df_casen <- read_rds(sprintf(url_file,"CASEN/2017.rds"))
# df_casen %>% names()

## Unify codigo_comuna: add 0 to regions (01)
df_casen <- df_casen %>% 
  mutate(comuna=paste(if_else(str_length(comuna)==4,"0",""),
                      comuna,sep=""))

# Expansion Factor: commune and region
# df_casen$expc %>% sum()
# df_casen$expr %>% sum()


get_var_CASEN <- function(var_interest){
  # By communes of interest
  df_aux <- df_casen %>% 
    filter(comuna %in% comunes_cll_codes2) %>% 
    group_by(comuna) %>% 
    summarise(var_mean=weighted.mean({{var_interest}},w = expc,na.rm=T),
              var_median=weighted.median({{var_interest}}, w = expc,na.rm=T)) %>% 
    ungroup() %>% 
    left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
    rename(codigo_comuna=comuna)
  
  # National level
  df_aux_nat <- df_casen %>% 
    summarise(var_mean=weighted.mean({{var_interest}},w = expc,na.rm=T),
              var_median=weighted.median({{var_interest}}, w = expc,na.rm=T)) %>% 
    mutate(codigo_comuna="00",nombre_comuna="National level",
           codigo_provincia="00",nombre_provincia="National level",
           codigo_region="00",nombre_region="National level")
  
  return(rbind(df_aux_nat,df_aux))
  # return(df_aux_nat)
}

# Function to get avg value for CLL ZOne
get_var_CASEN_CLL <- function(var_interest,CLL=T){
  # By communes of interest if CLL=T
  if (CLL==T){
    df_aux <- df_casen %>% 
      filter(comuna %in% comunes_cll_codes2)
  } else {
    df_aux <- df_casen
  }
  
  df_aux <- df_aux %>% 
    summarise(var_mean=weighted.mean({{var_interest}},w = expc,na.rm=T),
              var_median=weighted.median({{var_interest}}, w = expc,na.rm=T)) 
  return(df_aux)
}



# df_casen %>%
#   summarise(var_mean=weighted.mean(ytotcor,w = expc,na.rm=T),
#             var_median=weighted.median(ytotcor, w = expc,na.rm=T))



# df_casen %>% 
#   group_by(region) %>% 
#   summarise(var_mean=weighted.mean(ytotcor,w = expc,na.rm=T),
#             var_median=weighted.median(ytotcor, w = expc,na.rm=T))

# Function

# Income ----------------
# ytotcor: monthly income per capita (corrected). (Ingreso total corregido)

df_income <- get_var_CASEN(ytotcor) %>% 
  rename(income_median=var_median,income_mean=var_mean)

# plot
# usd to clp
usd_clp <- 800

fig_income_med <- df_income %>% 
  left_join(map_commune) %>% 
  mutate(latitude_commune=if_else(is.na(latitude_commune),
                                  -30,latitude_commune)) %>%   # to sort
  mutate(income_median_usd=income_median*12/usd_clp) %>% 
  ggplot(aes(reorder(nombre_comuna,latitude_commune),income_median_usd,
             fill=nombre_region))+
  geom_col()+
  coord_flip()+
  labs(x="",y="Median annual income (USD)",fill="Region",
       caption="Source: CASEN Socioeconomic Survey 2017. \n A value of 800 clp per USD was used.")

# f_savePlot(fig_income,"Figures/median_income.png")

fig_income <- df_income %>% 
  left_join(map_commune) %>% 
  mutate(latitude_commune=if_else(is.na(latitude_commune),
                                  -30,latitude_commune)) %>%   # to sort
  mutate(income_mean_usd=income_mean*12/usd_clp) %>% 
  ggplot(aes(reorder(nombre_comuna,latitude_commune),income_mean_usd,
             fill=nombre_region))+
  geom_col()+
  coord_flip()+
  labs(x="",y="Mean annual income (USD)",fill="Region",
       caption="Source: CASEN Socioeconomic Survey 2017. \n A value of 800 clp per USD was used.")
# f_savePlot(fig_income,"Figures/mean_income.png")



## EDUCATION ------------
# e6a: Cuál fue el nivel educacional más alto alcanzado o el nivel educacional actual
# e6a: , highest education leve
df_codigoEducacion <- read_excel(sprintf(url_file,"CASEN/Codigos_CASEN.xlsx"), 
                                 sheet = "e6a")
df_education <- df_casen %>% 
  filter(comuna %in% comunes_cll_codes2) %>% 
  group_by(comuna,e6a) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoEducacion, by=c("e6a"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

# df_education$e6a %>% unique()
df_education <- df_education %>% 
  filter(e6a!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(menor_media=if_else(e6a<8,1,0)) %>% 
  group_by(nombre_region,codigo_comuna,nombre_comuna,menor_media) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(menor_media==1) %>% 
  select(nombre_region,codigo_comuna,nombre_comuna,perc) %>% 
  rename(perc_less_highschool=perc)

# Same but with national level
df_education_nat <- df_casen %>% 
  group_by(e6a) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoEducacion, by=c("e6a"="codigo")) %>% 
  mutate(codigo_comuna="00",nombre_comuna="National level",
         codigo_provincia="00",nombre_provincia="National level",
         codigo_region="00",nombre_region="National level")

df_education_nat <- df_education_nat %>% 
  filter(e6a!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(menor_media=if_else(e6a<8,1,0)) %>% 
  group_by(nombre_region,codigo_comuna,nombre_comuna,menor_media) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(menor_media==1) %>% 
  select(nombre_region,codigo_comuna,nombre_comuna,perc) %>% 
  rename(perc_less_highschool=perc)

df_education <- rbind(df_education_nat,df_education)

# avg for all CLL communes (14)
df_education_cll <- df_casen %>% 
  filter(comuna %in% comunes_cll_codes2) %>% 
  group_by(e6a) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoEducacion, by=c("e6a"="codigo")) %>% 
  mutate(codigo_comuna="00",nombre_comuna="National level",
         codigo_provincia="00",nombre_provincia="National level",
         codigo_region="00",nombre_region="National level")

df_education_cll <- df_education_cll %>% 
  filter(e6a!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(menor_media=if_else(e6a<8,1,0)) %>% 
  group_by(nombre_region,codigo_comuna,nombre_comuna,menor_media) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(menor_media==1) %>% 
  select(nombre_region,codigo_comuna,nombre_comuna,perc) %>% 
  rename(perc_less_highschool=perc)


# plot
fig_education <- df_education %>% 
  left_join(map_commune) %>% 
  mutate(latitude_commune=if_else(is.na(latitude_commune),
                                  -30,latitude_commune)) %>%   # to sort
  ggplot(aes(reorder(nombre_comuna,latitude_commune),perc_less_highschool,
             fill=nombre_region))+
  geom_col()+
  coord_flip()+
    labs(x="",y="% of population with less than high school education",fill="Region",
       caption="Source: CASEN Socioeconomic Survey 2017.")

# f_savePlot(fig_education,"Figures/high_school.png")


## HEALTH CARE PROVIDER -------
# s12: A qué sistema previsional de salud pertenece usted
# s12: affiliation to health care provider (private or state level
df_codigoSalud <- read_excel(sprintf(url_file,"CASEN/Codigos_CASEN.xlsx"), 
                             sheet = "s12")
df_healthProvider <- df_casen %>% 
  filter(comuna %in% comunes_cll_codes2) %>% 
  group_by(comuna,s12) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoSalud, by=c("s12"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

df_healthProvider <-  df_healthProvider %>% 
  # filter(s12!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(prev=case_when(
    s12==1 ~ "Fonasa A",
    s12==2 ~ "Fonasa B",
    s12==3 ~ "Fonasa C",
    s12==4 ~ "Fonasa D",
    s12==6 ~ "FF.AA.",
    s12==7 ~ "Isapre (private)",
    T~"Unknown")) %>% 
  group_by(nombre_region,codigo_comuna,nombre_comuna,prev) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% select(-hab) %>% 
  spread(prev, perc, fill=0)

# same with national
df_healthProvider_nat <- df_casen %>% 
  group_by(s12) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoSalud, by=c("s12"="codigo")) %>% 
  mutate(codigo_comuna="00",nombre_comuna="National level",
         codigo_provincia="00",nombre_provincia="National level",
         codigo_region="00",nombre_region="National level")
  
df_healthProvider_nat <-  df_healthProvider_nat %>% 
  # filter(s12!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(prev=case_when(
    s12==1 ~ "Fonasa A",
    s12==2 ~ "Fonasa B",
    s12==3 ~ "Fonasa C",
    s12==4 ~ "Fonasa D",
    s12==6 ~ "FF.AA.",
    s12==7 ~ "Isapre (private)",
    T~"Unknown")) %>% 
  group_by(nombre_region,codigo_comuna,nombre_comuna,prev) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% select(-hab) %>% 
  spread(prev, perc, fill=0)


df_healthProvider <- rbind(df_healthProvider_nat,df_healthProvider)


df_healthProvider_plot <- df_healthProvider %>% 
  gather("key","value",-nombre_region,-codigo_comuna,-nombre_comuna)

# plot
fig_health <- df_healthProvider_plot %>% 
  left_join(map_commune) %>% 
  mutate(latitude_commune=if_else(is.na(latitude_commune),
                                  -30,latitude_commune)) %>%   # to sort
  ggplot(aes(reorder(nombre_comuna,latitude_commune),value))+
  geom_col(aes(fill=forcats::fct_rev(key)))+
  coord_flip()+
  labs(x="",y="% of population affiliated to each health provider",
       fill="Health Provider",
       caption="Source: CASEN Socioeconomic Survey 2017. \n Note: Fonasa A indicates a lower income than Fonasa D.")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.position="bottom")

# f_savePlot(fig_health,"Figures/healthprovider.png")

## EoF