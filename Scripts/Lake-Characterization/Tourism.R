## Tourism Analysis
## Some analysis on Tourism
## PBH Dec. 2021

# source("Scripts/00-Common.R", encoding = "UTF-8")

# PIB (GDP) Data ------------

# Source: Banco Central Chile

# Load data
gdp <- read_excel(sprintf(url_file,
                          "Economic/PIBR_Act.xls"),
                  sheet = "Referencia 2013_nominal",
                  range = "A10:N127",
                  col_names = F)

actividad_levels <- c("Agropecuario Silvicola","Pesca","Mineria",
                      "Industria Manufacturera",
                      "Electricidad, Gas, Agua y Gestion de Desechos",
                      "Construccion","Comercio, Restaurantes y Hoteles",
                      "Transporte, Informacion y Comunicaciones",
                      "Servicios Financieros y Empresariales",
                      "Servicios de Vivienda e Inmobiliarios",
                      "Servicios Personales",
                      "Administracion Publica")
names(gdp) <- c("year","region",actividad_levels)

# Data preparation

# fill years. remove empty rows and change Total name to Nacional
gdp <- gdp %>% 
  fill(year) %>% 
  filter(!is.na(region)) %>% 
  mutate(region=if_else(region=="Total","Nacional",region)) %>% 
  mutate(region=factor(region,
                          levels=c("AyP","TPCA","ANTOF","ATCMA","COQ","VALPO",
                                   "RM","LGBO","MAULE","BBIO","ARAUC","RIOS",
                                   "LAGOS","AYSEN","MAG","Nacional")))

# flat table
gdp <- gdp %>% 
  pivot_longer(-c(year,region), 
               names_to = "Actividad", values_to = "gdp") %>% 
  mutate(Actividad=factor(Actividad,
                          levels=actividad_levels))

# estimate percent
gdp <- gdp %>% 
  group_by(year,region) %>% 
  mutate(gdp_perc=gdp/sum(gdp),
         gdp_perc_cum=cumsum(gdp_perc)) %>% ungroup() %>% 
  mutate(gdp_perc_label=paste0(round(gdp_perc*100,0),"%"))
# check
# gdp %>% group_by(year,region) %>% summarize(suma=sum(gdp_perc))


## Plot for most recent year
fig_gdp_all <- gdp %>% 
  filter(year==2019) %>% 
  ggplot(aes(region,gdp_perc,fill=Actividad))+
  geom_col(position=position_fill(reverse = TRUE))+
  geom_text(data=filter(gdp, year==2019,
                        Actividad=="Comercio, Restaurantes y Hoteles"),
            aes(y=gdp_perc_cum-gdp_perc/2,label=gdp_perc_label),
            )+
  coord_flip(expand = F)+scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7))+
  labs(x="Region",y="% Share of GDP",
       caption = "Banco central 2019. Share for Comercio, Restaurantes y Hoteles is shown")

# Same plot but for regions of interest
fig_gdp <- gdp %>% 
  filter(year==2019) %>% 
  filter(region %in% c("ARAUC","RIOS","LAGOS","Nacional")) %>% 
  ggplot(aes(region,gdp_perc,fill=Actividad))+
  geom_col(position=position_fill(reverse = TRUE))+
  geom_text(data=filter(gdp, year==2019,
                        Actividad=="Comercio, Restaurantes y Hoteles",
                        region %in% c("ARAUC","RIOS","LAGOS","Nacional")),
            aes(y=gdp_perc_cum-gdp_perc/2,label=gdp_perc_label),
  )+
  coord_flip(expand = F)+scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom",
        legend.text = element_text(size=7))+
  labs(x="Region",y="% Share of GDP",
       caption = "Banco central 2019. Share for Comercio, Restaurantes y Hoteles is shown")


## EoF