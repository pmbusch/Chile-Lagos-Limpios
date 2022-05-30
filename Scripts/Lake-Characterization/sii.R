## SII Data: companies and registered workers
## PBH May 2022

# load data
# sii <- read_excel(sprintf(url_file,"SII/202110_PUB_COMU_ACT.xlsx"),
#                   sheet="Datos")
#                   # range="A1013220:AA1013229")
# 
# # filter
# sii$`Region del domicilio o casa matriz` %>% unique
# sii <- sii %>% 
#   filter(`Region del domicilio o casa matriz` %in% 
#            c("Región de Los Ríos","Región de Los Lagos","Región de La Araucanía"))
# 
# # save to avoid loading everytime
# saveRDS(sii,"../../Data/sii_data.rds")

sii <- read_rds("../../Data/sii_data.rds")

# filter by commune
sii <- sii %>% 
  mutate(commune=`Comuna del domicilio o casa matriz` %>% 
           str_replace("ó","o") %>% 
           str_replace("í","i")) %>% 
  filter(commune %in% comunes_cll)

sii <- sii %>% 
  mutate(year=as.numeric(`Año Comercial`),
         empresas=as.numeric(`Número de empresas`),
         trabajadores=as.numeric(`Número de trabajadores dependientes informados`))

# aggreate by year
sii <- sii %>% group_by(year,commune) %>% 
  summarise(empresas=sum(empresas,na.rm=T),
            trabajadores=sum(trabajadores,na.rm=T)) %>% ungroup()

# table per year
sii %>% group_by(year) %>% summarise(empresas=sum(empresas,na.rm=T),
                                    trabajadores=sum(trabajadores,na.rm=T))

# figure: stacked area
p_emp <- sii %>% 
  rename(nombre_comuna=commune) %>% 
  left_join(left_join(map_commune,codigos_territoriales)) %>%
  ggplot(aes(year,empresas,fill=reorder(nombre_comuna,desc(latitude_commune))))+
  geom_area(alpha=.5,size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(x="Year",fill="",y="Number of registered companies",
       caption=expression(paste(italic("Source: SII 2020"))))+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  coord_cartesian(expand = F)

p_workers <- sii %>% 
  rename(nombre_comuna=commune) %>% 
  left_join(left_join(map_commune,codigos_territoriales)) %>%
  ggplot(aes(year,trabajadores,fill=reorder(nombre_comuna,desc(latitude_commune))))+
  geom_area(alpha=.5,size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(x="Year",fill="",y="Number of registered workers",
       caption=expression(paste(italic("Source: SII 2020"))))+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  coord_cartesian(expand = F)


#EoF