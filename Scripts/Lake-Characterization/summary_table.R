## Summary Table
# Create summary table for policy brief
## PBH April 2022

source("Scripts/00-Common.R", encoding = "UTF-8")

# Population ------
source("Scripts/Lake-Characterization/Population.R",encoding = "UTF-8")

# total pop
pop <- c(sum(pop_commune_cll$pop), # Lakes
         sum(pop_commune$pop)) # Chile

pop_region <- pop_district %>% 
  filter(REGION %in% c(9,10,14)) %>% 
  pull(PERSONAS) %>% sum()
  
  
## On Shore
cities_lakes <- read_excel(sprintf(url_file,"Cities-Lakes.xlsx"), 
                           sheet = "Cities-Lakes", range = "A1:C13")
pop_shore <- c(sum(cities_lakes$`Pop District Census 2017`),0)


# Demographics -----

pop_district_cll <- pop_district %>% filter(COMUNA %in% comunes_cll_codes)

## Urban share
pop_urban <- c(
  pop_district_cll %>% filter(AREA==1) %>% pull(PERSONAS) %>% sum(),  # lakes
  pop_district %>% filter(AREA==1) %>% pull(PERSONAS) %>% sum() # Chile
)
# to %
pop_urban <- pop_urban/pop*100

## Indigineous people 
pop_indigineous <- c(
  sum(as.numeric(pop_district_cll$PUEBLO),na.rm=T), # Lakes
  sum(as.numeric(pop_district$PUEBLO),na.rm=T) # Chile
)
pop_indigineous <- pop_indigineous/pop*100

# Clean
rm(pop_commune,pop_commune_cll,fig_pop,cod_commune,
   pop_district,pop_district_cll, cities_lakes)

## pop trend
source("Scripts/Lake-Characterization/LoadData-Population.R", 
       encoding = "UTF-8")

# checks
pop_proj$`Poblacion 2017` %>% sum
pop_commune_cll %>% filter(year=="2017") %>% pull(pop) %>% sum()

# expected to 2035
pop_2035 <- c(
  pop_commune_cll %>% filter(year=="2035") %>% pull(pop) %>% sum(),
  pop_proj$`Poblacion 2035` %>% sum()
)

# see diffference with annual componduing rate
t <- 2035-2017

pop_crec <- ((pop_2035/pop)^(1/t)-1)*100
rm(pop_proj,pop_commune,pop_commune_cll,pop_commune_wide,t,pop_district)


# SOCIOECONOMIC CASEN ------
source("Scripts/Lake-Characterization/casen_load.R",
       encoding = "UTF-8")

## median income
median_income <- c(
  get_var_CASEN_CLL(ytotcor,CLL=T) %>% pull(var_median), # Lakes
  get_var_CASEN_CLL(ytotcor,CLL=F) %>% pull(var_median) # Chile
)

median_income <- median_income*12/800 # 800 USD value assumed

# High school education
education <- c(
  df_education_cll$perc_less_highschool, # lakes
  df_education_nat$perc_less_highschool # chile
)

# GDP -----

## See excel PIBR_Act_edited.xls for values
# 2019 - Miles de millones de pesos encadenados
gdp <- c(
  10733, # Lakes
  154660 # Chile
)

# convert to USD
gdp <- gdp/800*1e3*1e6
# per capita
gdp <- gdp/c(pop_region,pop[2])

# SUMMARY TABLE ---------

# define dataframe to store data
table_metrics <- tibble()
# Construct table
table_metrics <- rbind(table_metrics,
                       cbind("Population","Census 2017",
                             "Only 14 communes in Lakes district considered",
                             t(pop)),
                       cbind("Population on-shore","Census 2017",
                             "Population in urban areas in 10km range from lake shore",
                             t(pop_shore)),
                       cbind("% Urban","Census 2017",
                             "Only 14 communes in Lakes district considered",
                             t(pop_urban)),
                       cbind("% Indigenous origin","Census 2017",
                             "Only 14 communes in Lakes district considered",
                             t(pop_indigineous)),
                       cbind("Avg. annual growth rate to 2035 (%)",
                             "INE Porjections","Calculated with an annual componduing rate assumption",
                             t(pop_crec)),
                       cbind("GDP per capita (USD)","Banco Central 2019",
                             "USD value of 800 used. GDP for 3 regions considered",
                             t(gdp)),
                       cbind("Median Annual Income (USD)","CASEN 2017",
                             "USD value of 800 used. Communal expansion factors used. Only 14 communes in Lakes district considered",
                             t(median_income)),
                       cbind("% People with less than high school education",
                             "Casen 2017"," Communal expansion factors used. Only 14 communes in Lakes district considered",
                             t(education))
                      )
names(table_metrics) <- c("Metric","Source","Notes","Lakes Zone","Chile")

table_metrics$`Lakes Zone` <- as.numeric(table_metrics$`Lakes Zone`)
table_metrics$Chile <- as.numeric(table_metrics$Chile)

table_metrics %>% select(-Source,-Notes) %>% 
  flextable() %>% autofit() %>% 
  colformat_double(i=c(1:2,6:7),j=2:3,digits = 0,big.mark = " ") %>% 
  colformat_double(i=c(3:5,8),j=2:3,digits=2)




## EoF