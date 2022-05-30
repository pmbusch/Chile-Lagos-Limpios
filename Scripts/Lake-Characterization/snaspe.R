## SNASPE
## Figure for tourism vistis to national parks
## PBH May 2022


# load data
snaspe <- read_excel(sprintf(url_file,"Sernatur/1599687311Totalvisitantesaño2019.xlsx"),
                     sheet="Sheet1",
                     range="A4:L33")



# Data manipulation

# filter >0
snaspe <- snaspe %>% filter(TOTAL>0)

# change names
snaspe <- snaspe %>% 
  rename(`Protected Area`=`UND. SNAPE`) %>% 
  mutate(`Protected Area`=`Protected Area` %>% 
           str_remove_all("P.N.|M.N.|S.N.|R.N.") %>% 
           str_to_title())

# factor
snaspe <- snaspe %>% 
  mutate(REGION=factor(REGION,levels = c("IX","XIV","X")),
         Tipo=factor(Tipo,levels=c("National Park","Natural Reserve","Natural Monument")))


# figure
p_snaspe <- snaspe %>% 
  rownames_to_column() %>% 
  ggplot(aes(reorder(`Protected Area`,rowname),TOTAL))+
  geom_col(aes(fill=Tipo))+
  facet_grid(REGION~.,scales = "free",space="free",switch='y')+
  coord_flip()+
  geom_label(aes(label=format(TOTAL, big.mark = " ", scientific = FALSE)),
             nudge_y = 50000)+
  scale_y_continuous(expand = c(0, 0),limits=c(0,7.8*1e5),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  labs(x="Protected Wild Area",y="Total number of visitors (2019)",
       fill="Type",
       caption=expression(paste(italic(
    "Source: CONAF (2019)"))))+
  theme(legend.position = c(0.75, 0.8)
        ,legend.background = element_rect(fill = "white", colour = NA))

rm(snaspe)

# EoF