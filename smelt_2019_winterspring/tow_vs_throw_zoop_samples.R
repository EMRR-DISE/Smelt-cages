zoop <- read_csv("smelt_2019_winterspring/edi/data_objects/2019_smeltstudy_zoop_data.csv") %>%
  filter(TaxonomicGrouping!="Microzoo")
samples <- zoop %>%
  group_by(Location, Method, Date) %>%
  summarize(n = n()) %>%
  distinct()

taxa<-abundance %>% 
  group_by(Location, Method, Date, Taxa) %>% 
  summarize("ind"= sum(ind_m3), "bio"=sum(bio_m3))

taxa.week.2 <- taxa[taxa$Taxa != "Microzoo",]

taxa.week.2$SiteName = factor(taxa.week.2$Location, levels=c('RV','DWSC'))

taxa.week.2$Taxa <- as.character(taxa.week.2$Taxa)
taxa.week.2$Taxa[taxa.week.2$Taxa=='Macrozoo'] <- 'Other'

for.mod<-taxa.week.2 %>% 
  group_by(Date, Location, Taxa, Method) %>% 
  summarize("bio.all"=sum(bio))


ggplot(data = for.mod, aes(x=factor(Date), y=bio.all, fill = Taxa, colour = Method)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~Location, scales = "free_x")+
  theme_bw() + 
  scale_colour_manual(values = c("black", "yellow")) + 
 # scale_fill_manual(legend_title, values=cbPalette)+
 # labs(x = "Week") +
 # labs(y = "Biomass (ugC/m3)")+
  theme(legend.position="right")+
  theme(axis.text = element_text(size = 10, angle = 90),
        axis.title = element_text(size =17),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        text=element_text(family = "Times"))
