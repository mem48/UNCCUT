# Load in area classifications
library(dplyr)
library(ggplot2)

oac <- read.csv("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")

oac <- oac[,c("LSOA11CD","SOAC11NM")]
oac <- oac[!duplicated(oac$LSOA11CD),]

ru <- read.csv("data/Rural_Urban_Classification.csv")
names(ru) <- c("LSOA11CD", "LSOA11NM","RUC11CD","RUC11","FID")
ru <- ru[,c("LSOA11CD","RUC11")]

table(substr(ru$LSOA11CD,1,1))

ru <- left_join(ru, oac, by = "LSOA11CD")

clasif = readRDS("data/lsoa_carpp_pop_classifications.Rds")
clasif = clasif[,c("code","gradient_cars","pop_change_class","cars_change_class","desc_pop","desc_cars")]

ru <- left_join(ru, clasif, by = c("LSOA11CD" = "code"))

casestudy <- read.csv("data/case_study_locations2.csv")

all <- left_join(ru, casestudy, by = c("LSOA11CD" = "geo_code"))

all$RUC11 <- factor(all$RUC11, levels = c("Urban major conurbation",
                                          "Urban minor conurbation",
                                          "Urban city and town",
                                          "Urban city and town in a sparse setting",
                                          "Rural town and fringe",
                                          "Rural town and fringe in a sparse setting",
                                          "Rural village and dispersed",
                                          "Rural village and dispersed in a sparse setting"))

all$class <- factor(all$class, levels = c(NA,
                                          "rising population falling cars",
                                          "stable population falling cars",
                                          "stable population and cars",
                                          "rising population stable cars",
                                          "rising population slowly rising cars"))

all$SOAC11NM <- factor(all$SOAC11NM, levels = c("Cosmopolitan student neighbourhoods",
                                                "Ageing rural neighbourhoods",
                                                "Prospering countryside life",
                                                "Remoter communities",
                                                "Rural traits",
                                                "Achieving neighbourhoods",
                                                "Asian traits",
                                                "Highly qualified professionals",
                                                "Households in terraces and flats",
                                                "Challenged white communities",
                                                "Constrained renters",
                                                "Hampered neighbourhoods",
                                                "Hard-pressed flat dwellers",
                                                "Ageing urban communities",
                                                "Aspiring urban households",
                                                "Comfortable neighbourhoods",
                                                "Endeavouring social renters",
                                                "Primary sector workers",
                                                "Inner city cosmopolitan",
                                                "Urban cultural mix",
                                                "Young ethnic communities",
                                                "Affluent communities",
                                                "Ageing suburbanites",
                                                "Comfortable suburbia"))


summ <- all %>%
  group_by(RUC11, SOAC11NM, class, pop_change_class, cars_change_class, desc_pop, desc_cars) %>%
  summarise(count = n())

table(all$desc_cars)


# Plot stacked Bar Charts

ggplot(summ, aes(fill=RUC11, y=count, x=class)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_brewer(palette="RdYlGn") +
  coord_flip() +
  theme(legend.position="bottom")


ggplot(summ, aes(fill=SOAC11NM, y=count, x=class)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=c('#955123','#007f42','#3ea456','#8aca8e',
                             '#cfe8d1','#00498d','#2967ad','#7b99c7',
                             '#b9c8e1','#e3ac20','#edca1a','#f6e896',
                             '#fcf5d8','#e64c2b','#ec773c','#faa460',
                             '#fcc9a0','#fee4ce','#f79ff0','#6a339a',
                             '#9f84bd','#576362','#a1a2a1','#e5e4e3')) +
  coord_flip() +
  theme(legend.position="bottom")

ggplot(summ, aes(fill=RUC11, y=count, x=desc_cars)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_brewer(palette="RdYlGn") +
  coord_flip() +
  theme(legend.position="bottom")


ggplot(all, aes(color=class, y=desc_pop, x=gradient_cars)) + 
  geom_point() +
  theme(legend.position="bottom")


