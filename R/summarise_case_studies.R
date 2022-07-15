library(dplyr)
library(sf)
library(tmap)
library(ggplot2)


clasif = readRDS("data/lsoa_carpp_pop_classifications.Rds")

casestudy <- read.csv("data/case_study_locations2.csv")

oac <- read.csv("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")
oac <- oac[,c("LSOA11CD","SOAC11NM")]
oac <- oac[!duplicated(oac$LSOA11CD),]

ru <- read.csv("data/Rural_Urban_Classification.csv")
names(ru) <- c("LSOA11CD", "LSOA11NM","RUC11CD","RUC11","FID")
ru <- ru[,c("LSOA11CD","RUC11")]

casestudy <- left_join(casestudy, ru, by = c("geo_code" = "LSOA11CD" ))
casestudy <- left_join(casestudy, oac, by = c("geo_code" = "LSOA11CD"))
casestudy <- left_join(casestudy, clasif, by = c("geo_code" = "code"))

# Clean up terms
casestudy$class[casestudy$class == "rising population falling cars"] <- "Growing population declining cars"
casestudy$class[casestudy$class == "rising population slowly rising cars"] <- "Growing population slowly growing cars"
casestudy$class[casestudy$class == "rising population stable cars"] <- "Growing population stable cars"
casestudy$class[casestudy$class == "stable population and cars"] <- "Stable population and cars"
casestudy$class[casestudy$class == "stable population falling cars"] <- "Stable population declining cars"

  
Sum_rural <- casestudy %>%
  group_by(class, RUC11) %>%
  tally()

Sum_rural_all <- ru %>%
  group_by(RUC11) %>%
  tally()

Sum_rural_all$class <- "England and Wales"
Sum_rural_all <- rbind(Sum_rural_all, Sum_rural)

ggplot(Sum_rural_all, aes(x =class, y = n, fill = RUC11)) +
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(position = "top") +
  coord_flip() +
  ylab("") +
  xlab("Classification") +
  guides(fill=guide_legend(title="",
                           nrow = 2)) +
  theme(legend.position="top",
        legend.justification = "left") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.text=element_text(size=6))


ggsave("plots/casestudy_rural_urban.png", width = 210, height = 80, units = "mm")

all_long = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")
foo = all_long[all_long$code == "E01033104",]

# summarise(Ru,raltown = n(RUC11 == "Rural town and fringe"),
  #           Ruraltownsparse = n(RUC11 == "Rural town and fringe in a sparse setting"),
  #           Ruralvillage = n(RUC11 == "Rural village and dispersed"), 
  #           Ruralvillagesparse  = n(RUC11 == "Rural village and dispersed in a sparse setting"),
  #           Urban  = n(RUC11 == "Urban city and town"),
  #           Urbansparse  = n(RUC11 == "Urban city and town in a sparse setting"),
  #           majorconurbation = n(RUC11 == "Urban major conurbation"),
  #           minorconurbation = n(RUC11 == "Urban minor conurbation"),)
