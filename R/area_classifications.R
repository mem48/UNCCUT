# Load in area classifications
library(dplyr)
library(ggplot2)

all_long = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")
high_cpp = all_long[all_long$cars_per_person > 2,]
high_cpp = unique(high_cpp$code)

oac <- read.csv("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")

oac <- oac[,c("LSOA11CD","SOAC11NM")]
oac <- oac[!duplicated(oac$LSOA11CD),]

ru <- read.csv("data/Rural_Urban_Classification.csv")
names(ru) <- c("LSOA11CD", "LSOA11NM","RUC11CD","RUC11","FID")
ru <- ru[,c("LSOA11CD","RUC11")]

table(substr(ru$LSOA11CD,1,1))

ru <- left_join(ru, oac, by = "LSOA11CD")

all <- left_join(all_long, ru, by = c("code" = "LSOA11CD"))

all$RUC11 <- factor(all$RUC11, levels = c("Rural village and dispersed",
                                          "Rural village and dispersed in a sparse setting",
                                          "Rural town and fringe",
                                          "Rural town and fringe in a sparse setting",
                                          "Urban city and town in a sparse setting",
                                          "Urban city and town",
                                          "Urban minor conurbation",
                                          "Urban major conurbation"))



all_summary_ru <- all %>%
  filter(cars_per_person < 2) %>%
  group_by(RUC11, year) %>%
  summarise(median = median(cars_per_person, na.rm = TRUE),
            mean = mean(cars_per_person, na.rm = TRUE),
            q5 = quantile(cars_per_person, probs = 0.05, na.rm = TRUE),
            q95 = quantile(cars_per_person, probs = 0.95, na.rm = TRUE),
            q25 = quantile(cars_per_person, probs = 0.25, na.rm = TRUE),
            q75 = quantile(cars_per_person, probs = 0.75, na.rm = TRUE),
            min = min(cars_per_person, na.rm = TRUE),
            max = max(cars_per_person, na.rm = TRUE),
            sd = sd(cars_per_person, na.rm = TRUE))

ggplot(all_summary_ru, aes(x = year, y = median, color = RUC11)) +
  geom_line(size=2) +
  ylab("Median cars per person") +
  xlab("Year") +
  guides(color=guide_legend(title="Rural/urban classification"))

ggsave("plots/rural_urban.png", width = 210, height = 297, units = "mm")


all_summary_oac <- all %>%
  filter(cars_per_person < 2) %>%
  group_by(SOAC11NM, year) %>%
  summarise(median = median(cars_per_person, na.rm = TRUE),
            mean = mean(cars_per_person, na.rm = TRUE),
            q5 = quantile(cars_per_person, probs = 0.05, na.rm = TRUE),
            q95 = quantile(cars_per_person, probs = 0.95, na.rm = TRUE),
            q25 = quantile(cars_per_person, probs = 0.25, na.rm = TRUE),
            q75 = quantile(cars_per_person, probs = 0.75, na.rm = TRUE),
            min = min(cars_per_person, na.rm = TRUE),
            max = max(cars_per_person, na.rm = TRUE),
            sd = sd(cars_per_person, na.rm = TRUE))


cols = c("Cosmopolitan student neighbourhoods" ='#955123',
         "Ageing rural neighbourhoods" ='#007f42',
         "Prospering countryside life" ='#3ea456',
         "Remoter communities" ='#8aca8e',
         "Rural traits" ='#cfe8d1',
         "Achieving neighbourhoods" ='#00498d',
         "Asian traits" ='#2967ad',
         "Highly qualified professionals" ='#7b99c7',
         "Households in terraces and flats" ='#b9c8e1',
         "Challenged white communities" ='#e3ac20',
         "Constrained renters" ='#edca1a',
         "Hampered neighbourhoods" ='#f6e896',
         "Hard-pressed flat dwellers" ='#fcf5d8',
         "Ageing urban communities" ='#e64c2b',
         "Aspiring urban households" ='#ec773c',
         "Comfortable neighbourhoods" ='#faa460',
         "Endeavouring social renters" ='#fcc9a0',
         "Primary sector workers" ='#fee4ce',
         "Inner city cosmopolitan" = '#f79ff0',
         "Urban cultural mix" ='#6a339a',
         "Young ethnic communities" ='#9f84bd',
         "Affluent communities" ='#576362',
         "Ageing suburbanites" ='#a1a2a1',
         "Comfortable suburbia" ='#e5e4e3')

ggplot(all_summary_oac, aes(x = year, y = median, color = SOAC11NM)) +
  geom_line(size=2) +
  ylab("Median cars per person") +
  xlab("Year") +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols)

ggsave("plots/OAC_class.png", width = 210, height = 297, units = "mm")



  # geom_errorbar(aes(ymin=q25, ymax=q75), width=.2, # error bars are too wide
  #               position=position_dodge(.9))

# clasif = readRDS("data/lsoa_carpp_pop_classifications.Rds")
# clasif = clasif[,c("code","gradient_cars","pop_change_class","cars_change_class","desc_pop","desc_cars")]
# 
# ru <- left_join(ru, clasif, by = c("LSOA11CD" = "code"))
# 
# casestudy <- read.csv("data/case_study_locations2.csv")
# 
# all <- left_join(ru, casestudy, by = c("LSOA11CD" = "geo_code"))



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


