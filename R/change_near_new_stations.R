library(sf)
library(tmap)
library(ggplot2)
library(dplyr)

all = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")
classif = readRDS("data/lsoa_carpp_pop_classifications.Rds")

bounds = read_sf("data/LSOA_bounds_simplified.gpkg")
stops = read_sf("data/new_stations.geojson")
stops$cluster[is.na(stops$cluster)] <- "Other"
stops$cluster_year <- paste0(stops$cluster," - ",stops$year)

bounds = st_transform(bounds, 27700)
stops = st_transform(stops, 27700)

buff = st_buffer(stops, 500)

bounds = st_join(bounds, buff)
bounds = bounds[order(bounds$year),]
bounds = bounds[!duplicated(bounds$geo_code),]
bounds = bounds[bounds$geo_code %in% unique(all$code),]
#qtm(bounds[!is.na(bounds$year),], fill = "year") + qtm(stops)

stop_join <- st_drop_geometry(bounds)
names(stop_join) <- c("code","stop","year_stop","cluster","cluster_year")
all <- left_join(all, stop_join, by = "code")
all$cluster_year[is.na(all$cluster_year)] <- "None"
all$cluster[is.na(all$cluster)] <- "None"
all$year_offset = all$year - all$year_stop


all_summary_stop <- all %>%
  filter(cars_per_person < 2) %>%
  filter(cluster != "Other") %>%
  filter(cluster != "None") %>%
  group_by(cluster_year, year_offset) %>%
  summarise(median = median(cars_per_person, na.rm = TRUE),
            mean = mean(cars_per_person, na.rm = TRUE),
            q5 = quantile(cars_per_person, probs = 0.05, na.rm = TRUE),
            q95 = quantile(cars_per_person, probs = 0.95, na.rm = TRUE),
            q25 = quantile(cars_per_person, probs = 0.25, na.rm = TRUE),
            q75 = quantile(cars_per_person, probs = 0.75, na.rm = TRUE),
            min = min(cars_per_person, na.rm = TRUE),
            max = max(cars_per_person, na.rm = TRUE),
            sd = sd(cars_per_person, na.rm = TRUE))

#all_summary_stop$year_stop <- as.character(all_summary_stop$year_stop)

cols = c("DLR Woolwich Extension - 2005" = "#cb181d",
         "DLR Woolwich Extension - 2009" = "#fb6a4a",
         "DLR Stratford Extension - 2011" = "#fcae91",
         "East London Line - 2010" = "#ff7f00",
         "Ebbe Vale Line - 2008" = "#238b45",
         "Ebbe Vale Line - 2014" = "#74c476",
         "Ebbe Vale Line - 2015" = "#bae4b3",
         "Manchester Tram South - 2011" = "#7a0177",
         "Manchester Tram Didsbury Line - 2013" = "#c51b8a",
         "Manchester Tram East Line - 2013" = "#f768a1",
         "Manchester Tram Airport Line - 2014" = "#fbb4b9",
         "Manchester Tram Rochdale Line - 2012" = "#ce1256",
         "Manchester Tram Rochdale Line - 2013" = "#df65b0",
         "Manchester Tram Rochdale Line - 2014" = "#d7b5d8",
         "Nottingham Tram North - 2004" = "#2b8cbe",
         "Nottingham Tram South - 2015" = "#a6bddb")

all_summary_stop$cluster_year <- factor(all_summary_stop$cluster_year, levels = names(cols))



ggplot(all_summary_stop, aes(x = year_offset, y = median, color = cluster_year)) +
  geom_vline(xintercept = 0) +
  geom_line(size=1) +
  scale_color_manual(values=cols) +
  geom_smooth(method=lm, se=FALSE, linetype="dotted") +
  ylab("Median cars per person") +
  xlab("Year's since station(s) opended") +
  guides(color=guide_legend(title="Project - year opened", ncol =1))
  
ggsave("plots/change_stations.png", width = 210, height = 297, units = "mm")


all_summary_stop2 <- all %>%
  filter(cars_per_person < 2) %>%
  filter(cluster == "Other") %>%
  group_by(stop, year_offset) %>%
  summarise(median = median(cars_per_person, na.rm = TRUE),
            mean = mean(cars_per_person, na.rm = TRUE),
            q5 = quantile(cars_per_person, probs = 0.05, na.rm = TRUE),
            q95 = quantile(cars_per_person, probs = 0.95, na.rm = TRUE),
            q25 = quantile(cars_per_person, probs = 0.25, na.rm = TRUE),
            q75 = quantile(cars_per_person, probs = 0.75, na.rm = TRUE),
            min = min(cars_per_person, na.rm = TRUE),
            max = max(cars_per_person, na.rm = TRUE),
            sd = sd(cars_per_person, na.rm = TRUE))

ggplot(all_summary_stop2, aes(x = year_offset, y = median, color = stop)) +
  geom_vline(xintercept = 0) +
  geom_line(size=1) +
  geom_smooth(method=lm, se=FALSE, linetype="dotted") +
  ylab("Median cars per person") +
  xlab("Year's since station opended") +
  guides(color=guide_legend(title="Station", ncol =1))

ggsave("plots/change_stations_single.png", width = 210, height = 297, units = "mm")


foo = all[all$cluster == "Nottingham Tram North",]
ggplot(foo, aes(x = year, y = cars_per_person, color = code)) +
  geom_line(size=1) +
  ylab("Median cars per person") +
  xlab("Year's since station opended") +
  ylim(0,0.8)
  guides(color=guide_legend(title="Station", ncol =1))


# class_summary <- classif %>% 
#   group_by(stop) %>%
#   summarise(carpp_change_max = max(carpp_max - carpp_min, na.rm = TRUE),
#             carpp_change_min = min(carpp_max - carpp_min, na.rm = TRUE),
#             carpp_change_sd = sd(carpp_max - carpp_min, na.rm = TRUE),
#             carpp_change_mean = mean(carpp_max - carpp_min, na.rm = TRUE),
#             carpp_change_median = median(carpp_max - carpp_min, na.rm = TRUE),
#             carpp_percent_max = max((carpp_max - carpp_min) / carpp_max, na.rm = TRUE),
#             carpp_percent_min = min((carpp_max - carpp_min) / carpp_max, na.rm = TRUE),
#             carpp_percent_sd = sd((carpp_max - carpp_min) / carpp_max, na.rm = TRUE),
#             carpp_percent_mean = mean((carpp_max - carpp_min) / carpp_max, na.rm = TRUE),
#             carpp_percent_median = median((carpp_max - carpp_min) / carpp_max, na.rm = TRUE))
# 
# 
# ggplot(class_summary, aes(x = year, y = median, color = SOAC11NM)) +
#   geom_line(size=2) +
#   ylab("Median cars per person") +
#   xlab("Year") +
#   guides(color=guide_legend(title="Area classification", ncol =1)) +
#   scale_color_manual(values=cols)


# Example Holt Town

stop = "Shudehill Interchange"
all_sub = all[all$code %in% c(bounds$geo_code[bounds$stop == stop]),]
all_sub$intervention_year = stops$year[stops$stop == stop]
all_sub$year_offset = all_sub$year - all_sub$intervention_year

# coeff =  max(c(all_sub$population), na.rm = T) / max(all_sub$cars_per_person, na.rm = T)
# ggplot(all_sub, aes(x = year_offset)) +
#   geom_line(aes(y = cars_per_person), size=2, color="red") +
#   geom_line(aes(y=population / coeff) , size=2, color="green") +
#   geom_line(aes(y=AllCars / coeff) , size=2, color="blue") +
#   scale_y_continuous(
#     name = "Population",
#     sec.axis = sec_axis(~.*coeff, name="Population (green)")
#   )
all_sub_long = tidyr::pivot_longer(all_sub[,c("code","year_offset","population","AllCars","cars_per_person")],
                                   cols = c("population","AllCars","cars_per_person"))

ggplot(all_sub_long, aes(year_offset, value, colour = code)) + geom_line() +
  facet_wrap(~ name, ncol = 1, scales = "free_y")




# Do for all data
  
all = left_join(all, st_drop_geometry(bounds), by = c("code" = "geo_code"))
names(all) = c("code","year","population","AllCars","cars_per_person","stop","intervention_year")

all_near = all[!is.na(all$intervention_year),]
all_near$year_offset = all_near$year - all_near$intervention_year

all_near_gp = all_near %>%
  group_by(stop, year_offset) %>%
  summarise(cars_per_person = mean(cars_per_person))

ggplot(all_near_gp, aes(x = year_offset, group = stop)) +
  geom_line(aes(y = cars_per_person), size=1, color="black") +
  ylim(0,1)

 foo = all_near_gp %>%
   group_by(stop) %>%
   summarise(max = max(cars_per_person),
             min = min(cars_per_person))

 foo$diff = foo$max - foo$min

 #Lace Market tram stop vs  Eskdale Drive tram stop
 ggplot(all_near_gp[all_near_gp$stop %in% foo$stop[foo$diff > 0.1],], 
        aes(x = year_offset, group = stop, color = stop)) +
   geom_line(aes(y = cars_per_person), size=1) +
   ylim(0,1)
 