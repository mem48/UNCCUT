library(sf)
library(tmap)
library(ggplot2)
library(dplyr)

all = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")
bounds = read_sf("data/LSOA_bounds_simplified.gpkg")
stops = read_sf("data/new_stations.geojson")

bounds = st_transform(bounds, 27700)
stops = st_transform(stops, 27700)

buff = st_buffer(stops, 500)

bounds = st_join(bounds, buff)
bounds = bounds[order(bounds$year),]
bounds = bounds[!duplicated(bounds$geo_code),]
bounds = bounds[bounds$geo_code %in% unique(all$code),]
#qtm(bounds, fill = "year")


# Example E01033707

all_sub = all[all$code == "E01033707",]
all_sub$intervention_year = 2010
all_sub$year_offset = all_sub$year - all_sub$intervention_year

coeff =  max(c(all_sub$population), na.rm = T) / max(all_sub$cars_per_person, na.rm = T)
ggplot(all_sub, aes(x = year_offset)) +
  geom_line(aes(y = cars_per_person), size=2, color="red") +
  geom_line(aes(y=population / coeff) , size=2, color="green") +
  scale_y_continuous(
    name = "Population",
    sec.axis = sec_axis(~.*coeff, name="Population (green)")
  )


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
 