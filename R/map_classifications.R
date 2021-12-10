#

bounds <- read_sf("data/LSOA_bounds_simplified.gpkg")

dat <- readRDS("data/lsoa_carpp_pop_classifications.Rds")



dat_plot <- left_join(bounds, dat, by = c("geo_code" = "code"))
dat_plot <- dat_plot[,c("geo_code","desc_pop","desc_cars","bna_cars_class")]
dat_plot <- dat_plot[substr(dat_plot$geo_code,1,1) != "S",]

write_sf(dat,"data/lsoa_carpp_pop_classifications.gpkg")

dat_falling = dat_plot[dat_plot$desc_cars %in% c("Continuous decline","Falling which has stopped","Falling with delayed start"),]

write_sf(dat_falling,"data/lsoa_carpp_pop_classifications_falling.gpkg")

table(dat$combined_class_cars[dat$desc_cars == "other"])

library(tmap)
tmap_mode("view")

qtm(dat_falling, fill = "desc_cars")
qtm(dat_falling, fill = "desc_pop")


# Investigat the delayed start falling areas
all = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")
fds = dat$code[dat$desc_cars == "Falling with delayed start" & dat$bna_cars_class == ""]

fds <- all[all$code %in% fds,]

foo <- fds[1:170,]
foo <- foo[!foo$code  %in% c("E01016281"),]

ggplot(foo, aes(year, cars_per_person, color = code)) +
  geom_line(lwd = 2)
