# Identify Cast Studies
library(ggplot2)
library(sf)
library(tmap)
tmap_mode("view")


clasif = readRDS("data/lsoa_carpp_pop_classifications.Rds")
all = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")
bounds <- read_sf("data/LSOA_bounds_simplified.gpkg")

table(clasif$desc_cars)

sel <- clasif$code[clasif$desc_cars %in% c("Continuous decline","Falling with delayed start","Falling which has stopped")]
sel <- sel[!sel %in% c("E01016281", # Silly high number of cars
                       "E01006178",
                       "E01016479",
                       "E01033500",
                       "E01033333",
                       "E01016696",
                       "E01032739",
                       "E01019579")]

#sel <- sel[1:10]
all_sel = all[all$code %in% sel,]

# Plot cars per person
ggplot(all_sel, aes(year, cars_per_person, color = code)) +
  geom_line(lwd = 2)

# Good Candidates
#sel <- c("E01016473", "E01010614", "E01033755", "E01027682")
all_sel = all[all$code %in% sel,]
ggplot(all_sel, aes(year, cars_per_person, color = code)) +
  geom_line(lwd = 2) +
  scale_x_continuous(breaks = seq(2002,2018,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(all_sel, aes(year, population, color = code)) +
  geom_line(lwd = 2) +
  scale_x_continuous(breaks = seq(2002,2018,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#can = sel[2]
# Candidate
can = "E01033395"


for(can in sel){
  all_can = all[all$code %in% can,]
  clasif_can = clasif[clasif$code == can,]
  
  coeff = max(c(all_can$population, all_can$AllCars), na.rm = T) / max(all_can$cars_per_person, na.rm = T)
  
  ggplot(all_can, aes(x=year)) +
    
    geom_line( aes(y=cars_per_person), size=2, color="red") + 
    geom_line( aes(y=population / coeff) , size=2, color="green") +
    geom_line( aes(y=AllCars / coeff) , size=2, color="blue") +
    scale_x_continuous(breaks = seq(2002,2018,1)) +
    scale_y_continuous(
      name = "Cars per person",
      sec.axis = sec_axis(~.*coeff, name="Population people (green) / cars (blue)"),
      limits = c(0,max(all_can$cars_per_person) + 0.1)
    ) +
    theme(
      axis.title.y = element_text(color = "red", size=13),
      axis.title.y.right = element_text(color = "blue", size=13),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
    ggtitle(paste0(can," ",clasif_can$desc_cars)) +
    ggsave(paste0("plots/casestudy/LSOA_",can,"_timeseries.jpg"))
}




qtm(bounds[bounds$geo_code %in% can,], fill = NULL)


library(ggmap)
mal <- get_map('Malabo', zoom = 12, maptype = 'satellite')
ggmap(mal)

