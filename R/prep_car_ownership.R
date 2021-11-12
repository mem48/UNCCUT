# LSOA Data
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
tmap_mode("view")


dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/CREDS Data/github-secure-data/Historical_Car_Emissions_LSOA.zip",
      exdir = "tmp")
dat_raw <- read.csv("tmp/Historical_Car_Emissions_LSOA.csv")
unlink("tmp", recursive = TRUE)



dat <- dat_raw[,c("year","LSOA","fuel","AllCars")]
dat <- dat %>%
  group_by(year, LSOA) %>%
  summarise(AllCars = sum(AllCars, na.rm = TRUE))

dat_ave <- dat %>%
  group_by(year) %>%
  summarise(AllCars = mean(AllCars, na.rm = TRUE))
dat_ave$LSOA <- "Average"

dat <- rbind(dat, dat_ave)

dat_wide <- pivot_wider(dat, 
                        id_cols = "LSOA",
                        names_from = "year", 
                        values_from = "AllCars")

#dat = dat[order(dat$LSOA), ]

quantile(dat$AllCars[dat$year == 2018], probs = seq(0.99,1,0.001))

# Filter out Large numbers of cars
dat <- dat[!dat$LSOA %in% c("zBetween Keepers","zUnknown"),]
ext <- unique(dat$LSOA[dat$AllCars > 3000]) #99.8% of LSOA have less than 2549 cars
dat <- dat[!dat$LSOA %in% ext,]

ggplot() +
  geom_line(data = dat,
            aes(x = year, y = AllCars, group = LSOA), 
            color = "grey", 
            alpha = 0.4) +
  geom_line(data = dat[dat$LSOA == "Average",],
            aes(x = year, y = AllCars),
            color = "red")

sel_lsoa <- c("E01004736","E01033269","E01018465","E01003182","E01030478","E01030679","E01002444","E01024149") # Some intresting examples
ggplot() +
  geom_line(data = dat[dat$LSOA %in% sel_lsoa,],
            aes(x = year, y = AllCars, color = LSOA)) +
  geom_line(data = dat[dat$LSOA == "Average",],
            aes(x = year, y = AllCars),
            color = "red", linetype = "dashed") +
  ylim(c(0,3000))



summary(dat$AllCars)
quantile(dat$AllCars, probs = seq(0,1,0.01))

# Find the trendline for each LSOA
dat_split <- dat %>%
  group_by(LSOA) %>%
  group_split()

res <- pbapply::pblapply(dat_split, function(x){
  nm <- x$LSOA[1]
  mod <- lm(AllCars ~ year, data = x)
  gradient <- mod$coefficients[2]
  rsquare <- summary(mod)$r.squared
  return(data.frame(LSOA = nm, 
                    gradient = gradient, 
                    rsquare = rsquare))
})
res <- bind_rows(res)
row.names(res) <- 1:nrow(res)

# Plot Map
dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_clipped.zip",
      exdir = "tmp")
bounds <- st_read("tmp/infuse_lsoa_lyr_2011_clipped.shp")
unlink("tmp", recursive = TRUE)

bounds <- bounds[,"geo_code"]
bounds <- bounds[substr(bounds$geo_code,1,1) != "9",]
bounds <- st_simplify(bounds, dTolerance = 10)

dat_growth <- left_join(bounds, res, by = c("geo_code" = "LSOA"))
dat_growth <- st_as_sf(dat_growth)
summary(dat_growth$gradient)

dat_growth$rsquare_inv = (1 - dat_growth$rsquare)
dat_growth_lines <- st_cast(dat_growth, "MULTILINESTRING")


tm_shape(dat_growth) +
  tm_fill("gradient",
          style = "fixed",
          breaks = c(-130,-50,-10,0,7.9,10,20,50,170),
          midpoint = 7.9, 
          #palette = "Spectral",
          palette = c("#2166ac",
                      "#4393c3",
                      "#92c5de",
                      "#b2abd2",
                      "#fddbc7",
                      "#f4a582",
                      "#d6604d",
                      "#b2182b")
          ) +
  tm_shape(dat_growth_lines) +
  tm_lines(col = "black",
           lwd = "rsquare_inv",
           scale = 2)

st_write(dat_growth, "data/change_ownership.gpkg")


plot(2001:2018, dat_ave$AllCars)
