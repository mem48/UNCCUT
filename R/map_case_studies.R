library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

files <- list.files("plots/casestudy/sorted/", recursive = TRUE, full.names = FALSE)
files <- strsplit(files, "/")
lsoa <- sapply(files,`[[`,2)
lsoa <- gsub("LSOA_","",lsoa)
lsoa <- gsub("_timeseries.jpg","",lsoa)
classif <- sapply(files,`[[`,1)

bounds <- read_sf("data/LSOA_bounds_simplified.gpkg")
bounds <- bounds[bounds$geo_code %in% lsoa,]
bounds <- bounds[match(lsoa, bounds$geo_code), ]
bounds$class <- classif

bounds <- bounds[bounds$class != "error", ]
qtm(bounds, fill = "class")

st_write(bounds, "data/case_study_locations2.geojson")
st_write(bounds, "data/case_study_locations2.kml")


bounds <- cbind(bounds, st_coordinates(st_centroid(bounds)))
bounds <- bounds[order(bounds$Y, decreasing = TRUE),]

dat <- st_drop_geometry(bounds)
#write.csv(dat,"data/case_study_locations.csv")

dat$X <- NULL
dat$Y <- NULL

#add in old work
old <- read.csv("data/case_study_locations.csv")
old$class <- NULL

dat2 <- left_join(dat,old, by = "geo_code")
write.csv(dat2,"data/case_study_locations2.csv", row.names = FALSE, na = "")
