url = "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv"
dir.create("temp_naptan")
utils::download.file(url = url, destfile = "temp_naptan/Stops.csv", mode = "wb", quiet = TRUE)
naptan <- readr::read_csv("temp_naptan/Stops.csv", progress = FALSE, show_col_types = FALSE)
unlink("temp_naptan", recursive = TRUE)

library(lubridate)
library(tmap)
library(sf)
library(dplyr)
library(fuzzyjoin)
tmap_mode("view")


# NPATAN is useless for station / bus stop ages

#https://trundleage.co.uk/reopened-railway-stations/

new_stations <- read.csv("data/new_stations.csv")


# clean file
naptan <- naptan[, c("ATCOCode", "NaptanCode", "CommonName", "Easting", "Northing","BusStopType",
                     "CreationDateTime","ModificationDateTime","RevisionNumber","Modification","Status")]
naptan <- naptan[!is.na(naptan$Easting),]
naptan <- sf::st_as_sf(naptan, coords = c("Easting", "Northing"), crs = 27700)
naptan <- st_transform(naptan, 4326)
naptan <- naptan[is.na(naptan$BusStopType),]

foo <- stringdist_left_join(new_stations, naptan, 
                       by = c("Railway.Station" = "CommonName"),
                       ignore_case = TRUE,
                       method = "jw")
foo <- st_as_sf(foo)
foo <- foo[!st_is_empty(foo),]
tm_shape(foo) +
  tm_dots(col = "Year")


#naptan_study <- naptan_study[naptan_study$Status == "active",]
naptan_study <- naptan_study[naptan_study$CreationDateTime >= ymd("2016-01-01"), ] # Most stops created arourund early 2000s start of database?
naptan_study <- naptan_study[naptan_study$CreationDateTime <= ymd("2018-12-31"), ]
hist(year(naptan_study$CreationDateTime))

naptan_study$secs <- as.numeric(naptan_study$CreationDateTime)

tm_shape(naptan_study) +
  tm_dots(col = "secs")


#hist(year(naptan$CreationDateTime))

naptan <- sf::st_transform(naptan, 4326)
naptan <- cbind(sf::st_drop_geometry(naptan), sf::st_coordinates(naptan))
names(naptan) <- c("stop_id", "stop_code", "stop_name", "stop_lon", "stop_lat")

naptan$stop_lon <- format(round(naptan$stop_lon, 6), scientific = FALSE)
naptan$stop_lat <- format(round(naptan$stop_lat, 6), scientific = FALSE)

# Append alterative tags
naptan_extra <- naptan_extra[!naptan_extra$stop_id %in% naptan$stop_id,]
naptan <- rbind(naptan, naptan_extra)
  
