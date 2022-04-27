library(readxl)
library(sf)
library(tmap)
tmap_mode("view")

dat = read_xlsx("E:/Users/earmmor/OneDrive - University of Leeds/Documents/DecarboN8/Car Demand/New Stations.xlsx")

head(dat)

coord = strsplit(dat$Notes,",")

for(i in 1:nrow(dat)){
  if(is.na(dat$Lat[i])){
    dat$Lat[i] = as.numeric(coord[[i]][1])
  }
  if(is.na(dat$Lng[i])){
    dat$Lng[i] = as.numeric(coord[[i]][2])
  }
}

dat = dat[,c("Railway Station","Year","Lat","Lng")]
names(dat) = c("stop","year","Lat","Lng")

dat = st_as_sf(dat, coords = c("Lng","Lat"))
qtm(dat, dots.col = "year")

st_write(dat,"data/new_stations.geojson")
