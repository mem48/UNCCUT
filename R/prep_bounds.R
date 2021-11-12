library(sf)
dir.create("tmp")
unzip("E:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_clipped.zip",
      exdir = "tmp")
bounds <- st_read("tmp/infuse_lsoa_lyr_2011_clipped.shp")
unlink("tmp", recursive = TRUE)

bounds <- bounds[,"geo_code"]
bounds <- bounds[substr(bounds$geo_code,1,1) != "9",]
object.size(bounds)
bounds <- st_simplify(bounds, dTolerance = 10)
object.size(bounds)
bounds <- st_transform(bounds, 4326) 
st_write(bounds,"data/LSOA_bounds_simplified.gpkg")
