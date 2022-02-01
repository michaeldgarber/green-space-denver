#Practice with Terra based on Robin's book
#https://geocompr.robinlovelace.net/raster-vector.html

#installing per instructions in Robin's book - https://geocompr.robinlovelace.net/spatial-class.html#spatial-class
# install.packages("terra")
# install.packages("spData")
# install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')

library(tidyverse)
library(sf)
library(spData)
library(spDataLarge)
library(mapview)
library(terra)
library(raster)
library(here)
library(viridisLite)
library(viridis)
#Following here, as noted above:
#https://geocompr.robinlovelace.net/raster-vector.html
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, crs(srtm))

#- extract elevations values for a line-----------
data("zion_points", package = "spDataLarge")
elevation = terra::extract(srtm, vect(zion_points)) #extract the information from the raster
zion_points = cbind(zion_points, elevation) %>% #add the points back with the geometry
  rename(elevation = srtm)
zion_points %>% mapview(zcol = "elevation")
srtm
zion_points

#-extract elevation values for a polygon
zion
zion_elev_polygon = srtm %>% 
  terra::extract(vect(zion)) %>% 
  rename(elev = srtm) %>% 
  group_by(ID) %>% #looks like ID is automatically generated.
  summarise(elev = mean(elev))




zion_elev_polygon
class(zion)
srtm
zion_srtm_values = terra::extract(x = srtm, y = vect(zion))