# RGISTools
# Following the demo here - https://moradii.github.io/PartI.html
library(sf)
library(tidyverse)
library(terra)
#library(devtools)
install.packages("RGISTools")#more stable
#install_github("spatialstatisticsupna/RGISTools")
library(RGISTools) #cool, this requires sf. that's great.
library(mapview)
library(tmap)
library(tmaptools)

#----Registration--------######
#Requires EarthData and SciHub accounts
#I am mdg711 for both
earth_data_username = "mdg711"
earth_data_pw = "dkhG757D?$xKpsGBj49K"

copernicus_username ="mdg711"
copernicus_pw = "o$AT#P$98iAmn6ig"



#-----Define region of interest-----#####
spain = raster::getData('GADM', country = "Spain", level = 2)
castellon = raster::subset(spain, NAME_2 == "Castell?n")

#mapview(castellon)
#Or use tmap if you want

tmap_mode("view")
tmap_leaflet(tm_shape(castellon) +
               tm_polygons(col = "red") +
               tm_view(set.view = 4))

#Exercise: do Germany
germany = raster::getData('GADM', country = "Germany", level = 2)
table(germany$NAME_1)
nordrhein = raster::subset(germany, NAME_1 == "Nordrhein-Westfalen")
#and visualize - neat
tmap_leaflet(tm_shape(nordrhein) +
               tm_polygons(col = "red") +
               tm_view(set.view = 4))

#----Download data using RGISTools-----#####
#-Retrieve-----#

sres.ndvi= RGISTools::modSearch(
  product = "MOD13A2", #product name
  region = castellon, #defined above
  dates = seq(as.Date("2001-01-01"),
                      as.Date("2018-12-31"),1)) #define a time period







