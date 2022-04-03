#filename: 0_read_parking_lots
library(tidyverse)
library(sf)
library(mapview)
library(here)
#load this 
#https://www.denvergov.org/opendata/dataset/parking-lots-2016

# Load parking data and measure area--------
#dsn = #the folder where the shapefile is located
setwd(here("data-input", "city-of-denver-data"))
den_parking = st_read(dsn ="parking") %>%
  st_transform(2876) %>% #local feet
  st_make_valid() %>% 
  st_simplify() %>% #reduce the object size a bit
  mutate(
    area_ft2 =  as.numeric(st_area(geometry)),
    area_mi2 = area_ft2*3.58701e-8  )

object.size(den_parking)

setwd(here("data-processed"))
save(den_parking, file = "den_parking.RData")
den_prkng = den_parking #I renamed it elsewhere so doing so here as well.
den_parking %>% mapview(zcol = "TYPE")
table(den_parking$TYPE)
names(den_parking)
# summarize area & keep geometry for unary uion----------
#how many square miles of parking in Denver?
#calculate using the unary union method;
#call it marg for marginal as I've done elsewhere
den_prkng_sum_marg =  den_prkng %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>%
  summarise(
    area_ft2 = sum(area_ft2),
    area_mi2 = sum(area_mi2)) %>% 
  ungroup() %>% 
  st_simplify() %>% 
  st_as_sf() %>% 
  dplyr::select(-dummy)
save(den_prkng_sum_marg,  file = "den_prkng_sum_marg.RData")
object.size(den_prkng_sum_marg)

#summarize by type. this will also create a smaller file.
den_prkng_sum_by_type =  den_prkng %>% 
  group_by(TYPE) %>%
  summarise(
    area_ft2 = sum(area_ft2),
    area_mi2 = sum(area_mi2)) %>% 
  ungroup() 
save(den_prkng_sum_by_type,  file = "den_prkng_sum_by_type.RData")
den_prkng_sum_by_type %>% mapview(zcol = "TYPE")

table(den_prkng_sum_by_type$TYPE)
den_prkng_sum_impervious_only = den_prkng_sum_by_type %>% 
  filter(TYPE == "Impervious")
save(den_prkng_sum_impervious_only, file = "den_prkng_sum_impervious_only.RData")
den_prkng_sum_impervious_only %>% mapview()
den_prkng_sum_pervious_only  = den_prkng_sum_by_type %>% 
  filter(TYPE == "Pervious")
den_prkng_sum_pervious_only %>% mapview()

#denver is 154.7 mi2
den_prkng_sum_marg$area_mi2/155
#so about 9% has been paved for prkng.
den_prkng_sum_marg %>% mapview()

# 500 m buffer--------
#for some reason, this buffer code is taking forever. 
setwd(here("data-processed"))
load("den_prkng_sum_marg.RData")
den_prkng_sum_marg %>% mapview()
#500 meters, but we're in feet
dist_500_m = 500*3.28084
den_prkng_500m = den_prkng_sum_marg %>% 
  st_union() %>% 
  st_buffer(dist_500_m) %>% #500 meters, but we're in feet
  st_as_sf() #possibly redundant, but seems to work better with this.

save(den_prkng_500m, file = "den_prkng_500m.RData")
load("den_prkng_500m.RData")
den_prkng_500m %>% mapview()
object.size(den_prkng_500m)
