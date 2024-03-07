#filename: 0_read_parking_lots
library(tidyverse)
library(sf)
library(mapview)
library(here)
#load this 
#https://www.denvergov.org/opendata/dataset/parking-lots-2016

# Load parking data and measure area--------
#dsn = #the folder where the shapefile is located
library(here)
library(sf)
library(tidyverse)
setwd(here("data-input", "city-of-denver-data"))
den_prkng = st_read(dsn ="parking") %>%
  st_transform(2876) %>% #local feet
  st_make_valid() %>% 
  #I tried 5. 10 lost too much detail. 7 might work.
  st_simplify(dTolerance = 7) %>% #reduce the object size so there are vertices max every x ft
  mutate(
    prkng_id = row_number(),
    area_ft2_prkng =  as.numeric(st_area(geometry)),
    area_mi2_prkng = area_ft2_prkng*3.58701e-8  )

object.size(den_prkng) #the st_sipmlify cut the object size in half!
den_prkng %>% mapview()
setwd(here("data-processed"))
save(den_prkng, file = "den_prkng.RData")
# summarize area & keep geometry for unary uion----------
#how many square miles of parking in Denver?
#calculate using the unary union method;
#call it marg for marginal as I've done elsewhere
#changed name again 4/18/22 to be consistent with other scenarios and 
#more easily find and replace with comp
den_prkng_tx_marg =  den_prkng %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>%
  summarise(
    area_ft2_prkng = sum(area_ft2_prkng),
    area_mi2_prkng = sum(area_mi2_prkng)) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  dplyr::select(-dummy)
save(den_prkng_tx_marg,  file = "den_prkng_tx_marg.RData")
object.size(den_prkng_tx_marg)

#summarize by type. this will also create a smaller file.
den_prkng_by_type =  den_prkng %>% 
  group_by(TYPE) %>%
  summarise(
    area_ft2_prkng = sum(area_ft2_prkng),
    area_mi2_prkng = sum(area_mi2_prkng)) %>% 
  ungroup() 
save(den_prkng_by_type,  file = "den_prkng_by_type.RData")
den_prkng_by_type %>% mapview(zcol = "TYPE")

table(den_prkng_by_type$TYPE)
den_prkng_impervious = den_prkng_by_type %>% 
  filter(TYPE == "Impervious")
save(den_prkng_impervious, file = "den_prkng_impervious.RData")
den_prkng_impervious %>% mapview()
den_prkng_sum_pervious_only  = den_prkng_by_type %>% 
  filter(TYPE == "Pervious")
den_prkng_sum_pervious_only %>% mapview()

#denver is 154.7 mi2
den_prkng_tx_marg$area_mi2_prkng/155
#so about 9% has been paved for prkng.
den_prkng_tx_marg %>% mapview()

# 500 m buffer--------
#for some reason, this buffer code is taking forever. 
setwd(here("data-processed"))
load("den_prkng_tx_marg.RData")
den_prkng_tx_marg %>% mapview()
#500 meters, but we're in feet
dist_500_m = 500*3.28084
#changed name from den_ prkng_ 500m to den_prkng_res
den_prkng_res = den_prkng_tx_marg %>% 
  st_union() %>% 
  st_buffer(dist_500_m) %>% #500 meters, but we're in feet
  st_as_sf() #possibly redundant, but seems to work better with this.

save(den_prkng_res, file = "den_prkng_res.RData")
load("den_prkng_res.RData")
den_prkng_res %>% mapview()
object.size(den_prkng_res)
