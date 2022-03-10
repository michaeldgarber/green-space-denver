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
  st_simplify() %>% 
  mutate(
    area_ft2 =  as.numeric(st_area(geometry)),
    area_mi2 = area_ft2*3.58701e-8  )

object.size(den_parking)

setwd(here("data-processed"))
save(den_parking, file = "den_parking.RData")
den_parking %>% mapview(zcol = "TYPE")
table(den_parking$TYPE)
names(den_parking)
# summarize area & keep geometry for unary uion----------
#how many square miles of parking in Denver?
#calculate using the unary union method
den_parking_sum_overall =  den_parking %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>%
  summarise(
    area_ft2 = sum(area_ft2),
    area_mi2 = sum(area_mi2))
  ungroup() 
save(den_parking_sum_overall,  file = "den_parking_sum_overall.RData")

#denver is 154.7 mi2
den_parking_sum_overall$area_mi2/155
#so about 9% has been paved for parking.
den_parking_sum_overall %>% mapview()

# 500 m buffer--------
den_parking_500m = den_parking_sum_overall %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
save(den_parking_500m, file = "den_parking_500m.RData")
den_parking_500m %>% mapview()
