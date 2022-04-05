# filename: 0_misc_xy_make_buffs_around_pts

#Create buffers around points of interest to filter to smaller sizes
#In my dissertation code, I call this 0_misc_XY_make_buffers_around_areas

library(tidyverse)
library(ggmap)
library(sf)
library(mapview) #loads leeaflet.
library(here) #updated 12/16/21 to add everything into the main analysis data folder and not the buffers
here()


#test push
#google api key registered in a separate script
union_station_tib = as_tibble("Union Station, Denver, Colorado") %>% 
  rename(address = value)
union_station_geo = mutate_geocode(union_station_tib, address, force=TRUE) %>%
  st_as_sf(coords = c("lon", "lat"),crs = 4326) %>% 
  st_transform(2876)
           
union_station_geo %>% mapview()

union_station_1_mi_2876 = union_station_geo %>% 
  st_buffer(5280)
union_station_1_mi_2876 %>% mapview()
setwd(here("data-processed"))
save(union_station_1_mi_2876, file = "union_station_1_mi.RData")
union_station_1_mi_4326 = union_station_1_mi %>% 
  st_transform(4326)
save(union_station_1_mi_4326, file = "union_station_1_mi_4326.RData")

