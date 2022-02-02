#Load polygons of various levels of native plants
#emailed from Michael Guidi Dec 2021

library(here)
library(sf)
library(tidyverse)
here()

# Read kml files---------

setwd(here("data-input"))
den_botanic_native_100  = sf::st_read("den_botanic_native_100.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 100,
    place_name = "Denver Botanic Gardens, 100% Native",
    place_name_short = "Denver Botanic G. Native Zone",
    comment = "mainly native prairie vegetation"
  )
den_botanic_native_100 %>% mapview()

setwd(here("data-input"))
city_park_native_0  = sf::st_read("city_park_native_0.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 0,
    place_name = "City Park, 0% Native",
    place_name_short = "City Park"
    
  )
city_park_native_0 %>% mapview()

setwd(here("data-input"))
suburban_open_space_1_native_50  = sf::st_read("suburban_open_space_1_native_50.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 50,
    place_name = "Suburban Open Space 1, Chatfield H.S., 50% Native",
    place_name_short = "Suburban Open Space 1"
    
    )
suburban_open_space_1_native_50 %>% mapview()

setwd(here("data-input"))
suburban_open_space_2_native_30  = sf::st_read("suburban_open_space_2_native_30.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 30,
    place_name = "Suburban Open Space 2, Columbine Hills Church, 30% Native",
    place_name_short = "Suburban Open Space 2"
  )
suburban_open_space_2_native_30 %>% mapview()


setwd(here("data-input"))
plains_conservation_center_native_100  = sf::st_read("plains_conservation_center_native_100.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 100,
    place_name = "Plains Conservation Center, 100% Native",
    place_name_short = "Plains Cons. Center"
    
  )
plains_conservation_center_native_100 %>% mapview()

setwd(here("data-input"))
hogback_along_c470_native_75  = sf::st_read("hogback_along_c470_native_75.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 75,
    place_name = "Hogback along C-470, 75% Native",
    place_name_short = "Hogback along C-470"
  )
hogback_along_c470_native_75 %>% mapview()

setwd(here("data-input"))
den_botanic_chatfield_native_10  = sf::st_read("den_botanic_chatfield_native_10.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 10,
    place_name = "Denver Botanic Gardens at Chatfield, 10% Native",
    place_name_short = "Denver Botanic G., Chatfield"
  )
den_botanic_chatfield_native_10 %>% mapview()

setwd(here("data-input"))
green_mtn_park_native_80_90  = sf::st_read("green_mtn_park_native_80_90.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 85,
    place_name = "Green Mountain Park, 85% Native",
    place_name_short = "Green Mountain"
  )
green_mtn_park_native_80_90 %>% mapview()

# Combine them---------
library(forcats)
places_native_geo = den_botanic_native_100 %>% 
  bind_rows(
    den_botanic_chatfield_native_10,
    city_park_native_0,
    suburban_open_space_1_native_50,
    suburban_open_space_2_native_30,
    green_mtn_park_native_80_90,
    hogback_along_c470_native_75,
    plains_conservation_center_native_100
  ) %>% 
  dplyr::select(-Name, -Description) %>% 
  #create another factor version for plotting sorted by native_percent
  mutate(
    place_name_fac = 
      fct_reorder(place_name, 
                  native_percent, 
                  .desc=T) #so native zones are earlier
    ) %>% 
  #arrange descending by percent native
  arrange(desc(native_percent)) %>% 
  mutate(place_id = row_number())

places_native_geo %>% 
  mapview(zcol = "native_percent",
          layer.name = "Percent native")
table(places_native_geo$place_name_fac)
places_native_nogeo = places_native_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
# save sf objects----------
setwd(here("data-processed"))
save(places_native_geo, file = "places_native_geo.RData")
save(places_native_nogeo, file = "places_native_nogeo.RData")

