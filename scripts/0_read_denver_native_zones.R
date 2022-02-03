#Load polygons of various levels of native plants
#emailed from Michael Guidi Dec 2021

library(here)
library(sf)
library(tidyverse)
here()

# Read kml files---------

## Denver Botanic Gardens 100 % Native Zone-------
setwd(here("data-input"))
den_botanic_native_100  = sf::st_read("den_botanic_native_100.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 100,
    place_name = "Denver Botanic Gardens, 100% Native",
    place_name_short = "Denver Botanic G. Native Zone",
    place_type = "native spectrum",
    comment = "mainly native prairie vegetation"
  )
den_botanic_native_100 %>% mapview()

## City Park 0% Native Zone-------
setwd(here("data-input"))
city_park_native_0  = sf::st_read("city_park_native_0.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 0,
    place_name = "City Park, 0% Native",
    place_type = "native spectrum",
    place_name_short = "City Park"
    
  )
city_park_native_0 %>% mapview()

## Suburban Open Space 1 near Chatfield High school-------
setwd(here("data-input"))
suburban_open_space_1_native_50  = sf::st_read("suburban_open_space_1_native_50.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 50,
    place_name = "Suburban Open Space 1, Chatfield H.S., 50% Native",
    place_type = "native spectrum",
    place_name_short = "Suburban Open Space 1"
    )
suburban_open_space_1_native_50 %>% mapview()

## Suburban Open Space 2 near Columbine Hills Church-------
setwd(here("data-input"))
suburban_open_space_2_native_30  = sf::st_read("suburban_open_space_2_native_30.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 30,
    place_name = "Suburban Open Space 2, Columbine Hills Church, 30% Native",
    place_type = "native spectrum",
    place_name_short = "Suburban Open Space 2"
  )
suburban_open_space_2_native_30 %>% mapview()


## Plains Conservation Center, 100% Native-------
setwd(here("data-input"))
plains_conservation_center_native_100  = sf::st_read("plains_conservation_center_native_100.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 100,
    place_name = "Plains Conservation Center, 100% Native",
    place_type = "native spectrum",
    place_name_short = "Plains Cons. Center"
    
  )
plains_conservation_center_native_100 %>% mapview()

## Hogback along C-470--------
setwd(here("data-input"))
hogback_along_c470_native_75  = sf::st_read("hogback_along_c470_native_75.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 75,
    place_name = "Hogback along C-470, 75% Native",
    place_type = "native spectrum",
    place_name_short = "Hogback along C-470"
  )
hogback_along_c470_native_75 %>% mapview()

setwd(here("data-input"))

## Denver Botanic Gardens, Chatfiled, 10% Native----------
den_botanic_chatfield_native_10  = sf::st_read("den_botanic_chatfield_native_10.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 10,
    place_name = "Denver Botanic Gardens at Chatfield, 10% Native",
    place_type = "native spectrum",
    place_name_short = "Denver Botanic G., Chatfield"
  )
den_botanic_chatfield_native_10 %>% mapview()

## Green Mountain Park---------
setwd(here("data-input"))
green_mtn_park_native_80_90  = sf::st_read("green_mtn_park_native_80_90.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    native_percent = 85,
    place_name = "Green Mountain Park, 85% Native",
    place_type = "native spectrum",
    place_name_short = "Green Mountain"
  )
green_mtn_park_native_80_90 %>% mapview()


## The second set of places sent Feb 1, 2021..high-diversity areas-----
### chatfield_meadow_restoration------
setwd(here("data-input", "high-diversity-plants"))
chatfield_meadow_restoration  = sf::st_read("chatfield_meadow_restoration.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    #no native_percent here...will be missing
    place_name = "Chatfield Meadow Restoration",
    place_type = "high diversity",
    place_name_short = "Chatfield Meadow"
  )

chatfield_meadow_restoration %>% mapview()

### chatfield_prairie_garden------
setwd(here("data-input", "high-diversity-plants"))
chatfield_prairie_garden  = sf::st_read("chatfield_prairie_garden.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    #no native_percent here...will be missing
    place_name = "Chatfield Prairie Garden",
    place_type = "high diversity",
    place_name_short = "Chatfield Prairie Garden"
  )

chatfield_prairie_garden %>% mapview()

### city_park_greenhouses------
setwd(here("data-input", "high-diversity-plants"))
city_park_greenhouses  = sf::st_read("city_park_greenhouses.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    #no native_percent here...will be missing
    place_name = "City Park Greenhouses",
    place_type = "high diversity",
    place_name_short = "City Park Greenhouses"
  )

city_park_greenhouses %>% mapview()

### den_botanic_green_roof------
setwd(here("data-input", "high-diversity-plants"))
den_botanic_green_roof  = sf::st_read("den_botanic_green_roof.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    #no native_percent here...will be missing
    place_name = "Denver Botanic Gardens Green Roof",
    place_type = "high diversity",
    place_name_short = "Denver Botanic G. Green Roof"
  )

den_botanic_green_roof %>% mapview()

### kendrick_lake_xeriscape_garden------
setwd(here("data-input", "high-diversity-plants"))
kenderick_lake_xeriscape_garden  = sf::st_read("kenderick_lake_xeriscape_garden.kml") %>% 
  st_transform(4326) %>% 
  st_zm() %>%  #drop z columnn
  st_sf() %>% 
  mutate(
    #no native_percent here...will be missing
    place_name = "Kenderick Lake Xeriscape Garden",
    place_type = "high diversity",
    place_name_short = "Kenderick Lake Xeriscape Garden"
  )

kenderick_lake_xeriscape_garden %>% mapview()



# Combine them---------
library(forcats)
sf::sf_use_s2(TRUE) #was creating invalid spherical geometry. 
#note this works but then I can't get a mapview b/c geometry is mixed.
#https://github.com/r-spatial/mapview/issues/342
#so keep true and don't measure area.
#https://gis.stackexchange.com/questions/413584
places_native_geo = den_botanic_native_100 %>% 
  bind_rows(
    #the original set of places
    den_botanic_chatfield_native_10,
    city_park_native_0,
    suburban_open_space_1_native_50,
    suburban_open_space_2_native_30,
    green_mtn_park_native_80_90,
    hogback_along_c470_native_75,
    plains_conservation_center_native_100,
    
    #the second set
    chatfield_meadow_restoration,
    chatfield_prairie_garden,
    city_park_greenhouses,
    den_botanic_green_roof,
    kenderick_lake_xeriscape_garden
  ) %>% 
  dplyr::select(
    -starts_with("Name"),
    -starts_with("Description")) %>% #soft code just in case
  #create another factor version for plotting sorted by native_percent
  mutate(
    place_name_fac = 
      fct_reorder(place_name, 
                  native_percent, 
                  .desc=T) #so native zones are earlier
    ) %>% 
  #arrange descending by percent native
  arrange(desc(native_percent)) %>% 
  st_make_valid() %>% #for measuring area. doesn't work for all
  mutate(
#    area_m2 = as.numeric(st_area(geometry)), #see above
    place_id = row_number()) 

mapviewOptions(fgb = TRUE)
places_native_geo %>% 
  mapview(zcol = "native_percent",
          layer.name = "Percent native")
places_native_geo %>% 
  mapview(zcol = "place_type",
          layer.name = "Place category")

table(places_native_geo$place_name_fac)
places_native_nogeo = places_native_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
# save sf objects----------
setwd(here("data-processed"))
save(places_native_geo, file = "places_native_geo.RData")
save(places_native_nogeo, file = "places_native_nogeo.RData")

