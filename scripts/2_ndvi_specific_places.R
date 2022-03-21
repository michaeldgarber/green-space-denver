#filename: 2_summarize_ndvi_specific_places
#the processing steps once data are downloaded and converted into raster in previous code
#note the previous code, which downloads the NDVI data using rgee, takes a while

library(tidyverse)
library(sf)
library(mapview)
library(terra)
library(raster)
library(here)
library(viridisLite)
library(viridis)
library(shades)
library(ggmap)
library(lubridate)
library(stringr)


# Load data--------------
## Load specific places provided by Michael Guidi----------
#Note these were imported as kml and converted to sf in a separate script:
#0_read_denver_native_zones.R
setwd(here("data-processed"))
load("places_native_geo.RData") #sent from Michael Guidi

places_native_geo %>% mapview(zcol = "place_name")
## Read raster data in terra format-------------
#i.e., load the data
setwd(here("data-processed"))
#read raster data. 
#see for additional discussion on the nuances of reading and writing raster data:
#1_get_landsat_ndvi_denver.R
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")


# Measure NDVI in the specific places-------------
load("places_native_nogeo.RData")
places_native_nogeo
#load the dataset of valid dates per those three test places
load("lookup_date_is_valid_all.RData")
lookup_date_is_valid_all
## create long-form dataset of NDVI by place over time--------
native_places_ndvi = ndvi_den_metro_terr_5_yr %>% 
  terra::extract(vect(places_native_geo)) %>% #for every day
  as_tibble() %>% 
  pivot_longer(
    cols = contains("20"),#contains a year that begins with 20..flexible 
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(   #now fix that date column using substr
    date_fixed_char = substr(date_ndvi, 1,8),
    date = lubridate::as_date(date_fixed_char)
  ) %>% 
  rename(
    place_id = ID) %>% #for left join below
  dplyr::select(place_id, ndvi, date) %>% 
  #link in the place names
  left_join(places_native_nogeo, by = "place_id") %>% 
  #link valid dates
  left_join(lookup_date_is_valid_all, by = "date")

names(native_places_ndvi)
native_places_ndvi %>% 
  filter(date_is_valid_all==1) %>% 
  dplyr::select(place_name_short, ndvi, date)

#this works but don't run...takes forever because so much data.
# native_places_ndvi %>% 
#   filter(date_is_valid_all==1) %>% 
#   ggplot(aes(x=date, y=ndvi))+
#   geom_line()+
#   geom_point()+
#   facet_grid(cols = vars(place_name))

## Summarize NDVI (mean, min, max, etc.) over time for each place-------
native_places_ndvi_day_geo = native_places_ndvi %>% 
  group_by(place_id, date) %>% 
  summarise(
    ndvi_min = min(ndvi, na.rm=TRUE),
    ndvi_max = max(ndvi, na.rm=TRUE),
    ndvi_25 = quantile(ndvi, probs = c(0.25), na.rm=TRUE),
    ndvi_75 = quantile(ndvi, probs = c(0.75), na.rm=TRUE),
    ndvi_med = median(ndvi, na.rm=TRUE), #note median,
    ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #recreate month and year columns
  mutate(
    month = lubridate::month(date),
    year=lubridate::year(date)
  ) %>% 
  left_join(places_native_geo, by = "place_id") %>%    #link in the place names
  left_join(lookup_date_is_valid_all, by = "date") %>% #valid dates
  st_as_sf()  #we have geometry so might as well use it.

#save and make a no-geo version for speed
setwd(here("data-processed"))
save(native_places_ndvi_day_geo, file = "native_places_ndvi_day_geo.RData")

native_places_ndvi_day_nogeo = native_places_ndvi_day_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(native_places_ndvi_day_nogeo, file = "native_places_ndvi_day_nogeo.RData")

## test NDVI on a cloud-free day--------------
table(lookup_date_is_valid_all$date_is_valid_all)
lookup_date_is_valid_all %>% filter(date_is_valid_all==1) 
pal_viridis_trunc=viridis::viridis_pal(end=.9) #trunc for truncated
native_places_ndvi_day_geo %>% 
  filter(date == "2016-06-09") %>% 
  mapview(
    layer.name = "NDVI, Median",
    col.regions = pal_viridis_trunc,
    zcol = "ndvi_med" 
  )

## ggplot - line graph of NDVI over time with median, 25th, and 75th as ribbon by area-------
library(scales)
names(native_places_ndvi_day_nogeo)
table(native_places_ndvi_day_nogeo$place_type)

#confirm valid dates are only in may, june, july, august
native_places_ndvi_day_nogeo %>% 
  filter(date_is_valid_all==1) %>% 
  group_by(month) %>% 
  summarise(n_obs=n())

### ggplot ndvi over time native spectrum areas---------
#Too many to graph at once
native_places_ndvi_day_nogeo %>% 
  filter(date_is_valid_all==1) %>% 
  filter(place_type == "native spectrum") %>% 
  ggplot(aes(
    x=date, 
    y=ndvi_med #plot median
  ))+
  geom_ribbon( #ribbon around 25th and 75th percentile
    aes(ymin =ndvi_25, ymax = ndvi_75 ),
    alpha=.4
  )+
  ylab("NDVI, Median") +
  xlab("Date") +
  geom_line( size=.7 ) +#note size better than lwd
  geom_point()+
  scale_x_date(breaks = pretty_breaks())+
#  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "6 months") +
  scale_y_continuous(
    limits = c(0, NA),  #force axis origin to be zero
    breaks= seq(0,0.8,by = 0.1))+ 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.border = element_rect(
          colour = "gray72", size = 0.5, fill=NA))+
  facet_grid(   #now facet them
    cols = vars(place_name_fac),
    labeller = label_wrap_gen() #wrap facet labels
  )

table(places_native_nogeo$place_name_fac)

### ggplot ndvi over time for high-diversity areas---------
table(native_places_ndvi_day_nogeo$place_type)
native_places_ndvi_day_nogeo %>% 
  filter(date_is_valid_all==1) %>% 
  filter(place_type == "high diversity") %>% 
  ggplot(aes(
    x=date, 
    y=ndvi_med #plot median
  ))+
  geom_ribbon( #ribbon around 25th and 75th percentile
    aes(ymin =ndvi_25, ymax = ndvi_75 ),
    alpha=.4
  )+
  ylab("NDVI, Median") +
  xlab("Date") +
  geom_line( size=.7 ) +#note size better than lwd
  geom_point()+
  scale_x_date(breaks = pretty_breaks())+
  scale_y_continuous(
    limits = c(0, NA),  #force axis origin to be zero
    breaks= seq(0,0.8,by = 0.1))+ 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.border = element_rect(
          colour = "gray72", size = 0.5, fill=NA))+
  facet_grid(   #now facet them
    cols = vars(place_name_fac),
    labeller = label_wrap_gen() #wrap facet labels
  )

### simple summaries of each type (percent nativity and high diversity)----------
native_places_ndvi_day_nogeo %>% 
  filter(date_is_valid_all==1) %>% 
  group_by(place_type, place_name_fac) %>% 
  summarise(ndvi_med = median(ndvi_med, na.rm=TRUE) )


## ggplot - NDVI vs percent native over all dates---------
#restricted to areas with nativity values
native_places_ndvi_day_nogeo %>% 
  filter(date_is_valid_all==1) %>% 
  filter(place_type == "native spectrum") %>% 
  ggplot(aes(
    x=native_percent, 
    y=ndvi_med #plot median
  ))+
  geom_point(
    aes(colour=place_name_fac), size = 1.5, 
    alpha=.5 #varying alpha to illustrate density
    ) +
  xlab("Percent native") +
  ylab("NDVI, median") +
  scale_y_continuous(
    limits = c(0, NA),  #force axis origin to be zero
    breaks= seq(0,0.8,by = 0.1))+ 
  scale_color_hue(
    name = "Place name"
  )
  
