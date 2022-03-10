#filename: 1_wrangle_water

#This is an extension of these two:

  #~/green-space-denver/scripts/0_read_denver_water.R
  #~/green-space-denver/scripts/0_load_denver_osm_parks_water.R

#The first script creates, the following, which includes all of the polygons, non-overlapping, as well
#as the streams and rivers represented with 10-foot buffers around them if they are otherwise
#represented as a line to give them some area.
setwd(here("data-processed"))
load("den_osm_water_poly_inc_waterways_10_ft.RData")

#The second reads in the data from the Denver portal, including both streams and rivers
#and lakes and ponds.
load("den_streams_lakes_ponds.RData")

# Compare waterways obtained from OSM with that of Denver Portal ----------
#My purpose here is to compare them and come up with a final dataset for the creation of the riparian
#buffers.

mv_den_streams_lakes_ponds = den_streams_lakes_ponds %>% 
  mapview(color = "firebrick1", col.regions = "firebrick1", layer.name = "Denver Portal")

mv_den_osm_water_poly_inc_waterways_10_ft = den_osm_water_poly_inc_waterways_10_ft %>% 
  mapview(color = "blue", col.regions = "blue", layer.name = "OSM")

mv_den_streams_lakes_ponds + mv_den_osm_water_poly_inc_waterways_10_ft 
#so OSM basically got everything except for some stuff out by the airport. 
#3/6/22 Let's go with OSM
#for the moment and if we need better detail later, can work on merging the two with 
#a bit more care

# Create buffers around the waterways from OSM-----
#meta-comment: I need to simplify the name and will lose some description. oh well.
st_crs(den_osm_water_poly_inc_waterways_10_ft)
#restrict to Denver County
load(file = "den_co_geo.RData")
den_co_geo_2876 = den_co_geo %>% 
  st_transform(2876) #co for county 
den_co_geo_2876 %>% mapview()
den_co_osm_water = den_osm_water_poly_inc_waterways_10_ft %>% 
  st_intersection(den_co_geo_2876)
save(den_co_osm_water, file = "den_co_osm_water.RData")
den_co_osm_water_200ft = den_co_osm_water %>% 
  st_buffer(200)
save(den_co_osm_water_200ft, file = "den_co_osm_water_200ft.RData")
den_co_osm_water_100ft = den_co_osm_water %>% 
  st_buffer(100)
save(den_co_osm_water_100ft, file = "den_co_osm_water_100ft.RData")

den_co_osm_water_50ft = den_co_osm_water %>% 
  st_buffer(50)
save(den_co_osm_water_50ft, file = "den_co_osm_water_50ft.RData")

den_co_osm_water_200ft %>% mapview()
#Create a 500 m buffer for residential exposure--------
500*3.28084
den_co_osm_water_500m = den_co_osm_water %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
save(den_co_osm_water_500m, file = "den_co_osm_water_500m.RData")
den_co_osm_water_500m %>% mapview()
#union this as well so it's one geo
den_co_osm_water_500m_union = den_co_osm_water_500m %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>%
  summarise(n=n()) %>%
  ungroup() %>% 
  dplyr::select(geometry)
den_co_osm_water_500m_union %>% mapview()
save(den_co_osm_water_500m_union, 
     file = "den_co_osm_water_500m_union.RData")
