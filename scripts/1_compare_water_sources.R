#filename: 1_wrangle_water

#This is an extension of these two:

  #~/green-space-denver/scripts/0_read_denver_water.R
  #~/green-space-denver/scripts/0_load_denver_osm_parks_water.R

#The first script creates, the following, which includes all of the polygons, 
#non-overlapping, as well as the streams and rivers represented 
#with 10-foot buffers around them if they are otherwise
#represented as a line to give them some area.
setwd(here("data-processed"))
load("den_osm_water_poly_both.RData")

#The second reads in the data from the Denver portal, including both streams and rivers
#and lakes and ponds.
load("den_streams_lakes_ponds.RData")

# Compare waterways obtained from OSM with that of Denver Portal ----------
#My purpose here is to compare them and come up with a final dataset for the creation of the riparian
#buffers.

mv_den_streams_lakes_ponds = den_streams_lakes_ponds %>% 
  mapview(color = "firebrick1", col.regions = "firebrick1", layer.name = "Denver Portal")

mv_den_osm_water_poly_both = den_osm_water_poly_both %>% 
  mapview(color = "blue", col.regions = "blue", layer.name = "OSM")

mv_den_streams_lakes_ponds + mv_den_osm_water_poly_both 


#so OSM basically got everything except for some stuff out by the airport. 
#3/6/22 Let's go with OSM
#For the moment and if we need better detail later, can work on merging the two with 
#a bit more care

