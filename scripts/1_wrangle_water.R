#filename: 1_wrangle_water

#This is an extension of these two:

  #~/green-space-denver/scripts/0_read_denver_water.R
  #~/green-space-denver/scripts/0_load_denver_osm_parks_water.R

#The first script creates, the following, which includes all of the polygons, non-overlapping, as well
#as the streams and rivers represented with 10-foot buffers around them if they are otherwise
#represented as a line to give them some area.
load("den_osm_water_poly_inc_waterways_10_ft.RData")

#The second reads in the data from the Denver portal, including both streams and rivers
#and lakes and ponds.

#My purpose here is to compare them and come up with a final dataset for the creation of the riparian
#buffers.
