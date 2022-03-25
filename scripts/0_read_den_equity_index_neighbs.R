#Revised March 11 2022
library(tidyverse)
library(mapview)
library(sf)
library(here)
# read neighborhood data-----------
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-statistical-neighborhoods
setwd(here("data-input", "city-of-denver-data"))
den_neighb = st_read(dsn ="neighborhoods") %>%
  st_transform(2876) %>% #central colorado, feet
  st_make_valid() %>% 
  #measure area of neighborhoods
  mutate(
    nbhd_area_ft2 = as.numeric(st_area(geometry)),
    nbhd_area_m2 = nbhd_area_ft2/10.764, #meters squared, for informational purposes.
    nbhd_area_mi2 = nbhd_area_ft2/(5280**2) #miles squared
  ) %>% 
  rename(#rename some vars for future joins
    nbhd_id = NBHD_ID,
    nbhd_name = NBHD_NAME,
    nbhd_notes = NOTES,
    nbhd_type = TYPOLOGY
  )

  
setwd(here("data-processed"))
save(den_neighb, file = "den_neighb.RData")
den_neighb %>% mapview()


#about how big (area) are neighborhoods?

den_neighb %>% 
  ggplot(aes(area_mi2))+
  geom_histogram()
options(scipen = 999)
summary(den_neighb$area_mi2)
#so about 1-2 square miles each.
#okay, if people are on average exposed within a 500m buffer, that's
ft_in_500m = 500*3.28
#how big would a circle of that radius be in square miles?
circle_area_in_ft2_500_m = pi*ft_in_500m**2
circle_area_in_mi2_500_m= circle_area_in_ft2_500_m/(5280**2)
circle_area_in_mi2_500_m
den_neighb %>%   mapview(
  col.regions = rainbow(n_distinct(den_neighb$NBHD_NAME)),
  zcol = "NBHD_NAME")

# read Denver equity index------
#These are the neighborhood level. Neighborhoods are typically comprised
#of census tracts.
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-equity-index-2020-neighborhood
#note some of these measures are somewat dated
setwd(here("data-input", "city-of-denver-data"))
den_equity_ind_2020 = st_read(dsn ="equity-index-2020") %>%
  st_transform(2876) %>% #central colorado, feet
  st_make_valid()  %>% 
  rename(
    #clean up var names a bit in case we need to link to other data at tract/bg level
    nbhd_name = NBRHD_NAME, #make it match other neighborhood data so it will link
    nbhd_care_access_score = ACCESSTOCA, 
    nbhd_socioecon_score = SOCIOECON_,
    nbhd_morbidity_score = MORBIDITY_,
    nbhd_mortality_score = MORTALITY_,
    nbhd_built_env_score = BUILTENV_S,
    nbhd_overall_equity_score = OVERALLEQU
  )

setwd(here("data-processed"))
save(den_equity_ind_2020, file = "den_equity_ind_2020.RData")
mapview(den_equity_ind_2020)
den_equity_ind_2020 %>% mapview(zcol = "nbhd_overall_equity_score")

#it's by neighborhood. interesting.
mv_den_equity_ind_2020= den_equity_ind_2020 %>% mapview(
  col.regions = rainbow(n_distinct(den_equity_ind_2020$nbhd_name)),
  zcol = "nbhd_name")
mv_den_equity_ind_2020
#how do the neighborhoods compare with census tracts?
setwd(here("data-processed"))
load("den_co_tract_geo.RData")

#how big are most census tracts?
den_co_tract_wrangle_geo = den_co_tract_geo %>% 
  st_transform(2876) %>% #central colorado, feet
  mutate(
    area_ft2 = as.numeric(st_area(geometry)),
    area_m2 = area_ft2/10.764, #meters squared, for informational purposes.
    area_mi2 = area_ft2/(5280**2) #miles squared
  ) 
summary(den_co_tract_wrangle_geo$area_mi2)
mv_den_co_tract_geo = den_co_tract_wrangle_geo %>% 
  mapview(zcol = "tract_fips")
mv_den_equity_ind_2020+mv_den_co_tract_geo

# Create a look-up table for neighborhoods and census tracts in Denver---------
#only variables unique to neighborhood; i.e., remove area measurements
names(den_neighb)

den_neighb_for_join  = den_neighb %>% 
  dplyr::select(-contains("area")) 


lookup_den_neighb_tract = den_co_tract_geo %>% 
  st_join(den_neighb, largest=TRUE)#ensure only largest
save(lookup_den_neighb_tract, file = "")


