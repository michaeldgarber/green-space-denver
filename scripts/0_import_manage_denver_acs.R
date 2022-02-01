#filename: 0_import_manage_denver_acs
#Purpose: download demographics data for Denver area (county, metro, to be defined)
#Date began 12/12/21
#Date reviwsed: 12/14/21
library(tidyverse)
library(sf)
library(mapview)
library(tidycensus)
library(here)
setwd(here("data-processed"))

#prep
## wrangle the ACS variables in prep for the long-form data-----------
options(tigris_use_cache = TRUE)
acs_2019_vars <- load_variables(2019, "acs5", cache = TRUE)

save(acs_2019_vars, file = "acs_2019_vars.RData") #takes a while so save.
#make it a tibble so you can select just the vars that begin with B01001
acs_2019_vars_tib = acs_2019_vars %>% 
  as_tibble() %>%
  #could also do the grepl framework but this is more straightforward
  mutate(sex_by_age = case_when(
    concept == "SEX BY AGE" ~ 1,
    TRUE ~ 0
  )) %>% 
  filter(sex_by_age==1)

#now grab the vars as a vector
acs_2019_vars_sex_by_age = acs_2019_vars_tib %>% dplyr::select(name) %>% pull()

lookup_acs_2019_var_label = acs_2019_vars_tib %>% 
  #rename first so it's var_name and var_label. less ambiguous with, e.g., census tract name
  rename(
    var_name = name,
    var_label = label
  ) %>% 
  distinct(var_name, var_label) %>% 
  mutate(
    var_sex = case_when(   #sex indicator
      #(grepl("needle", haystack))
      grepl("Male", var_label) ~ "Male",
      grepl("Female", var_label) ~ "Female"
    ),
    age_group_acs = case_when(
      grepl("Under 5", var_label) ~ "0-4",
      grepl("5 to 9", var_label) ~ "5-9",
      grepl("10 to 14", var_label) ~ "10-14",
      grepl("15 to 17", var_label) ~ "15-17",
      grepl("18 and", var_label) ~ "18-19",
      grepl("20 y", var_label) ~ "20",
      grepl("21 y", var_label) ~ "21",
      grepl("22 to", var_label) ~ "22-24",
      grepl("25 to", var_label) ~ "25-29",
      grepl("30 to", var_label) ~ "30-34",
      grepl("35 to", var_label) ~ "35-39",
      grepl("40 to", var_label) ~ "40-44",
      grepl("45 to", var_label) ~ "45-49",
      grepl("50 to", var_label) ~ "50-54",
      grepl("55 to", var_label) ~ "55-59",
      grepl("60 and", var_label) ~ "60-61",
      grepl("62 to", var_label) ~ "62-64",
      grepl("65 to", var_label) ~ "65-66",
      grepl("67 to", var_label) ~ "67-69",
      grepl("70 to", var_label) ~ "70-74",
      grepl("75 to", var_label) ~ "75-79",
      grepl("80 to", var_label) ~ "80-84",
      grepl("85 years", var_label) ~ "85+"
    )
  )

table(lookup_acs_2019_var_label$age_group_acs) #good
table(lookup_acs_2019_var_label$var_label)

lookup_acs_2019_var_label


# download tract-level geometry for denver metro -----------

#https://data-cdphe.opendata.arcgis.com/datasets/colorado-county-boundaries/
#FIPS codes
#denver 031
#jefferson 059
#douglas 035
#arapahoe 005
#adams 001
#denver FIPS code: 031
## grab geometry of each tract-------------
den_metro_tracts_geo  = tidycensus::get_acs(
  geography = "tract", 
  year=2019, #setting to 2019 as midpoint of study. otherwise, it will take the most recent.
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = acs_2019_vars_sex_by_age[1],
  geometry = TRUE  #takes a long time so just do this once
) %>% 
  #some light renaming, etc.
  mutate(
    tract_fips = GEOID,
    county_fips = str_sub(GEOID, 3,5)
  ) %>% 
  dplyr::select(contains("fips"), geometry)%>% 
  st_transform(4326)

save(den_metro_tracts_geo, file = "den_metro_tracts_geo.RData")

#a version restricted to just denver and jefferson counties (but still at tract level)
load("den_metro_tracts_geo.RData")
den_jeff_co_tracts_geo = den_metro_tracts_geo %>% 
  filter(county_fips == "031" | county_fips == "059") 
save(den_jeff_co_tracts_geo, file = "den_jeff_co_tracts_geo.RData")
den_jeff_co_tracts_geo %>% mapview(zcol = "county_fips")  

#and a no-geo version
den_jeff_co_tracts_nogeo = den_jeff_co_tracts_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_jeff_co_tracts_nogeo, file = "den_jeff_co_tracts_nogeo.RData")
## grab geometry of counties--------------
den_metro_co_geo  = tidycensus::get_acs(
  geography = "county", 
  year=2019, #setting to 2019 as midpoint of study. otherwise, it will take the most recent.
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = acs_2019_vars_sex_by_age[1],
  geometry = TRUE  #takes a long time so just do this once
) %>% 
  mutate(
    county_state_fips = GEOID,
    county_fips = str_sub(GEOID, 3,5),
    county_name = NAME,
    county_name_short = case_when(
      county_name == "Adams County, Colorado" ~ "Adams",
      county_name == "Jefferson County, Colorado" ~ "Jefferson",
      county_name == "Arapahoe County, Colorado" ~ "Arapahoe",
      county_name == "Denver County, Colorado" ~ "Denver",
      county_name == "Douglas County, Colorado" ~ "Douglas"
    )
  ) %>% 
  dplyr::select(contains("fips"), contains("county"), geometry) %>% 
  st_transform(4326)

save(den_metro_co_geo, file = "den_metro_co_geo.RData")
load("den_metro_co_geo.RData")
den_metro_co_geo %>% mapview()
#a no geo version
den_metro_co_nogeo = den_metro_co_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_metro_co_nogeo, file = "den_metro_co_nogeo.RData")

#a geo lookup
lookup_den_metro_co_fips_geo = den_metro_co_geo %>% 
  distinct(county_fips, geometry)
save(lookup_den_metro_co_fips_geo, file = "lookup_den_metro_co_fips_geo.RData")

### create versions of just denver county and just denver and jefferson counties--------
den_co_geo= den_metro_co_geo %>% 
  filter(county_fips == "031") 
save(den_co_geo, file = "den_co_geo.RData")
den_co_geo %>% mapview(zcol = "county_fips")

#denver and jefferson counties
den_jeff_co_geo = den_metro_co_geo %>% 
  filter(county_fips == "031" | county_fips == "059") 
save(den_jeff_co_geo , file = "den_jeff_co_geo.RData")

den_jeff_co_nogeo = den_jeff_co_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_jeff_co_nogeo, file = "den_jeff_co_nogeo.RData")
den_jeff_co_geo %>% mapview(zcol = "county_fips")

# #the fact that denver has a hole may be giving me some issues. try this.
# #https://stackoverflow.com/questions/52654701/removing-holes-from-polygons-in-r-sf
# #install.packages("nngeo")
# library(nngeo)
# #get a simplified boundary
# den_jeff_co_no_holes_geo = den_jeff_co_geo %>% 
#   nngeo::st_remove_holes()
# 
# den_jeff_co_no_holes_geo %>% mapview()

# census-tract-level ACS variables----------------

## download variables by census tract---------------
den_metro_tracts_vars_long  = tidycensus::get_acs(
  geography = "tract", 
  year=2019, #setting to 2019 as midpoint of study. otherwise, it will take the most recent.
#  cache_table = TRUE, #this may have been throwing an error. leave this as default.
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
#  output = "long", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = acs_2019_vars_sex_by_age,
  geometry = FALSE
) 
den_metro_tracts_vars_long

save(den_metro_tracts_vars_long, file = "den_metro_tracts_vars_long.RData")

#specify that it's long form to distinguish from the _geo version above
den_metro_tracts_long_wrangle = den_metro_tracts_vars_long %>% 
  #rename per above
  rename( var_name = variable) %>% #note because it's long form, it's called variable here
  left_join(lookup_acs_2019_var_label, by = "var_name") %>% 
  mutate(
    county_state_fips = GEOID,
    county_fips = str_sub(GEOID, 3,5)
  )  %>% 
  dplyr::select(-GEOID)   #drop geoid
den_metro_tracts_long_wrangle
setwd(here("data-processed"))
save(den_metro_tracts_long_wrangle, file = "den_metro_tracts_long_wrangle.RData")

#great, now I need a look-up to link with the GBD ages

#dissolve by county...just to be sure this works
# den_metro_tracts_long_wrangle %>% 
#   left_join(den_metro_tracts_geo, by = "GEOID") %>% 
#   st_as_sf() %>% 
#   st_transform(4326) %>% 
#   group_by(county_fips) %>% 
#   summarise(n=n()) %>% 
#   ungroup() %>% 
#   mapview(zcol = "county_fips")

#add age-stratification (5 year bins)


