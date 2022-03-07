#filename: 0_import_manage_denver_acs
#Purpose: download demographics data for Denver area (county, metro, to be defined)
#Date began 12/12/21
#Date revised: March 6 2022
library(tidyverse)
library(sf)
library(mapview)
library(tidycensus)
library(here)
setwd(here("data-processed"))

# Objective-----------
#This code accomplishes three main thigngs:
#1. Get geometry files for census tracts and counties in the Denver area.
#2. Download sex-by-age population by census tract. I do this long form in
#a somewhat slick way.
#3. Get other miscellaneous variables, including hh inc, race, med home value,
#and others. I do this long mimicking some of my old Atlanta code

# Download geometry only (no other measures)  for denver metro -----------
#for tracts and counties

#https://data-cdphe.opendata.arcgis.com/datasets/colorado-county-boundaries/
#FIPS codes
#denver 031
#jefferson 059
#douglas 035
#arapahoe 005
#adams 001
#denver FIPS code: 031
## geometry of tracts-------------
den_metro_tracts_geo  = tidycensus::get_acs(
  geography = "tract", 
  year=2019,  
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = "B01001_001",
  geometry = TRUE  #takes a long time so just do this once
) %>% 
  #some light renaming, etc.
  mutate(
    tract_fips = str_sub(GEOID, 1,11),#this always works
    county_fips = str_sub(GEOID, 3,5)
  ) %>% 
  dplyr::select(contains("fips"), geometry)%>% 
  st_transform(4326)

save(den_metro_tracts_geo, file = "den_metro_tracts_geo.RData")

#a version restricted to just denver and jefferson counties (but still at tract level)
load("den_metro_tracts_geo.RData")
den_metro_tracts_geo %>% mapview()
den_jeff_co_tracts_geo = den_metro_tracts_geo %>% 
  filter(county_fips == "031" | county_fips == "059") 
save(den_jeff_co_tracts_geo, file = "den_jeff_co_tracts_geo.RData")
den_jeff_co_tracts_geo %>% mapview(zcol = "county_fips")  

#and a no-geo version
den_jeff_co_tracts_nogeo = den_jeff_co_tracts_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_jeff_co_tracts_nogeo, file = "den_jeff_co_tracts_nogeo.RData")

## geometry of block groups--------
den_metro_bg_geo  = tidycensus::get_acs( #bg for block group
  geography = "block group", 
  year=2019,  
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = "B01001_001",
  geometry = TRUE  #takes a long time so just do this once
) %>% 
  #some light renaming, etc.
  mutate(
    bg_fips = GEOID ,
    county_fips = str_sub(GEOID, 3,5)
  ) %>% 
  dplyr::select(contains("fips"), geometry)%>% 
  st_transform(4326)


save(den_metro_bg_geo, file = "den_metro_bg_geo.RData")
den_metro_bg_geo %>% mapview()

##  geometry of counties--------------
den_metro_co_geo  = tidycensus::get_acs(
  geography = "county", 
  year=2019,  
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = "B01001_001",
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


# look-up tables for tracts, counties, and county names--------
#a geo lookup
lookup_den_metro_co_fips_geo = den_metro_co_geo %>% 
  distinct(county_fips, geometry)
save(lookup_den_metro_co_fips_geo, file = "lookup_den_metro_co_fips_geo.RData")
#county name look up denver metro
lookup_county_name_den_metro = den_metro_co_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(county_fips, county_name, county_name_short)

#tract-county lookup
lookup_den_metro_tract_county = den_metro_tracts_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(tract_fips, county_fips)

#block-group-county-lookup
lookup_den_metro_bg_county = den_metro_bg_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, county_fips)

## geometry for denver county and jefferson county--------
den_co_geo= den_metro_co_geo %>% 
  filter(county_fips == "031") 
save(den_co_geo, file = "den_co_geo.RData")
den_co_geo %>% mapview(zcol = "county_fips")

#denver and jefferson counties
load("den_metro_co_geo.RData")
den_jeff_co_geo = den_metro_co_geo %>% 
  filter(county_fips == "031" | county_fips == "059") 
save(den_jeff_co_geo , file = "den_jeff_co_geo.RData")

den_jeff_co_nogeo = den_jeff_co_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_jeff_co_nogeo, file = "den_jeff_co_nogeo.RData")
den_jeff_co_geo %>% mapview(zcol = "county_fips")


# Download ACS variables (not just geometry)------
## census-tract-level ACS variables----------------

## wrangle the ACS sex-by-age variables in prep for the long-form data-----------
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
  #rename first so it's var_name and var_label. 
  #less ambiguous with, e.g., census tract name
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

## download sex by age vars long form----------
#this will be long form. easier to link with health information.
den_metro_tracts_vars_s_by_a_long  = tidycensus::get_acs( #s by a = sex by age
  geography = "tract", 
  year=2019, 
#  cache_table = TRUE, #this may have been throwing an error. leave this as default.
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
#  output = "long", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = acs_2019_vars_sex_by_age,
  geometry = FALSE
) 
den_metro_tracts_vars_s_by_a_long

save(den_metro_tracts_vars_s_by_a_long, file = "den_metro_tracts_vars_s_by_a_long.RData")

#specify that it's long form to distinguish from the _geo version above
den_metro_tracts_long_s_by_a_wrangle = den_metro_tracts_vars_s_by_a_long %>% 
  #rename per above
  rename( var_name = variable) %>% #note because it's long form, it's called variable here
  left_join(lookup_acs_2019_var_label, by = "var_name") %>% 
  mutate(
    tract_fips = GEOID, #we're at the tract level
    county_fips = str_sub(GEOID, 3,5)
  )  %>% 
  dplyr::select(-GEOID)   #drop geoid

den_metro_tracts_long_s_by_a_wrangle
setwd(here("data-processed"))
save(den_metro_tracts_long_s_by_a_wrangle, 
     file = "den_metro_tracts_long_s_by_a_wrangle.RData")

## By tract, download other socio-demographic variables (wide form) -------
#race, income, etc.
#define wide-form variables here because I'm using this twice
acs_vars_sociodem = c(

  #race (dichotomizing as white or else)
  #add an underscore to all of these, because the package automatically adds E and M
  #to the end of each var
  pop_tot_ = "B01003_001",
  race_tot_ = "B02001_001",
  race_w_ = "B02001_002",
  race_b_ = "B02001_003",
  
  #median home value (use h_val to shorten)
  h_val_med_ = "B25077_001",
  
  #median household income (take the continuous value instead of categories)
  #12/11/21 changing from hh_inc_ to hh_inc. shorten words...
  hh_inc_med_ = "B19013_001",
  
  #median age
  age_med_ = "B01002_001")

den_tracts_acs5_20152019_nogeo  = get_acs(
  geography = "tract", 
  year=2019,  
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide",
  survey = "acs5", 
  geometry = FALSE, #you already have them saved
  variables = acs_vars_sociodem 
  ) 

#wrangle these wide-form data
den_metro_tracts_geo %>% mapview()
st_crs(den_metro_tracts_geo)
#some operations that I perform to both the tract-level and the block-group-level
#data, so make a function 
tract_and_bg_wrangle <-function(df){
  df %>% 
    #rename estimate and margin of error to something more meaningful
    #not sure what the ~ means or why we need .x but asi es
    dplyr::rename_with( ~gsub("_E", "", .x, fixed = TRUE)) %>% 
    dplyr::rename_with(~gsub("_M", "_moe", .x, fixed = TRUE))  %>% 
    mutate(
      #calculate sf stuff first.
      #I had called these, e.g., tract_area, but this way I can leave it more 
      #general. you know the unit from other clues.
      area_ft2 = as.numeric(st_area(geometry)),
      area_mi2 = area_ft2*3.58701e-8  ,
      pop_dens_mi2 = pop_tot/area_mi2 ,
      #prop for proportion
      race_w_prop = race_w/race_tot,  
      race_b_prop = race_b/race_tot,
      race_nw_prop = 1-race_w_prop, #nw for nonwhite (i.e, 1-white)
      race_o_prop = 1-race_b_prop - race_w_prop
      ) 
}
    

den_tracts_acs5_20152019_wrangle_geo = den_tracts_acs5_20152019_nogeo %>%
  mutate(
    tract_fips = GEOID  #we're at the tract level
  )  %>% 
  dplyr::select(-GEOID) %>%    #drop geoid
  left_join(den_metro_tracts_geo, by = "tract_fips") %>%   #add geometry
  st_as_sf() %>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  left_join(lookup_county_name_den_metro, by = "county_fips") %>% 
  tract_and_bg_wrangle()


den_tracts_acs5_20152019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "hh_inc_med")

## By block group, download other socio-demographic variables (wide form) -------
den_bg_acs5_20152019_nogeo  = get_acs(
  geography = "block group", 
  year=2019,  
  cache_table = TRUE,
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  output = "wide",
  survey = "acs5", 
  geometry = FALSE, #you already have them saved
  variables = acs_vars_sociodem 
) 

den_bg_acs5_20152019_wrangle_geo = den_bg_acs5_20152019_nogeo %>%
  mutate(
    bg_fips = GEOID  #we're at the tract level
    #don't need county information because we can link it in with the geo file
  )  %>% 
  dplyr::select(-GEOID) %>%    #drop geoid
  left_join(den_metro_bg_geo, by = "bg_fips") %>%   #add geometry
  st_as_sf() %>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  left_join(lookup_county_name_den_metro, by = "county_fips") %>% 
  tract_and_bg_wrangle()

save(den_bg_acs5_20152019_wrangle_geo, 
     file = "den_bg_acs5_20152019_wrangle_geo.RData")
  
View(den_bg_acs5_20152019_wrangle_geo)
den_bg_acs5_20152019_wrangle_geo %>% mapview(zcol = "hh_inc_med")
den_bg_acs5_20152019_wrangle_geo %>% mapview(zcol = "race_nw_prop")