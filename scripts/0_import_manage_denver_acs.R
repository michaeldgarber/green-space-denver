#filename: 0_import_manage_denver_acs
#Purpose: download demographics data for Denver area (county, metro, to be defined)
#Date began 12/12/21
#Date revised: March 14 2022
library(tidyverse)
library(sf)
library(mapview)
library(tidycensus)
library(here)
setwd(here("data-processed"))

# Objective-----------
#This code accomplishes three main things:
#1. Get geometry files for census tract and counties in the Denver area.
#2. Download sex-by-age population by census tract. I do this long form in
#a somewhat slick way.
#3. Get other miscellaneous variables, including hh inc, race, med home value,
#and others. I do this long mimicking some of my old Atlanta code

# Download geometry only (no other measures)  for denver metro -----------
#for tract and counties

#https://data-cdphe.opendata.arcgis.com/datasets/colorado-county-boundaries/
#FIPS codes
#denver 031
#jefferson 059
#douglas 035
#arapahoe 005
#adams 001
#denver FIPS code: 031
## geometry of tract-------------


#tracts  
den_metro_tract_geo  = tidycensus::get_acs(
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
    admin_unit = "tract",
    tract_fips = str_sub(GEOID, 1,11),#this always works
    county_fips = str_sub(GEOID, 3,5)
  ) %>% 
  dplyr::select(contains("fips"), admin_unit, geometry)%>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  #measure area here as well
  mutate(
    #3/24 decision to use the _tract prefix rather than leave general.
    #less confusing when link with other polygons which may have different
    #areas
    area_ft2_tract = as.numeric(st_area(geometry)),
    area_m2_tract = area_ft2_tract/10.764, #meters squared, for informational purposes.
    area_mi2_tract = area_ft2_tract/(5280**2) #miles squared
  )

den_metro_tract_geo %>% mapview(zcol = "area_mi2_tract")
setwd(here("data-processed"))
save(den_metro_tract_geo, file = "den_metro_tract_geo.RData")
#a version restricted to just denver and jefferson counties (but still at tract level)
load("den_metro_tract_geo.RData")
den_metro_tract_geo %>% mapview()
den_jeff_co_tract_geo = den_metro_tract_geo %>% 
  filter(county_fips == "031" | county_fips == "059") 
save(den_jeff_co_tract_geo, file = "den_jeff_co_tract_geo.RData")
den_jeff_co_tract_geo %>% mapview(zcol = "county_fips")  

#and a no-geo version
den_jeff_co_tract_nogeo = den_jeff_co_tract_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_jeff_co_tract_nogeo, file = "den_jeff_co_tract_nogeo.RData")

#restricted to Denver only
den_co_tract_geo = den_metro_tract_geo %>% 
  filter(county_fips == "031" )
setwd(here("data-processed"))
save(den_co_tract_geo, file = "den_co_tract_geo.RData")

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
    admin_unit = "block group",
    bg_fips = str_sub(GEOID, 1,12),
    tract_fips = str_sub(GEOID, 1,11),#this always works
    county_fips = str_sub(GEOID, 3,5)
  ) %>% 
  dplyr::select(contains("fips"), admin_unit, geometry)%>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  mutate(
    area_ft2_bg = as.numeric(st_area(geometry)),
    area_m2_bg = area_ft2_bg/10.764, #meters squared, for informational purposes.
    area_mi2_bg = area_ft2_bg/(5280**2) #miles squared
  )


save(den_metro_bg_geo, file = "den_metro_bg_geo.RData")
den_metro_bg_geo %>% mapview(zcol = "area_mi2_bg")
load("den_metro_bg_geo.RData")
nchar(den_metro_bg_geo$bg_fips)#12 characters
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
    admin_unit = "county",
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
  dplyr::select(contains("fips"), admin_unit, 
                contains("county"), geometry) %>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  mutate(
    #co for county
    area_ft2_co = as.numeric(st_area(geometry)),
    area_m2_co = area_ft2_co/10.764, #meters squared, for informational purposes.
    area_mi2_co = area_ft2_co/(5280**2) ,#miles squared
  )

save(den_metro_co_geo, file = "den_metro_co_geo.RData")
load("den_metro_co_geo.RData")
den_metro_co_geo %>% mapview(zcol = "area_mi2_co")
den_metro_co_nogeo = den_metro_co_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_metro_co_nogeo, file = "den_metro_co_nogeo.RData")


# look-up tables for tract, counties, and county names--------
setwd(here("data-processed"))

## geo lookups--------
load("den_metro_co_geo.RData")
### county geo lookup------------
lookup_den_metro_co_fips_geo = den_metro_co_geo %>% 
  distinct(county_fips, geometry)%>% 
  as_tibble()
save(lookup_den_metro_co_fips_geo, file = "lookup_den_metro_co_fips_geo.RData")

### tract geo lookup----------
lookup_den_metro_tract_geo = den_metro_tract_geo %>% 
  distinct(tract_fips, geometry)
save(lookup_den_metro_tract_geo, file = "lookup_den_metro_tract_geo.RData")
### bg geo lookup----------
lookup_den_metro_bg_geo = den_metro_bg_geo %>% 
  distinct(bg_fips, geometry)
save(lookup_den_metro_bg_geo, file = "lookup_den_metro_bg_geo.RData")

#county name look up denver metro
lookup_county_name_den_metro = den_metro_co_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(county_fips, county_name, county_name_short)%>% 
  as_tibble()

## tract-county lookup-------
load("den_metro_tract_geo.RData")
lookup_den_metro_tract_county = den_metro_tract_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(tract_fips, county_fips)%>% 
  as_tibble()

## block-group-county-lookup-------
lookup_den_metro_bg_county = den_metro_bg_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, county_fips) %>% 
  as_tibble()
save(lookup_den_metro_bg_county, file = "lookup_den_metro_bg_county.RData")

lookup_den_metro_bg_tract = den_metro_bg_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, tract_fips) %>% 
  as_tibble()
save(lookup_den_metro_bg_tract, file = "lookup_den_metro_bg_tract.RData")

## area measurements for tracts and block groups--------
lookup_tract_area = den_metro_tract_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(tract_fips, area_ft2_tract, area_m2_tract, area_mi2_tract) %>% 
  as_tibble()
save(lookup_tract_area, file = "lookup_tract_area.RData")
nrow(lookup_tract_area)
nrow(den_metro_tract_geo)

lookup_bg_area = den_metro_bg_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_ft2_bg, area_m2_bg, area_mi2_bg) %>% 
  as_tibble()
save(lookup_bg_area, file = "lookup_bg_area.RData")

#the same for those without water are located in
#the script that creates them:
#~/2_ndvi_tract_bg_park_den.R

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


# Download & manage ACS variables (not just geometry)------

## Long form (age/sex)-------
#note this should work regardless of geometry (i.e., tract or block group)
options(tigris_use_cache = TRUE)
library(tidycensus)
acs_2019_vars <- load_variables(2019, "acs5", cache = TRUE)
save(acs_2019_vars, file = "acs_2019_vars.RData") #takes a while so save.

#make it a tibble so you can select just the vars that begin with B01001
acs_2019_var_tib_sex_age = acs_2019_vars %>% 
  as_tibble() %>%
  #could also do the grepl framework but this is more straightforward
  mutate(
    sex_age = case_when(
      concept == "SEX BY AGE" ~ 1,
      TRUE ~ 0
  )) %>% 
  filter(sex_age==1)

#now grab the vars as a vector for use in the get_acs() function
acs_2019_vars_sex_age = acs_2019_var_tib_sex_age %>% 
  dplyr::select(name) %>% 
  pull()

#s_by_a = sex by age
lookup_acs_2019_var_sex_age_label = acs_2019_var_tib_sex_age %>% 
  #rename first so it's var_name and var_label. 
  #less ambiguous with, e.g., census tract name
  rename(
    var_name = name,
    var_label = label
  ) %>% 
  distinct(var_name, var_label) %>% 
  mutate(
    sex = case_when(   #sex indicator
      #(grepl("needle", haystack))
      #changing to lowercase. personal preference.
      grepl("Male", var_label) ~ "male",
      grepl("Female", var_label) ~ "female",
      var_label == "Estimate!!Total:" ~"all"
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
      grepl("65 and", var_label) ~ "65-66",
      grepl("67 to", var_label) ~ "67-69",
      grepl("70 to", var_label) ~ "70-74",
      grepl("75 to", var_label) ~ "75-79",
      grepl("80 to", var_label) ~ "80-84",
      grepl("85 years", var_label) ~ "85+",
      var_label == "Estimate!!Total:" ~ "all",
      var_label == "Estimate!!Total:!!Female:" ~ "all", #all ages
      var_label == "Estimate!!Total:!!Male:" ~ "all"  #all ages
    ),
    
    #this is the same grouping as above, but I want to make it numeric for easier
    #categorization later, so:
    #lb for lower bound
    age_group_acs_lb = case_when(
      grepl("Under 5", var_label) ~ 0,
      grepl("5 to 9", var_label) ~ 5,
      grepl("10 to 14", var_label) ~ 10,
      grepl("15 to 17", var_label) ~ 15,
      grepl("18 and", var_label) ~ 18,
      grepl("20 y", var_label) ~ 20,
      grepl("21 y", var_label) ~ 21,
      grepl("22 to", var_label) ~ 22,
      grepl("25 to", var_label) ~ 25,
      grepl("30 to", var_label) ~ 30,
      grepl("35 to", var_label) ~ 35,
      grepl("40 to", var_label) ~ 40,
      grepl("45 to", var_label) ~ 45,
      grepl("50 to", var_label) ~ 50,
      grepl("55 to", var_label) ~ 55,
      grepl("60 and", var_label) ~ 60,
      grepl("62 to", var_label) ~ 62,
      grepl("65 a", var_label) ~ 65,
      grepl("67 to", var_label) ~ 67,
      grepl("70 to", var_label) ~ 70,
      grepl("75 to", var_label) ~ 75,
      grepl("80 to", var_label) ~ 80,
      grepl("85 years", var_label) ~ 85
    ),
    
    #Many of the GBD rates are in bigger groups.
    #Create a few options for collapsed adult age groups
    age_group_acs_18_plus = case_when(
      age_group_acs_lb>=18 ~1,
      TRUE ~0),
    age_group_acs_20_plus = case_when(
      age_group_acs_lb>=20 ~1,
      TRUE ~0),
    age_group_acs_25_plus = case_when(
      age_group_acs_lb>=25 ~1,
      TRUE ~0),
    age_group_acs_30_plus = case_when(
      age_group_acs_lb>=30 ~1,
      TRUE ~0)
    )


setwd(here("data-processed"))
save(lookup_acs_2019_var_sex_age_label, 
     file = "lookup_acs_2019_var_sex_age_label.RData")

#Put this into Excel, so I can create a look-up table there between
#this and the GBD data
library(writexl)
setwd(here("data-processed"))
writexl::write_xlsx(
  lookup_acs_2019_var_sex_age_label,
  "lookup_acs_2019_var_sex_age_label.xlsx"
                    )

table(lookup_acs_2019_var_sex_age_label$age_group_acs) #good
table(lookup_acs_2019_var_sex_age_label$var_label)
table(lookup_acs_2019_var_sex_age_label$age_group_acs,
 lookup_acs_2019_var_sex_age_label$age_group_acs_18_plus) 


### tracts: dl sex by age long form------------
#this will be long form. easier to link with health information.
den_metro_tract_sex_age  = tidycensus::get_acs( #s by a = sex by age
  geography = "tract", 
  year=2019, 
#  cache_table = TRUE, #this may have been throwing an error. leave this as default.
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
#  output = "long", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = acs_2019_vars_sex_age,
  geometry = FALSE
) 
den_metro_tract_sex_age

save(den_metro_tract_sex_age, 
     file = "den_metro_tract_sex_age.RData")

### wrangle long-form (age/sex) tracts---------
#in the object name, specify that it's long form to distinguish from the 
#_geo version above
den_metro_tract_sex_age_wrangle = den_metro_tract_sex_age %>% 
  rename( var_name = variable) %>% #note because it's long form, it's called variable here
  left_join(lookup_acs_2019_var_sex_age_label, by = "var_name") %>% #brings in lots
  mutate(
    tract_fips = str_sub(GEOID, 1,11),#this always works
    county_fips = str_sub(GEOID, 3,5)
  )  %>% 
  dplyr::select( #drop geoid and name. we have it via the fips codes
    -GEOID, -NAME  
    ) %>%  
  #rename the estimate to explicitly be population,
  rename(pop = estimate, #this is the population estimate in that age-sex group
         pop_moe = moe) %>% 
  dplyr::select(contains("fips"), everything()) %>% 
  #link area measurements to calculate population density
  left_join(lookup_tract_area, by = "tract_fips") %>% 
  mutate( pop_dens_mi2 = pop/area_mi2_tract )#changed 3/24/22

setwd(here("data-processed"))
save(den_metro_tract_sex_age_wrangle , 
     file = "den_metro_tract_sex_age_wrangle .RData")

### tracts: summarize age group categories for later linking------
#note eventually we should probably consider the moe here
names(den_metro_tract_sex_age_wrangle)

den_metro_tract_18_plus  = den_metro_tract_sex_age_wrangle %>% 
  group_by(tract_fips, age_group_acs_18_plus) %>% 
  summarise(pop_age_18_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_18_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

den_metro_tract_20_plus  = den_metro_tract_sex_age_wrangle %>% 
  group_by(tract_fips, age_group_acs_20_plus) %>% 
  summarise(pop_age_20_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_20_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

den_metro_tract_25_plus  = den_metro_tract_sex_age_wrangle %>% 
  group_by(tract_fips, age_group_acs_25_plus) %>% 
  summarise(pop_age_25_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_25_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

den_metro_tract_30_plus  = den_metro_tract_sex_age_wrangle %>% 
  group_by(tract_fips, age_group_acs_30_plus) %>% 
  summarise(pop_age_30_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_30_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

###  bg: dl sex by age long form--------
#this will be long form. easier to link with health information.
den_metro_bg_sex_age  = tidycensus::get_acs( #s by a = sex by age
  geography = "block group", 
  year=2019, 
  #  cache_table = TRUE, #this may have been throwing an error. 
  #leave this as default.
  state = "CO",
  county =  c("031", "059", "035", "005", "001"), 
  keep_geo_vars = FALSE, 
  #  output = "long", #keep it long form. easier to link with GBD data
  survey = "acs5", 
  variables = acs_2019_vars_sex_age,
  geometry = FALSE
) 
den_metro_bg_sex_age

save(den_metro_bg_sex_age, 
     file = "den_metro_bg_sex_age.RData")

### wrangle long-form (age/sex) bg---------
den_metro_bg_sex_age_wrangle = den_metro_bg_sex_age %>% 
  #note because it's long form, it's called variable here
  rename( var_name = variable) %>% 
  left_join(lookup_acs_2019_var_sex_age_label, by = "var_name") %>% 
  mutate(
    bg_fips = str_sub(GEOID, 1,12),
    tract_fips = str_sub(GEOID, 1,11), 
    county_fips = str_sub(GEOID, 3,5)
  )  %>% 
  #drop geoid and name. we have it via the fips codes
  dplyr::select(-GEOID, -NAME) %>%  
  #rename the estimate to explicitly be population,
  rename(pop = estimate, #this is the population estimate in that age-sex group
         pop_moe = moe) %>% 
  dplyr::select(contains("fips"), everything()) %>% 
  #link area measurements to calculate population density
  left_join(lookup_bg_area, by = "bg_fips") %>% 
  mutate( pop_dens_mi2 = pop/area_mi2_bg )
  
  
den_metro_bg_sex_age_wrangle 
setwd(here("data-processed"))
save(den_metro_bg_sex_age_wrangle , 
     file = "den_metro_bg_sex_age_wrangle.RData")

### block group: summarize age group categories for later linking------
#note eventually we should probably consider the moe here
names(den_metro_tract_sex_age_wrangle)

den_metro_bg_18_plus  = den_metro_bg_sex_age_wrangle %>% 
  group_by(bg_fips, age_group_acs_18_plus) %>% 
  summarise(pop_age_18_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_18_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

den_metro_bg_20_plus  = den_metro_bg_sex_age_wrangle %>% 
  group_by(bg_fips, age_group_acs_20_plus) %>% 
  summarise(pop_age_20_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_20_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

den_metro_bg_25_plus  = den_metro_bg_sex_age_wrangle %>% 
  group_by(bg_fips, age_group_acs_25_plus) %>% 
  summarise(pop_age_25_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_25_plus==1) %>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

den_metro_bg_30_plus  = den_metro_bg_sex_age_wrangle %>% 
  group_by(bg_fips, age_group_acs_30_plus) %>% 
  summarise(pop_age_30_plus = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(age_group_acs_30_plus==1)%>% 
  dplyr::select(-starts_with("age_group")) #remove so it doesn't link in.

## Wide-form ---------
## By tract, download other socio-demographic variables (wide form) -------
### Define variables to be pulled--------
#race, income, etc.
#define wide-form variables here because I'm using this twice
acs_vars_sociodem = c(

  #race (dichotomizing as white or else)
  #add an underscore to all of these, because the package automatically adds E and M
  #to the end of each var
  #note I updated this to include hispanic/latino categories
  pop_tot_ = "B01003_001",
  #I was using B02001_001 but use this instead, as it includes hispanic/latino  
  race_tot_ = "B03002_001", 
  race_w_ = "B03002_003", #white and not hispanic or latino
  race_h_ = "B03002_012",#hispanic or latino total
  race_b_ = "B03002_004", #black, not hispanic or latino
  
  #median home value (use h_val to shorten)
  #and poverty in last 12 months
  h_val_med_ = "B25077_001",
  pov_last_12_ = "B17001_002",
  pov_last_12_tot_ = "B17001_001",
  
  #median household income (take the continuous value instead of categories)
  #12/11/21 changing from hh_inc_ to hh_inc. shorten words...
  hh_inc_med_ = "B19013_001",
  
  #edu for education. high school graduate or more. this column should work.
  edu_hs_tot_  = "B16010_001",    # total in this column (at least high school)
  edu_lt_hs_ = "B16010_002", #less than (not including) high school,
  edu_hs_ = "B16010_015", # high school graduate (includes equivalency)
  
  #housing cost burdened
  #B25070 for rent; B25091 for mortgage
  #https://www.arcgis.com/home/item.html?id=9c7647840d6540e4864d205bac505027

  #median age
  age_med_ = "B01002_001")

den_tract_acs5_2019_nogeo  = get_acs(
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
setwd(here("data-processed"))
save(den_tract_acs5_2019_nogeo, file = "den_tract_acs5_2019_nogeo.RData")
#wrangle these wide-form data
den_metro_tract_geo %>% mapview()
st_crs(den_metro_tract_geo)
#some operations that I perform to both the tract-level and the block-group-level
#data, so make a function 

rename_prefixes<-function(df){
  df %>% 
    #rename estimate and margin of error to something more meaningful
    #not sure what the ~ means or why we need .x but asi es
    dplyr::rename_with( ~gsub("_E", "", .x, fixed = TRUE)) %>% 
    dplyr::rename_with(~gsub("_M", "_moe", .x, fixed = TRUE))
}

tract_and_bg_wrangle <-function(df){
  df %>% 
    mutate(
      #prop for proportion
      race_w_prop = race_w/race_tot,  
      race_b_prop = race_b/race_tot,
      race_h_prop = race_h/race_tot, 
      race_nw_prop = 1-race_w_prop, #nw for nonwhite (i.e, 1-white)
      race_o_prop = 1-race_b_prop - race_w_prop,
      
      pov_last_12_prop = pov_last_12/pov_last_12_tot, #percent poverty
      
      edu_hs_or_less = edu_lt_hs + edu_lt_hs, #add these up
      edu_hs_or_less_prop = edu_hs_or_less/edu_hs_tot, #percent high school edu or less
      edu_lt_hs_prop = edu_lt_hs/edu_hs_tot, #proportion less than hs.
      #per the definition of disproportionately impacted in
      #https://leg.colorado.gov/sites/default/files/2021a_1266_signed.pdf
      #at least 40% in poverty or at least 40% non-white
      pov_last_12_prop_40plus = case_when(
        pov_last_12_prop > 0.4 ~ 1,
        pov_last_12_prop <= 0.4 ~ 0 #hopefully creates NAs as appropriate
      ),
      
      race_nw_prop_40plus = case_when(
        race_nw_prop > 0.4 ~ 1,
        race_nw_prop <= 0.4 ~ 0
      ),
      #note the definition is EITHER of the two 
      pov_or_race_nw_40plus = case_when(
        #if either ...1
        pov_last_12_prop_40plus==1 |
          race_nw_prop_40plus == 1 ~ 1,
        
        #complement: if both are 0, then 0.
        #everything else should be missing.
        pov_last_12_prop_40plus==0 &
          race_nw_prop_40plus==0 ~0
      )
      ) %>% 
    #reorder the variables for convenient mapviewing
    dplyr::select(contains("fips"), 
                  starts_with("pop"), 
                  starts_with("area"), 
                  starts_with("age"),
                  starts_with("pov"),
                  starts_with("hh_inc"), 
                  starts_with("edu"), 
                  starts_with("race"), 
                  starts_with("h_val"), 
                  everything()) %>% 
    #drop NAME
    dplyr::select(-NAME)
}

lookup_tract_area
lookup_county_name_den_metro
den_metro_tract_geo

### Wide-form tract wrangling----------
den_tract_acs5_2019_wrangle_geo = den_tract_acs5_2019_nogeo %>%
  mutate(
    tract_fips = GEOID  #we're at the tract level
  )  %>% 
  dplyr::select(-GEOID) %>%    #drop geoid
  #note this already has the area measurements as well.
  left_join(den_metro_tract_geo, by = "tract_fips") %>%   
  st_as_sf() %>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  left_join(lookup_county_name_den_metro, by = "county_fips") %>% 
  #link in the age-group totals
  left_join(den_metro_tract_18_plus, by = "tract_fips") %>% 
  left_join(den_metro_tract_20_plus, by = "tract_fips") %>% 
  left_join(den_metro_tract_25_plus, by = "tract_fips") %>% 
  left_join(den_metro_tract_30_plus, by = "tract_fips") %>% 
  rename_prefixes() %>% 
  mutate(
      #population density for each age category
    pop_dens_mi2_all = pop_tot/area_mi2_tract ,
    pop_dens_mi2_age_18_plus =   pop_age_18_plus/area_mi2_tract ,
    pop_dens_mi2_age_20_plus =   pop_age_20_plus/area_mi2_tract ,
    pop_dens_mi2_age_25_plus =   pop_age_25_plus/area_mi2_tract ,
    pop_dens_mi2_age_30_plus =   pop_age_30_plus/area_mi2_tract ) %>% 
  tract_and_bg_wrangle()  #the remaining wrangling

save(den_tract_acs5_2019_wrangle_geo, 
     file = "den_tract_acs5_2019_wrangle_geo.RData")
names(den_tract_acs5_2019_wrangle_geo)
den_tract_acs5_2019_wrangle_geo %>% mapview(zcol = "pop_dens_mi2_all")

#make a no-geo version for linking
den_tract_acs5_2019_wrangle_nogeo=den_tract_acs5_2019_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_tract_acs5_2019_wrangle_nogeo, file = "den_tract_acs5_2019_wrangle_nogeo.RData")
#checks
den_tract_acs5_2019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "hh_inc_med")
den_tract_acs5_2019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "pov_last_12_prop")
den_tract_acs5_2019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "race_nw_prop")
den_tract_acs5_2019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "pov_or_race_nw_40plus")
den_tract_acs5_2019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "edu_hs_or_less_prop")
den_tract_acs5_2019_wrangle_geo %>% 
  filter(county_name_short=="Denver") %>% 
  mapview(zcol = "edu_lt_hs_prop")


den_tract_acs5_2019_wrangle_geo %>% 
  ggplot(aes(edu_hs_or_less_prop)) + geom_histogram()
den_tract_acs5_2019_wrangle_geo %>% 
  ggplot(aes(edu_lt_hs_prop)) + geom_histogram()


## By block group, download other socio-demographic variables (wide form) -------
den_bg_acs5_2019_nogeo  = get_acs(
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

#note poverty and education aren't getting pulled in. use tracts for equity instead
save(den_bg_acs5_2019_nogeo, file = "den_bg_acs5_2019_nogeo.RData")

### Wide-form block group wrangling-------------
lookup_bg_area
lookup_county_name_den_metro
den_metro_bg_geo
den_bg_acs5_2019_wrangle_geo = den_bg_acs5_2019_nogeo %>%
  mutate(
    bg_fips = GEOID  #we're at the tract level
    #don't need county information because we can link it in with the geo file
  )  %>% 
  dplyr::select(-GEOID) %>%    #drop geoid
  left_join(den_metro_bg_geo, by = "bg_fips") %>%   #add geometry
  st_as_sf() %>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  left_join(lookup_county_name_den_metro, by = "county_fips") %>% 
  #link in the age-group totals
  left_join(den_metro_bg_18_plus, by = "bg_fips") %>% 
  left_join(den_metro_bg_20_plus, by = "bg_fips") %>% 
  left_join(den_metro_bg_25_plus, by = "bg_fips") %>% 
  left_join(den_metro_bg_30_plus, by = "bg_fips") %>% 
  rename_prefixes() %>% 
  mutate(
    #population density for each age category
    pop_dens_mi2_all = pop_tot/area_mi2_bg ,
    pop_dens_mi2_age_18_plus =   pop_age_18_plus/area_mi2_bg ,
    pop_dens_mi2_age_20_plus =   pop_age_20_plus/area_mi2_bg ,
    pop_dens_mi2_age_25_plus =   pop_age_25_plus/area_mi2_bg ,
    pop_dens_mi2_age_30_plus =   pop_age_30_plus/area_mi2_bg ) %>% 
  tract_and_bg_wrangle()  #the remaining wrangling

save(den_bg_acs5_2019_wrangle_geo, 
     file = "den_bg_acs5_2019_wrangle_geo.RData")

#a no-geo version
den_bg_acs5_2019_wrangle_nogeo = den_bg_acs5_2019_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_bg_acs5_2019_wrangle_nogeo, file = "den_bg_acs5_2019_wrangle_nogeo.RData")  
names(den_bg_acs5_2019_wrangle_nogeo)
den_bg_acs5_2019_wrangle_geo %>% mapview(zcol = "hh_inc_med")
den_bg_acs5_2019_wrangle_geo %>% mapview(zcol = "race_nw_prop")
den_bg_acs5_2019_wrangle_geo %>% mapview(zcol = "pov_last_12_prop")
den_bg_acs5_2019_wrangle_geo %>% mapview(zcol = "pov_or_race_nw_40plus")
den_bg_acs5_2019_wrangle_geo %>% 
  filter(  county_name_short == "Denver") %>% 
  mapview(zcol = "pop_dens_mi2_all")
