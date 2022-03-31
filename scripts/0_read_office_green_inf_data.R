#This file reads the data provided by the Office of Green Infrastructure
#Began March 4 2022
#Revised Mar 30 2022
#filename: 0_read_office_green_inf_data

library(tidyverse)
library(sf)
library(mapview)
library(here)

# Notes from Colin:-------

## 1. Green Infrastructure Capital Projects - Regional----------
#Background: 
  #The Office of Green Infrastructure has a budget to design, build, establish, and monitor 
  #green infrastructure facilities which treat stormwater quality. One part of the portfolio is regional projects, 
#which are large ponds or basins located on public property that collect and treat stormwater 
#after it has been collected in a storm pipe. These are usually vegetated, very often with native/adapted landscapes.

#In 2017 the Office of Green Infrastructure produced a Green Infrastructure Implementation Strategy which 
#identified potential regional projects. These were subsequently mapped in GIS. 
#A selection of the table has been included in the attached files.

# Calculating area suggestions:
# •	The RegionalProjects.shp polygons each have a ShapeArea field based on the footprint. 
#This maps the Project boundaries, which may be larger than the actual facility boundaries. 
#It may take some aerial photo analysis and/or referring to the implementation strategy to better 
#constrain the actual footprint.
# •	We would expect ~75% of a facility footprint to have native/adapted vegetation
# •	Short term / likely program goals are the build these projects:
#   o	2020_REG_MCDONOUGH_PK
# o	2020_SUB_WBVWPARK
# o	2020_REG_WEIRIRV
# o	2020_REG_SUNK
# o	2020_REG_38THFOX
# •	Long term / aspiration program goals are to build all of the remaining projects

# Reference sites for Landsat:
#   •	38th & Holly - Wastewater parcel located to the northwest of the intersection of 38th & Holly. 
      #If you can find an image from late 2020 that may be best – there was some mis-management late in 2020 
      #that lead to vegetation death in early 2021 and it has not yet recovered
# •	Glenbrook Detention Basin - 39°37'50.9"N 105°06'12.1"W
# •	Hampden Heights Bioretention EBD – Located at the Joe Shoemaker School in Denver, 
    #at the end of Girard Ave near Cherry Creek
# •	La Lomita Park – located at Asbury and Tejon intersection. Two basins exist on either side of the street.


##  2. 	Green Infrastructure Capital Projects – Green Streets--------
# status quo goal - 2.7 miles of green streets per year, and each mile of green street 
#equates to 0.15 acres of native/vegetated landscape

#so in 5 years:
2.7*.15

# short-term goal is to increase output to 0.5 miles per year (same)

# aspirational goal is to output 5 miles per year but increase vegetated area
#to .75 acres per mile


## 3. Site redevelopment stormwater controls--------

# •	100 sites / year > 1.0 ac
# •	25 sites / year 0.5 to 1.0 ac
# •	400 sites / year < 0.5 ac and adding 3000 SF of impervious cover


# •	Stormwater footprint:impervious footprint is ~40:1 on sites >1.0 ac
# •	Stormwater footprint:impervious footprint is ~25:1 on sites 0.5-1.0 ac
# •	Stormwater footprint:impervious footprint is ~10:1 on sites <0.5 ac

#NOTE – when you distribute this area across the census blocks/tracks – 
#suggest you do NOT add any to northeast Denver. 
#They have regional stormwater control up there, and so they will be exempt from these regulations.


# Code for 1. ----------

## Load regional project data---------
#ogi for office of green infrastructure - regional projects
setwd(here("data-input"))
ogi_reg_proj  = st_read(dsn ="from-office-green-infra") %>%
  st_transform(2876) %>% #local feet
  st_make_valid() %>% 
  st_simplify() %>% #reduce the object size a bit
  mutate(
    #As elsewhere, I'm making the area variables verbose in case they get mixed with other areal units
    area_ft2_ogi_proj =  as.numeric(st_area(geometry)),
    area_ac_ogi_proj = area_ft2_ogi_proj/43560, #acres
    area_mi2_ogi_proj = area_ft2_ogi_proj/(5280**2)  ,
    #per colin, these are the short-term priority projects likely to be built:
    short_term_proj = case_when(
      PROJECT_ID == "2020_REG_MCDONOUGH_PK" ~1,
      PROJECT_ID == "2020_SUB_WBVWPARK"~1,
      PROJECT_ID == "2020_REG_WEIRIRV"~1,
      PROJECT_ID == "2020_REG_SUNK"~1,
      PROJECT_ID == "2020_REG_38THFOX"~1,
      TRUE ~0
    )
  )

table(ogi_reg_proj$PROJECT_ID)

ogi_reg_proj %>% dplyr::select(PROJECT_ID) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

setwd(here("data-processed"))
save(ogi_reg_proj, file = "ogi_reg_proj.RData")
ogi_reg_proj %>% mapview(zcol = "area_ac_ogi_proj")
ogi_reg_proj %>% mapview(zcol = "short_term_proj")

#The code continues in the HIA code. The files aren't very big,
#so it's easier to have everything there so I don't have to switch between scripts.


