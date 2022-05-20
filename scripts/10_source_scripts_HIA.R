#source scripts in this order
#filename: 10_source_scripts_HIA
#I like to call these source scripts 10 in case other stuff gets filled in
#4/16/22 I loaded the new 2020 ACS data, and I think there were new block-group definitions.

library(here)
library(tidyverse)

source(here("scripts", "0_import_manage_denver_acs.R"))#I reran when the 2020 ACS was released
source(here("scripts", "0_read_den_nbhd.R")) #does very little beyond import neighborhoods
source(here("scripts", "1a_get_landsat_ndvi_denver.R")) #don't need to run very often; will be very slow
source(here("scripts", "0_read_equity_indices.R")) #run this if you change the way equity indices are categorized / defined
source(here("scripts", "0_read_denver_native_zones.R")) #

source(here("scripts", "1_remove_water_tract_bg_park.R")) #if ACS data is re-run, this should be, too

#will take a while (~10 min) but not as long as it used to, because now I'm not extracting the NDVI of every census tract
#in the full Denver Metro area
source(here("scripts", "2_ndvi_tract_green_space_native_zones.R"))

source(here("scripts", "3_HIA_for_each_scenario.R")) #run the HIA
source(here("scripts", "3a_HIA_for_each_scenario_boot.R")) #boot the HIA


#confirm the rmarkdown scripts work to render html
rmarkdown::render(here("docs", "ndvi-of-places-tracts.Rmd"))
rmarkdown::render(here("docs", "hia-results.Rmd"))
