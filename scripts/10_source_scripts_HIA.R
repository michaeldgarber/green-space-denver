#source scripts in this order
#filename: 10_source_scripts_HIA
#I like to call these source scripts 10 in case other stuff gets filled in
#4/16/22 I loaded the new 2020 ACS data, and I think there were new block-group definitions.

library(here)
library(tidyverse)

~/Dropbox/Work/CSU/green-space-denver/scripts/1a_get_landsat_ndvi_denver.R #don't need to run very often; will be very slow
~/Dropbox/Work/CSU/green-space-denver/scripts/0_import_manage_denver_acs.R #I reran when the 2020 ACS was released

~/Dropbox/Work/CSU/green-space-denver/scripts/1_remove_water_tract_bg_park.R #if ACS data is re-run, this should be, too
~/Dropbox/Work/CSU/green-space-denver/scripts/2_ndvi_tract_bg_park_den.R 

~/Dropbox/Work/CSU/green-space-denver/scripts/3_HIA_for_each_scenario.R