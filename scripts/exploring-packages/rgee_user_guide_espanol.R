#filename: rgee_user_guide_espanol
#following here: https://ambarja.github.io/Handbook_rgee/pdf/vol01.pdf
#Revised 12/27/21

library(mapview)
library(tidyverse)
library(sf)
library(raster)
library(rgee)
library(here)
#run this script, which initializes. I thought I could just run rgee::ee_initialize, but that doesn't seem to work.
#only run every time you restart
source(here("exploring-packages", "scripts", "install-rgee-r-google-earth-engine.R"))
# rgee::ee_Initialize(drive=T) #doesn't seem to work. run the above script instead.



#going through the documentation, and this is no longer works
#https://github.com/r-spatial/rgee/issues/144
ee_search_dataset() >
  ee_search_type('ImageCollection') >
  ee_search_provider('European Union/ESA/Copernicus') >
  ee_search_title('Sentinel-2')

#per this link, try instead:
ee$ImageCollection$Dataset$CAS_IGSNRR_PML_V2 %>% 
  ee_utils_dataset_display()

rgee::ee$ImageCollection$

landast = ee$imagecollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')
