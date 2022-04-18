#filename: 3a_HIA_for_each_scenario_boot.R

library(tidyverse)
library(sf)
library(here)
library(truncnorm)
setwd(here("data-processed"))
#started 4/16/22
#revised

#This is the bootstrapping code for the HIA
#Two sources of error: population estimates and the dose-response function

#https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20180418_MOE.pdf
#For the census data, assume a normal distribution, where the margin of error represents a 90%
#confidence interval, or 1.645 SD

#For the dose-response function, first log the risk ratio, then assume a normal distribution
#using the 95% CI values of 0.94 and 0.97; note point estimate is .96, so use the more conservative

#you now have these standard deviations in the combined hia file; great


load("hia_all.RData")
names(hia_all)
table(hia_all$scenario)
table(hia_all$scenario_sub)
summary(hia_all$drf_est_log)
nrow(hia_all)
# Define bootstrap function-----------
#as with other projects, e.g.,
#/diss/scripts/aim1_1_1_hex_acs_bootstrap.R
#don't rename the variables to indicate that they're a sample. not necessary
#you will know by virtue of their being in this dataset. if you need the point estimates, link in
#the other dataset
bootstrap_hia = function(s_id_val){
  hia_all_s = hia_all %>% 
    mutate(
#      pop_est = truncnorm::rtruncnorm(n=n(), mean=pop_est, sd=pop_sd, a=0),     #sample population
      pop_est = rnorm(n=n(), mean=pop_est, sd=pop_sd),     #sample population; try without truncated
      pop_dens_mi2 = pop_est/area_mi2_bg,
      pop_affected = case_when(
        scenario == "all-bg" ~ pop_est, #for scenario 1, it's just the pop of the bg
        TRUE ~ area_mi2_bg_int_res*pop_dens_mi2 #for other scenarios, multiply area by pop dens
      ),
      #sample dose-response function
      drf_est_log = rnorm(n=n(), mean=drf_est_log, sd = drf_sd_log_scale), #normal on log scale
      drf_est = exp(drf_est_log), #then exponentiate
      rr_alt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
      paf =(rr_alt -1)/rr_alt, #pop_est attrib fraction
      attrib_d = paf*(rate_per_100k_est/100000)*pop_affected, #attrib deaths. note divide by 100000
      
      s_id = s_id_val
      ) 
}  

# Run bootstrap function----------
#run the function x times
n_boot_reps = 500
s_id_val_list <- seq(from = 1, to = n_boot_reps, by = 1)

hia_all_boot  = s_id_val_list %>% 
  map_dfr(bootstrap_hia) %>% 
  dplyr::select(
    contains("fips"), contains("scenario"), 
    starts_with("pop"), starts_with("area"), contains("area"),
    contains("ndvi"), contains("drf"), starts_with("rr"), starts_with("paf"), starts_with("attrib"),
    everything())

hia_all_boot
# Summarize results--------------
summary(hia_all_boot$drf_est)


## Summarize HIA------------
### Summarize overall-----------
hia_all_overall_s = hia_all_boot %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(s_id, scenario, scenario_sub) %>% #
  summarise(
    pop_affected = sum(pop_affected, na.rm=TRUE),
    attrib_d = sum(attrib_d, na.rm=TRUE)
  ) %>% 
  #now summarize over sample id and take percentiles
  group_by( scenario, scenario_sub) %>% 
  summarise(
    pop_affected_ll = quantile(pop_affected, probs =c(0.025), na.rm=TRUE),
    pop_affected_ul = quantile(pop_affected, probs =c(0.975), na.rm=TRUE),
    attrib_d_ll = quantile(attrib_d, probs =c(0.025), na.rm=TRUE),
    attrib_d_ul = quantile(attrib_d, probs =c(0.975), na.rm=TRUE)
  )

hia_all_overall_s
#save the summary but not hia_all_boot, as it's too huge and will take up too much space
setwd(here("data-processed"))
save(hia_all_overall_s, file = "hia_all_overall_s.RData")
hia_all_overall
