#filename: 3a_HIA_for_each_scenario_boot.R

library(tidyverse)
library(sf)
library(here)
library(truncnorm)
setwd(here("data-processed"))
#started 4/16/22
#revised 5/26/22
# Rerunning February 2, 2023 including the 
#new definition for native plants (July 4, 2021)

#Run functions here:
#scripts/3_hia_functions_needed_for_boot.R
source(here("scripts", "3_hia_functions_needed_for_boot.R"))

#This is the bootstrapping code for the HIA
#Three sources of error for first group (stratified by native definition): 
#   population estimates and the dose-response function and the mortality rate
# three sources of error when summarizing over native definition: 
#native definition, population estimates, and dose-response f

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
      s_id = s_id_val,
#      pop_est = truncnorm::rtruncnorm(n=n(), mean=pop_est, sd=pop_sd, a=0),     #sample population
      pop_est = rnorm(n=n(), mean=pop_est, sd=pop_sd),     #sample population; try without truncated
      pop_dens_mi2 = pop_est/area_mi2_bg,
      pop_affected = case_when(
        scenario == "all-bg" ~ pop_est, #for scenario 1, it's just the pop of the bg
        TRUE ~ area_mi2_bg_int_res*pop_dens_mi2 #for other scenarios, multiply area by pop dens
      ),
      #sample dose-response function
      drf_est_log = rnorm(n=n(), mean=drf_est_log, sd = drf_sd_log_scale), #normal on log scale
      drf_est = exp(drf_est_log) ,  #then exponentiate
    
      #sample baseline mortality rate per 100k (assume normality)
      #https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_LMICS_U5M_2000_2017_INFO_SHEET_Y2019M10D16.PDF
    rate_per_100k_est = rnorm(n=n(), mean= rate_per_100k_est, sd=rate_per_100k_sd)) %>% 
    mutate_part_of_hia()   #function created in 3_HIA_for_each_scenario.R
}  

# Run bootstrap function----------
#run the function x times; 500 reaches memory limit. 200 is fine. 
#don't save the data frame  because it's huge
# Feb 2, 2023: 200 is reaching memory limit. hmm. Let me try with no applications open.
#2 pm: still failed - trying with 100 (worked). 150? worked. 175 good. 200 and 199 failed. 
#Trying 190.
#200 works if I start from a fresh R program and close other programs.
n_boot_reps = 200 
s_id_val_list <- seq(from = 1, to = n_boot_reps, by = 1)

hia_all_boot  = s_id_val_list %>% 
  map_dfr(bootstrap_hia) %>% 
  dplyr::select(
    contains("fips"), contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("pop"), starts_with("area"), contains("area"),
    contains("ndvi"), contains("drf"), starts_with("rr"), 
    starts_with("rate_per_100k"),
    starts_with("paf"), 
    starts_with("equity"),
    starts_with("attrib"),
    everything())

hia_all_boot
nrow(hia_all_boot)
summary(hia_all_boot$drf_est)
summary(hia_all_boot$rate_per_100k_est)

step_two_bootstrap_summarise_ungroup_select = function(df){
  df %>% 
    summarise(
      pop_affected_ll = quantile(pop_affected, probs =c(0.025), na.rm=TRUE),
      pop_affected_ul = quantile(pop_affected, probs =c(0.975), na.rm=TRUE),
      attrib_d_ll = quantile(attrib_d, probs =c(0.025), na.rm=TRUE),
      attrib_d_ul = quantile(attrib_d, probs =c(0.975), na.rm=TRUE),
      #call this _count so it can be dynamically selected later.
      deaths_prevented_count_ll = quantile(deaths_prevented, probs =c(0.025), na.rm=TRUE),
      deaths_prevented_count_ul = quantile(deaths_prevented, probs =c(0.975), na.rm=TRUE),
      deaths_prevented_per_pop_ll = quantile(deaths_prevented_per_pop, probs =c(0.025), na.rm=TRUE),
      deaths_prevented_per_pop_ul = quantile(deaths_prevented_per_pop, probs =c(0.975), na.rm=TRUE),
      deaths_prevented_per_pop_100k_ll = quantile(deaths_prevented_per_pop_100k, probs =c(0.025), na.rm=TRUE),
      deaths_prevented_per_pop_100k_ul = quantile(deaths_prevented_per_pop_100k, probs =c(0.975), na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    dplyr::select(
      contains("scenario"), 
      starts_with("ndvi_native_threshold"),
      starts_with("equity"),
      starts_with("pop"), 
      starts_with("area"),
      starts_with("ndvi"), 
      starts_with("death"),
      everything())
}


# Load point-estimate summaries------
load("hia_all_over_ndvi_by_equity.RData")
load("hia_all_over_ndvi_over_equity.RData")
load("hia_all_by_ndvi_over_equity.RData")
load("hia_all_by_ndvi_by_equity.RData")

# Summarize HIA - collapse over NDVI threshold-------------
## over NDVI over equity--------
hia_all_over_ndvi_over_equity_s = hia_all_boot %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(s_id, scenario, scenario_sub, ndvi_native_threshold) %>% #first summarize by s_id, over equity but by ndvi
  summarise_ungroup_hia_pop_stuff_by_ndvi() %>% #function created in scripts/3_HIA_for_each_scenario.R
  group_by( scenario, scenario_sub  ) %>% #over s_id and over ndvi cat
  #quantiles here reflect overall percentiles including both sources of variation (ndvi definition and others)
  step_two_bootstrap_summarise_ungroup_select()

hia_all_over_ndvi_over_equity_s
names(hia_all_over_ndvi_over_equity_s)
save(hia_all_over_ndvi_over_equity_s, file = "hia_all_over_ndvi_over_equity_s.RData")
## over NDVI by equity-----------
hia_all_over_ndvi_by_equity_s = hia_all_boot %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(s_id, scenario, scenario_sub, ndvi_native_threshold,  equity_nbhd_denver_tertile) %>% #by sample id, etc.
  summarise_ungroup_hia_pop_stuff_by_ndvi() %>% #function created in scripts/3_HIA_for_each_scenario.R
  group_by( scenario, scenario_sub,  equity_nbhd_denver_tertile  ) %>% #over s_id and over ndvi cat but by equity
  #quantiles here reflect overall percentiles including 
  #both sources of variation (ndvi definition and others)
  step_two_bootstrap_summarise_ungroup_select()

hia_all_over_ndvi_by_equity_s
save(hia_all_over_ndvi_by_equity_s, file = "hia_all_over_ndvi_by_equity_s.RData")

## combine the bootstrapped results with the static results and point estimates------
#Previously I had done this in the rmarkdown tables/fig doc, but I'd prefer to do it here.
#combine these first before bind_rows
select_vars_estimate_boot = function(df){
  df %>% 
  dplyr::select(
    contains("scenario"),
    contains("equity"),
    contains("area_mi2_bg_int_tx"),
    contains("area_mi2_bg_int_res"),
    contains("area_prop_tx"),
    contains("ndvi_quo"),
    contains("ndvi_mean_alt"),
    contains("ndvi_diff"),
    contains("ndvi_mean_wt_tx"),
    contains("pop_affect"),
    #have to hard code these to get them in the desired order
    starts_with("deaths_prevented_cou"),
    #this omits the per pop that's not per 100k pop. that's okay. don't use it in tables
    starts_with("deaths_prevented_per_pop_100k"),
    contains("attrib_d")
  )
}


hia_all_over_ndvi_over_equity_est_boot = hia_all_over_ndvi_over_equity %>% 
  left_join(hia_all_over_ndvi_over_equity_s, by = c("scenario", "scenario_sub")) %>% 
  select_vars_estimate_boot()

save(hia_all_over_ndvi_over_equity_est_boot, file = "hia_all_over_ndvi_over_equity_est_boot.RData")
names(hia_all_over_ndvi_over_equity_est_boot)
hia_all_over_ndvi_by_equity_est_boot = hia_all_over_ndvi_by_equity %>% 
  left_join(hia_all_over_ndvi_by_equity_s, by = c("scenario", "scenario_sub", "equity_nbhd_denver_tertile")) %>% 
  select_vars_estimate_boot()

save(hia_all_over_ndvi_by_equity_est_boot, file = "hia_all_over_ndvi_by_equity_est_boot.RData")
names(hia_all_over_ndvi_by_equity_est_boot)

# Summarize HIA - Stratify by NDVI threshold-------------
#Note here neither the NDVI nor the area will have variance because 
#they don't vary within NDVI threshold category
##  by NDVI over equity-----------

hia_all_by_ndvi_over_equity_s = hia_all_boot %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(s_id, scenario, scenario_sub, ndvi_native_threshold) %>% 
  summarise_ungroup_hia_pop_stuff_by_ndvi() %>% 
  group_by( scenario, scenario_sub, ndvi_native_threshold) %>% #summarize over sample id and get quantiles
  step_two_bootstrap_summarise_ungroup_select()

  
hia_all_by_ndvi_over_equity_s
#save the summary but not hia_all_boot, as it's too huge and will take up too much space
setwd(here("data-processed"))
save(hia_all_by_ndvi_over_equity_s, file = "hia_all_by_ndvi_over_equity_s.RData")


## by ndvi by equity--------------
hia_all_by_ndvi_by_equity_s = hia_all_boot %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(s_id, scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile) %>% 
  summarise_ungroup_hia_pop_stuff_by_ndvi() %>% 
  group_by( scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile) %>% 
  step_two_bootstrap_summarise_ungroup_select()


hia_all_by_ndvi_by_equity_s
#save the summary but not hia_all_boot, as it's too huge and will take up too much space
setwd(here("data-processed"))
save(hia_all_by_ndvi_by_equity_s, file = "hia_all_by_ndvi_by_equity_s.RData")




## combine the bootstrapped results with the static results and point estimates------
#Previously I had done this in the rmarkdown tables/fig doc, but I'd prefer to do it here.
#combine these first before bind_rows
hia_all_by_ndvi_over_equity_est_boot = hia_all_by_ndvi_over_equity %>% 
  left_join(hia_all_by_ndvi_over_equity_s, by = c("scenario", "scenario_sub"  )) %>% 
  select_vars_estimate_boot()
names(hia_all_by_ndvi_over_equity_est_boot)
save(hia_all_by_ndvi_over_equity_est_boot, file = "hia_all_by_ndvi_over_equity_est_boot.RData")

hia_all_by_ndvi_by_equity_est_boot = hia_all_by_ndvi_by_equity %>% 
  left_join(hia_all_by_ndvi_by_equity_s, by = c("scenario", "scenario_sub", "equity_nbhd_denver_tertile")) %>% 
  select_vars_estimate_boot()

save(hia_all_by_ndvi_over_equity_est_boot, file = "hia_all_by_ndvi_over_equity_est_boot.RData")


