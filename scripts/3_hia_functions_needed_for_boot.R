#filename: 3_hia_functions_needed_for_boot

#I'm copying the functions created here
#~green-space-denver/scripts/3_HIA_for_each_scenario.R
#so that I don't have to re-run that entire code every time


mutate_part_of_hia = function(df){ #make a function out of it since 
  df %>%    #I use it again in the bootstrap
    mutate(
      rr_alt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
      paf =(rr_alt -1)/rr_alt , #pop_est attrib fraction
      #attributable deaths
      #rate is per 100,000 so first divide by 100,000 before multiplying by the pop_est. in age group
      #marginal totals were filtered out above, so it's just the joint sex-by-age categories
      #could generalize from d to o for outcome if we include other outcomes rather than just death
      attrib_d = paf*(rate_per_100k_est/100000)*pop_affected, #attrib deaths. note divide by 100000
      deaths_prevented = attrib_d*-1,
      deaths_prevented_per_pop = deaths_prevented/pop_affected #pp is per person
    )
}

summarise_ungroup_hia_pop_stuff_by_ndvi = function(df){
  df %>% 
    summarise(
      pop_affected = sum(pop_affected, na.rm=TRUE),
      attrib_d = sum(attrib_d, na.rm=TRUE),
      deaths_prevented = sum(deaths_prevented, na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      #this has to be in its own separate mutate by order of operations
      deaths_prevented_per_pop = deaths_prevented/pop_affected,
      deaths_prevented_per_pop_100k = deaths_prevented_per_pop*100000
    ) 
}

summarise_ungroup_hia_area_stuff_by_ndvi = function(df){
  df %>% 
    summarise(
      #recall, area_mi2_bg_int_res also works for the block-group-level ones
      area_mi2_bg_int_res = sum(area_mi2_bg_int_res, na.rm=TRUE), #residential buffer area
      area_mi2_bg_int_tx = sum(area_mi2_bg_int_tx, na.rm=TRUE), #treatment area; same for bg-level ones
      
      #sum these products before computing the weighted average below.
      #note these _int values are products of the block-group-level value and the corresponding area (weight)
      ndvi_mean_alt_int = sum(ndvi_mean_alt_int, na.rm=TRUE),
      ndvi_quo_int = sum(ndvi_quo_int, na.rm=TRUE),
      ndvi_mean_wt_tx_int = sum(ndvi_mean_wt_tx_int, na.rm=TRUE) #keep track of this for summary
    ) %>% 
    ungroup() %>% 
    mutate(
      #adding this 6/24/22 - what proportion of the residential area is
      #covered by the treatment area? I think this is useful to show how how dependent results are
      #on use of 500 m buffer
      area_prop_tx_res = area_mi2_bg_int_tx/area_mi2_bg_int_res,
      
      ndvi_mean_alt =ndvi_mean_alt_int/area_mi2_bg_int_res, #weighted mean
      ndvi_quo =ndvi_quo_int/area_mi2_bg_int_res, #weighted mean
      ndvi_diff = ndvi_mean_alt-ndvi_quo, #useful to keep
      ndvi_mean_wt_tx = ndvi_mean_wt_tx_int/area_mi2_bg_int_tx  #weighted average of NDVI over treatment area only. note diff denom
    ) 
}
