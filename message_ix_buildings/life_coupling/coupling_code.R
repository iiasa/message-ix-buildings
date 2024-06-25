
## 1) CORE FUNCTIONS OF THE LIFE MODEL

fun_ls1_d_beh_prop <- function(d_beh, f_beh_prop){d_prop = c(f_beh_prop*d_beh)} # Delta Propensity = f(Delta Behaviour)
fun_ls2_d_cog <- function(d_prop, h_cog, f_prop){d_cog = h_cog + f_prop * d_prop} # Delta Cognition = f(Delta Propensity)
fun_ls3_d_prop <- function(d_cog, h_prop, f_cog){d_prop = h_prop + f_cog * d_cog} # Delta Cognition = f(Delta Propensity)

## 2) CALIBRATION PROCESS FUNCTION

fun_life_calib <- function(cf_ls_mod_I_sens){
  
  # Fit spline curve on sensitivity results - from behavioural difference with reference to modifiers
  crv_mod_I = with(cf_ls_mod_I_sens,
                    cobs(x=beh_I_diff
                         , y=s_int_mod
                         , constraint = "decrease"
                         , degree = 2
                         , pointwise = rbind(c(0,0,0)) # constraint to zero
                         #, keep.data = TRUE
                    ))
  #plot(crv_mod_I)
  return(crv_mod_I)
}

# 3) MODEL COUPLING FUNCTION

fun_life_coupling <- function(yrs, i, 
d_beh, # Different file imported at every timestep
f_beh_prop, # Fixed.by hh_group
h_cog, # timeseries - by hh_group
f_prop, # timeseries - by hh_group
h_prop, # timeseries - by hh_group
f_cog_I, # timeseries - by hh_group
f_cog_A, # timeseries - by hh_group
ratio_beh_I_prop,# Fixed.by hh_group
ratio_beh_A_prop, # Fixed.by hh_group
beh_I_base # Fixed.by hh_group
){

  # APPLY LIFE COUPLING FUNCTIONS
  ls_delta_i = d_beh %>% 
    left_join(f_beh_prop) %>% # Fixed.by hh_group
    left_join(h_cog) %>% # timeseries - by hh_group
    left_join(f_prop) %>% # timeseries - by hh_group
    left_join(h_prop) %>% # timeseries - by hh_group
    left_join(f_cog_I) %>% # timeseries - by hh_group
    left_join(f_cog_A) %>% # timeseries - by hh_group
    left_join(ratio_beh_I_prop) %>% # Fixed.by hh_group
    left_join(ratio_beh_A_prop) %>% # Fixed.by hh_group
    mutate(d_beh_prop = fun_ls1_d_beh_prop(d_beh, f_beh_prop)) %>% # Step 1
    mutate(d_cog = fun_ls2_d_cog(d_beh_prop, h_cog, f_prop)) %>% # Step 2
    mutate(d_prop_I = fun_ls3_d_prop(d_cog, h_prop, f_cog_I)) %>% # Step 3 - Improve
    mutate(d_prop_A = fun_ls3_d_prop(d_cog, h_prop, f_cog_A)) %>% # Step 3 - Avoid
    # # calculate updated behaviours (scaled)
    # left_join(cf_ls_base %>% select(hh_group,ratio_beh_I_prop,ratio_beh_A_prop)) %>%
    mutate(beh_I = d_prop_I * ratio_beh_I_prop) %>%
    mutate(beh_A = d_prop_A * ratio_beh_A_prop) %>%
    mutate(beh_A = ifelse(beh_A > 1, 1, beh_A))
  
  
  ### OUTPUT: IMPROVE (I) DIMENSION ###
  ls_mod_I_i = ls_delta_i %>%
    select(-c(d_beh, f_beh_prop, h_cog, f_prop, h_prop, f_cog_I, f_cog_A, d_beh_prop, d_cog, d_prop_I, d_prop_A, ratio_beh_I_prop, ratio_beh_A_prop, beh_A)) %>%
    mutate(beh_I_univ_base = beh_I_base %>% filter(hh_group == "univ") %>% select(beh_I_base) %>% pull) %>% # average behaviour from base year (intang.costs modifiers = 0)
    mutate(beh_I_diff = (beh_I - beh_I_univ_base)/beh_I_univ_base) %>%
    mutate(mod_I = predict(crv_mod_I, beh_I_diff)[,"fit"]) %>%   # calculate propensity modifiers - based on spline curve
    select(-c(beh_I, beh_I_univ_base))
  
  ### OUTPUT: AVOID (A) DIMENSION ###
  cf_ls_A = ls_delta_i %>%
    select(-c(year, d_beh, f_beh_prop, h_cog, f_prop, h_prop, f_cog_I, f_cog_A, 
              d_beh_prop, d_cog, d_prop_I, d_prop_A, ratio_beh_I_prop, ratio_beh_A_prop, beh_I)) %>%
    mutate(beh_A = ifelse(beh_A >1, 1, beh_A)) 
  
  # Prepare combined Improve-Avoid output
  output_life = ls_mod_I_i %>% left_join(cf_ls_A)
  
  # Return results
  return(output_life)
}
