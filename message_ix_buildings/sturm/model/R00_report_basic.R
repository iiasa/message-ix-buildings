
## AGGREGATE AND REPORT RESULTS

## geo_level_report should be one of the column in the DF "geo_data"

fun_report_basic <- function(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, path_out){
  
  print(paste0("Aggregate and report results - STURM Basic Report"))

  # Write energy results
  if ("energy" %in% report_var){
                               
    print(paste0("Write energy results : ", scenario_name))
    
    if(geo_level_report == geo_level){
      en_stock_rep <- report$en_stock} else {
        en_stock_rep <- report$en_stock %>%
          ljoin(geo_data) %>%
          group_by_at(paste(c(geo_level_report, "urt","arch","mat","eneff","fuel_heat","fuel_cool",   
                              "scenario", # "ssp", # drop SSP
                              "year"))) %>%
          summarise(stock_M =sum(stock_M),
                    floor_Mm2 = sum(floor_Mm2), 
                    heat_TJ = sum(heat_TJ),   
                    cool_TJ = sum(cool_TJ),
                    cool_ac_TJ = sum(cool_ac_TJ),
                    cool_fans_TJ = sum(cool_fans_TJ),
                    hotwater_TJ = sum(hotwater_TJ),
                    other_uses_TJ = sum(other_uses_TJ)
          )%>%
          ungroup()}
  
    write_csv(en_stock_rep, paste0(path_out,"report_STURM_",scenario_name,"_", sector, "_",geo_level_report, "_energy.csv") )
  }
  
  # Write material results
  if ("material" %in% report_var){
    print(paste0("Write material results : ", scenario_name))
    
    if(geo_level_report == geo_level){
      mat_stock_rep <- report$mat_stock} else {
      mat_stock_rep <- report$mat_stock %>%
          ljoin(geo_data) %>%
          group_by_at(paste(c(geo_level_report, "urt","arch","mat","eneff","material",  
                              "scenario", # "ssp", # drop SSP
                              "year"))) %>%
          summarise(
                    floor_tot_Mm2 = sum(floor_tot_Mm2), 
                    floor_new_Mm2 = sum(floor_new_Mm2), 
                    floor_dem_Mm2 = sum(floor_dem_Mm2), 
                    mat_stock_Mt = sum(mat_stock_Mt),   
                    mat_demand_Mt = sum(mat_demand_Mt),
                    mat_primary_Mt = sum(mat_primary_Mt),
                    mat_scrap_Mt = sum(mat_scrap_Mt),
                    mat_reuse_Mt = sum(mat_reuse_Mt),
                    mat_recycling_Mt = sum(mat_recycling_Mt),
                    mat_downcycling_Mt = sum(mat_downcycling_Mt),
                    mat_other_treat_Mt = sum(mat_other_treat_Mt))%>%
          ungroup()
      }
    
    write_csv(mat_stock_rep, paste0(path_out,"report_STURM_",scenario_name,"_", sector, "_",geo_level_report, "_material.csv") )
  }
                               
  # Write vintage results
  if ("vintage" %in% report_var){
    print(paste0("Write vintage results : ", scenario_name))
  
    if(geo_level_report == geo_level){
      bld_eneff_age_rep <- report$bld_eneff_age} else {
        bld_eneff_age_rep <- report$bld_eneff_age %>%
          ljoin(geo_data) %>%
          group_by_at(paste(c(geo_level_report, "year", "yr_con","urt","arch","mat","eneff"
                              #,"scenario", # "ssp", # drop SSP
                              ))) %>%
          summarise(n_units =sum(n_units_eneff))%>%
          ungroup()}
    write_csv(bld_eneff_age_rep, paste0(path_out,"report_STURM_",scenario_name,"_", sector, "_",geo_level_report, "_vintage.csv") )
  }

  # Write household-type results (household-type overlay only)
  if (!is.null(report$hh_stock)) {
    print(paste0("Write household-type results : ", scenario_name))

    hh_cols <- c("urt", "clim", "inc_cl", "year", "hh_type", "n_units", "n_hh", "n_hh_target")
    housing_cols <- setdiff(names(report$hh_stock), c(names(geo_data), hh_cols))

    hh_rep <- report$hh_stock %>%
      ljoin(geo_data) %>%
      mutate(abs_dev = abs(n_hh - n_hh_target))

    # Households by type and housing type (million)
    hh_stock_rep <- hh_rep %>%
      group_by_at(c(geo_level_report, "urt", "inc_cl", housing_cols, "hh_type", "year")) %>%
      summarise(n_hh_M = sum(n_hh) / 1e6,
                n_hh_target_M = sum(n_hh_target) / 1e6) %>%
      ungroup()
    write_csv(hh_stock_rep, paste0(path_out,"report_STURM_",scenario_name,"_", sector, "_",geo_level_report, "_hh_stock.csv") )

    # Housing mismatch: share of households not living where the survey distribution would place them
    hh_mismatch_rep <- hh_rep %>%
      group_by_at(c(geo_level_report, "year")) %>%
      summarise(mismatch = 0.5 * sum(abs_dev) / sum(n_hh)) %>%
      ungroup()
    write_csv(hh_mismatch_rep, paste0(path_out,"report_STURM_",scenario_name,"_", sector, "_",geo_level_report, "_hh_mismatch.csv") )
  }
}