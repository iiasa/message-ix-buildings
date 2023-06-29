## AGGREGATE AND REPORT RESULTS

## geo_level_report should be one of the column in the DF "geo_data"

#' @title Aggregate and report results - STURM Basic Report
#' @description Aggregate and report results - STURM Basic Report
#' @param report Report object
#' @param report_var Report variables
#' @param geo_data Geo data
#' @param geo_level Geo level
#' @param geo_level_report Geo level report
#' @param sector Sector, default is "resid"
#' @param scenario_name Scenario name, default is "NAV_Dem-NPi-ref"
#' @param path_out Path to output data, default is current working directory
#' @return Results of the STURM model
fun_report_basic <- function(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, path_out) {
  print(paste0("Aggregate and report results - STURM Basic Report"))

  if ("energy" %in% report_var) {
    print(paste0("Write energy results : ", scenario_name))

    if (geo_level_report == geo_level) {
      en_stock_rep <- report$en_stock
    } else {
      en_stock_rep <- report$en_stock %>%
        left_join(geo_data) %>%
        group_by_at(paste(c(
          geo_level_report, "urt", "arch", "mat",
          "eneff", "fuel_heat", "fuel_cool",
          "scenario", "year"
        ))) %>%
        summarise(
          stock_M = sum(stock_M),
          floor_Mm2 = sum(floor_Mm2),
          heat_TJ = sum(heat_TJ),
          cool_TJ = sum(cool_TJ),
          cool_ac_TJ = sum(cool_ac_TJ),
          cool_fans_TJ = sum(cool_fans_TJ),
          hotwater_TJ = sum(hotwater_TJ),
          other_uses_TJ = sum(other_uses_TJ)
        ) %>%
        ungroup()
    }

    write_csv(en_stock_rep, 
      paste0(path_out, "report_STURM_", scenario_name, "_", 
        sector, "_", geo_level_report, "_energy.csv"))
  }

  # Write material results
  if ("material" %in% report_var) {
    print(paste0("Write material results : ", scenario_name))

    if (geo_level_report == geo_level) {
      mat_stock_rep <- report$mat_stock
    } else {
      mat_stock_rep <- report$mat_stock %>%
        left_join(geo_data) %>%
        group_by_at(paste(c(
          geo_level_report, "urt", "arch", "mat", "eneff", "material",
          "scenario", # "ssp", # drop SSP
          "year"
        ))) %>%
        summarise(
          floor_tot_Mm2 = sum(floor_tot_Mm2),
          floor_new_Mm2 = sum(floor_new_Mm2),
          floor_dem_Mm2 = sum(floor_dem_Mm2),
          mat_stock_Mt = sum(mat_stock_Mt),
          mat_demand_Mt = sum(mat_demand_Mt),
          mat_scrap_Mt = sum(mat_scrap_Mt)
        ) %>%
        ungroup()
    }

    write_csv(mat_stock_rep, paste0(path_out, "report_STURM_", scenario_name, "_", sector, "_", geo_level_report, "_material.csv"))
  }

  # Write vintage results
  if ("vintage" %in% report_var) {
    print(paste0("Write vintage results : ", scenario_name))

    if (geo_level_report == geo_level) {
      bld_eneff_age_rep <- report$bld_eneff_age
    } else {
      bld_eneff_age_rep <- report$bld_eneff_age %>%
        left_join(geo_data) %>%
        group_by_at(paste(c(
          geo_level_report, "year", "yr_con", "urt", "arch", "mat", "eneff"
          # ,"scenario", # "ssp", # drop SSP
        ))) %>%
        summarise(n_units = sum(n_units_eneff)) %>%
        ungroup()
    }
    write_csv(mat_stock_rep, paste0(path_out, "report_STURM_", scenario_name, "_", sector, "_", geo_level_report, "_vintage.csv"))
  }
}
