library(dplyr)
library(tidyr)



fun_format_output <- function(i,
                              yrs,
                              sector,
                              run,
                              bld_det_i,
                              bld_cases_fuel,
                              ct_fuel_comb,
                              shr_need_heat,
                              floor_cap,
                              hh_size,
                              report_var,
                              report,
                              en_m2_scen_heat,
                              en_m2_scen_cool,
                              en_hh_tot,
                              en_hh_hw_scen,
                              en_m2_hw_scen,
                              en_m2_others,
                              ren_det_i = NULL,
                              cost_renovation = NULL,
                              dem_det_age_i = NULL,
                              dem_det_slum_age_i = NULL,
                              new_det_age_i = NULL,
                              new_det_slum_age_i = NULL) {
    if ("energy" %in% report_var) {
        en_stock_i <- fun_format_bld_stock_energy(
            i,
            yrs,
            sector,
            run,
            bld_det_i,
            bld_cases_fuel,
            ct_fuel_comb,
            shr_need_heat,
            hh_size,
            floor_cap,
            en_m2_scen_heat,
            en_m2_scen_cool,
            en_hh_tot,
            en_hh_hw_scen
        )
        report$en_stock <- bind_rows(report$en_stock, en_stock_i)

        # Aggregate results at fuel level
        det_rows <- en_stock_i %>%
            group_by_at(c("region_bld", "year", "fuel_heat")) %>%
            summarise(
                stock_building = sum(stock_M),
                heat_TJ = sum(heat_TJ),
                cost_heat = sum(cost_energy_hh)
            ) %>%
            ungroup() %>%
            rename(resolution = fuel_heat) %>%
            gather(variable, value, stock_building, heat_TJ, cost_heat)

        # Adding total values for all resolutions
        agg_rows <- det_rows %>%
            group_by_at(setdiff(names(det_rows), c("resolution", "value"))) %>%
            summarize(value = sum(value)) %>%
            ungroup() %>%
            mutate(resolution = "all")

        temp <- bind_rows(det_rows, agg_rows)

        agg <- temp %>%
            group_by_at(c("year", "variable")) %>%
            summarize(value = sum(value)) %>%
            ungroup() %>%
            mutate(resolution = "all", region_bld = "EU")

        temp <- bind_rows(temp, agg) %>%
            select(c("region_bld", "year", "variable",
                "resolution", "value")) %>%
            arrange(region_bld, year, variable, resolution)

        report$agg_result <- bind_rows(report$agg_result, temp)
        

        if (!is.null(cost_renovation) && !is.null(ren_det_i)) {
            cost_renovation <- cost_renovation %>%
                left_join(hh_size) %>%
                left_join(floor_cap) %>%
                mutate(cost = hh_size * floor_cap * cost_invest_ren_shell)

            det_rows <- ren_det_i %>%
                left_join(cost_renovation) %>%
                mutate(total_cost = cost * n_units_fuel) %>%
                group_by_at(c("region_bld", "year", "eneff")) %>%
                summarize(cost_renovation = sum(total_cost),
                    n_renovation = sum(n_units_fuel)) %>%
                ungroup() %>%
                rename(resolution = eneff) %>%
                gather(variable, value, cost_renovation, n_renovation)
            
            # Adding total values for all resolutions
            agg_rows <- det_rows %>%
                group_by_at(setdiff(names(det_rows),
                    c("resolution", "value"))) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(resolution = "all")

            temp <- bind_rows(det_rows, agg_rows)
            
            agg <- temp %>%
                group_by_at(c("year", "variable")) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(resolution = "all", region_bld = "EU")

            temp <- bind_rows(temp, agg) %>%
                select(c("region_bld", "year", "variable",
                    "resolution", "value")) %>%
                arrange(region_bld, year, variable, resolution)

            report$agg_result <- bind_rows(report$agg_result, temp)
        }
    }

    if ("material" %in% report_var) {
        # Stock results - Material
        bld_cases_eneff <- bld_cases_fuel %>%
            select(-c(fuel_heat, fuel_cool)) %>%
            distinct()

        # Aggregate results at eneff level demolitions
        dem_eneff_i <- dem_det_age_i %>%
            group_by_at(setdiff(names(dem_det_age_i), c(
                "bld_age", "fuel_heat", "fuel_cool",
                "n_units_fuel_p", "n_dem", "n_empty"
            ))) %>%
            summarise(
                n_dem = sum(n_dem)
            ) %>%
            ungroup()

        # new constructions
        new_eneff_i <- bind_rows(new_det_age_i, new_det_slum_age_i) %>%
            group_by_at(setdiff(names(new_det_age_i), c(
                "bld_age", "fuel_heat", "fuel_cool", "n_units_fuel"
            ))) %>%
            summarise(n_new = sum(n_units_fuel)) %>%
            ungroup()

        # stock
        bld_eneff_i <- bld_det_i %>%
            group_by_at(setdiff(names(bld_det_i), c(
                "bld_age", "fuel_heat", "fuel_cool", "n_units_fuel"
            ))) %>%
            summarise(n_units = sum(n_units_fuel)) %>%
            ungroup()

        if (sector == "resid") {
            # Calculate material stock
            mat_stock_i <- bld_cases_eneff %>%
                mutate(scenario = run) %>%
                # mutate(ssp = ssp_r) %>%
                mutate(year = yrs[i]) %>%
                left_join(hh_size) %>%
                left_join(floor_cap) %>%
                left_join(bld_eneff_i) %>%
                left_join(dem_eneff_i) %>%
                left_join(new_eneff_i) %>%
                mutate(
                    n_units = ifelse(is.na(n_units), 0, n_units),
                    n_new = ifelse(is.na(n_new), 0, n_new),
                    n_dem = ifelse(is.na(n_dem), 0, n_dem)
                ) %>%
                filter(n_units + n_new + n_dem != 0) %>%
                filter(mat != "sub") %>% # Exclude slums (no material intensity data)
                left_join(mat_int) %>%
                # filter(arch != "inf") %>% # Materials not calculated for slums
                mutate(floor_tot_Mm2 = n_units * hh_size * floor_cap / 1e6) %>% # Mm2
                mutate(floor_new_Mm2 = n_new * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
                mutate(floor_dem_Mm2 = n_dem * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
                mutate(mat_stock_Mt = n_units * hh_size * floor_cap * mat_int / 1e3 / 1e6) %>% # Mt/y
                mutate(mat_demand_Mt = n_new * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
                mutate(mat_scrap_Mt = n_dem * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
                # Drop yr_con dimension
                group_by_at(
                    paste(c("region_gea", "region_bld",
                        "urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", "scenario",
                        "year"
                    ))) %>% 
                summarise(
                    floor_tot_Mm2 = sum(floor_tot_Mm2),
                    floor_new_Mm2 = sum(floor_new_Mm2),
                    floor_dem_Mm2 = sum(floor_dem_Mm2),
                    # mat_int = weighted.mean(mat_int, floor_tot_Mm2),
                    mat_stock_Mt = sum(mat_stock_Mt),
                    mat_demand_Mt = sum(mat_demand_Mt),
                    mat_scrap_Mt = sum(mat_scrap_Mt)
                ) %>%
                ungroup() %>%
                mutate(mat_int = 1e3 * mat_stock_Mt / floor_tot_Mm2) # Recalculate average material intensity
        } else {
            mat_stock_i <- bld_cases_eneff %>%
                mutate(scenario = run) %>%
                # mutate(ssp = ssp_r) %>%
                mutate(year = yrs[i]) %>%
                # left_join(floor_cap) %>%
                left_join(bld_eneff_i) %>%
                left_join(dem_eneff_i) %>%
                left_join(new_eneff_i) %>%
                mutate(
                    n_units = ifelse(is.na(n_units), 0, n_units),
                    n_new = ifelse(is.na(n_new), 0, n_new),
                    n_dem = ifelse(is.na(n_dem), 0, n_dem)
                ) %>%
                filter(n_units + n_new + n_dem != 0) %>%
                left_join(mat_int) %>%
                # filter(arch != "inf") %>% # Materials not calculated for slums
                mutate(floor_tot_Mm2 = n_units / 1e6) %>% # Mm2
                mutate(floor_new_Mm2 = n_new / stp / 1e6) %>% # Mm2/yr
                mutate(floor_dem_Mm2 = n_dem / stp / 1e6) %>% # Mm2/yr
                mutate(mat_stock_Mt = n_units * mat_int / 1e3 / 1e6) %>% # Mt
                mutate(mat_demand_Mt = n_new * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
                mutate(mat_scrap_Mt = n_dem * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
                # Drop yr_con dimension
                group_by_at(
                    paste(c("region_gea", "region_bld",
                        "urt", "clim", "inc_cl", "arch", "mat",
                        "eneff", "material", "scenario",
                        "year"
                    ))) %>%
                summarise(
                    floor_tot_Mm2 = sum(floor_tot_Mm2),
                    floor_new_Mm2 = sum(floor_new_Mm2),
                    floor_dem_Mm2 = sum(floor_dem_Mm2),
                    # mat_int = weighted.mean(mat_int, floor_tot_Mm2),
                    mat_stock_Mt = sum(mat_stock_Mt),
                    mat_demand_Mt = sum(mat_demand_Mt),
                    mat_scrap_Mt = sum(mat_scrap_Mt)
                ) %>%
                ungroup() %>%
                mutate(mat_int = 1e3 * mat_stock_Mt / floor_tot_Mm2) # Recalculate average material intensity
        }

        ## Stock results - Material - Add Cement

        cement_content <- 0.15 ## Cement content in concrete

        mat_stock_cem_i <- mat_stock_i %>%
            filter(material == "concrete") %>%
            mutate(material = "cement") %>%
            mutate(
                mat_stock_Mt = mat_stock_Mt * cement_content,
                mat_demand_Mt = mat_demand_Mt * cement_content,
                mat_scrap_Mt = mat_scrap_Mt * cement_content
            )

        mat_stock_i <- rbind(mat_stock_i, mat_stock_cem_i)
        report$mat_stock <- bind_rows(report$mat_stock, mat_stock_i)
    }

    if ("vintage" %in% report_var) {
        report$bld_eneff_age <- bind_rows(
            report$bld_eneff_age,
            bld_det_i %>%
                group_by_at(setdiff(names(bld_det_i), c(
                    "fuel_heat", "fuel_cool", "n_units_fuel"
                ))) %>%
                summarise(n_units_eneff = sum(n_units_fuel)) %>%
                ungroup()
        )
    }

    return(report)
}


#' @title Format building stock output
#' @description Format building stock output
#' @param bld_det_i Data frame with building stock information
#' @param bld_cases_fuel Data frame with building cases information
#' @param yrs: years to be analysed
#' @param sector: sector to be analysed
#' @param run: run to be analysed
#' @return Data frame with building stock information
fun_format_bld_stock_energy <- function(
                                        i,
                                        yrs,
                                        sector,
                                        run,
                                        bld_det_i,
                                        bld_cases_fuel,
                                        ct_fuel_comb,
                                        shr_need_heat,
                                        hh_size,
                                        floor_cap,
                                        en_m2_scen_heat,
                                        en_m2_scen_cool,
                                        en_hh_tot,
                                        en_hh_hw_scen
                                        ) {
    # Aggregate at fuel level for keeping track of the stock
    bld_det_i <- bld_det_i %>%
        # Select all variables, except the ones indicated, for grouping
        group_by_at(setdiff(
            names(bld_det_i),
            c("yr_con", "n_units_fuel", "fuel")
        )) %>%
        summarise(
            n_units_fuel = sum(n_units_fuel)
        ) %>%
        ungroup()

    en_stock_i <- bld_cases_fuel %>%
        mutate(scenario = run) %>%
        mutate(year = yrs[i]) %>%
        # Issue matching periods of construction when definition is different!
        left_join(bld_det_i) %>%
        # Add "v_no_heat" category
        pivot_wider(
            names_from = "fuel_heat",
            values_from = "n_units_fuel"
        ) %>%
        mutate(v_no_heat = 0) %>%
        pivot_longer(
            cols = c(sort(unique(ct_fuel_comb$fuel_heat)), "v_no_heat"),
            names_to = "fuel_heat",
            values_to = "n_units_fuel"
        ) %>%
        filter(!is.na(n_units_fuel)) %>%
        group_by_at(paste(c(
            "region_bld", "urt", "inc_cl", "arch",
            "year", "clim", "bld_age", "eneff"
        ))) %>%
        # Calculate n_units_eneff to account later for buildings with no heating
        mutate(n_units_eneff = sum(n_units_fuel)) %>%
        ungroup()

    if (sector == "resid") {
        en_stock_i <- en_stock_i %>%
            left_join(shr_need_heat) %>%
            # Rescale number of units based on fuel
            # Heating access not considered here!
            mutate(n_units_fuel = ifelse(fuel_heat == "v_no_heat",
                n_units_eneff * (1 - shr_need_heat),
                n_units_fuel * shr_need_heat
            )) %>%
            left_join(hh_size) %>%
            left_join(floor_cap) %>%
            # left_join(shr_acc_cool) %>%
            left_join(en_m2_scen_heat) %>%
            left_join(en_m2_scen_cool) %>%
            left_join(en_hh_tot %>% rename(cost_energy_hh = cost_op)) %>%
            mutate(cost_energy_hh = cost_energy_hh * n_units_fuel) %>%
            left_join(en_hh_hw_scen) %>%
            # convert n. units to millions
            mutate(floor_Mm2 = n_units_fuel / 1e6 * hh_size * floor_cap) %>%
            mutate(floor_heat_Mm2 = floor_Mm2) %>%
            # mutate(floor_heat_Mm2 = ifelse(acc_heat == 1, floor_Mm2, 0)) %>%
            mutate(
                floor_cool_Mm2 =
                    ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)
            ) %>%
            # Converted from kWh to MJ (3.6).
            #  Housing units are in million, so results are in TJ.
            mutate(heat_TJ = ifelse(fuel_heat == "v_no_heat", 0,
                en_dem_heat * n_units_fuel / 1e6 * hh_size * floor_cap * 3.6
            )) %>%
            # Converted from kWh to MJ (3.6).
            #  Houssing units are in million, so results are in TJ.
            mutate(cool_TJ = en_dem_cool * shr_acc_cool *
                n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>%
            mutate(cool_ac_TJ = en_dem_c_ac * shr_acc_cool *
                n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>%
            # Note:shr_acc_cool=1 for all cases (access calculated before)
            # Converted from kWh to MJ (3.6).
            #   Housing units are in million, so results are in TJ.
            mutate(cool_fans_TJ = en_dem_c_fans * shr_acc_cool *
                n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>%
            # converted from GJ/hh/yr to TJ
            mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0,
                en_dem_dhw * n_units_fuel / 1e3
            )) %>%
            # Other uses not covered for residential
            mutate(other_uses_TJ = 0) %>%
            mutate(stock_M = n_units_fuel / 1e6) %>%
            mutate(cost_energy_hh = ifelse(is.na(cost_energy_hh),
                0, cost_energy_hh)) %>%
            filter(stock_M > 0 & !is.na(stock_M)) %>%
            select_at(paste(c("region_gea", "region_bld",
                "urt", "clim", "inc_cl", "arch", "mat",
                "eneff", "bld_age", "fuel_heat", "fuel_cool",
                "scenario", "year", "stock_M", "floor_Mm2",
                "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ",
                "hotwater_TJ", "other_uses_TJ", "cost_energy_hh"
            )))

    }
    if (sector == "comm") {
        en_stock_i <- en_stock_i %>%
            left_join(shr_need_heat) %>%
            # Rescale number of units based on fuel
            # Heating access not considered here!!
            mutate(n_units_fuel = ifelse(fuel_heat == "v_no_heat",
                n_units_eneff * (1 - shr_need_heat),
                n_units_fuel * shr_need_heat
            )) %>%
            left_join(en_m2_scen_heat) %>%
            left_join(en_m2_scen_cool) %>%
            left_join(en_m2_hw_scen) %>%
            left_join(en_m2_others) %>%
            mutate(floor_Mm2 = n_units_fuel / 1e6) %>%
            mutate(floor_heat_Mm2 = floor_Mm2) %>%
            # mutate(floor_heat_Mm2 = ifelse(acc_heat == 1, floor_Mm2, 0)) %>%
            mutate(
                floor_cool_Mm2 =
                    ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)
            ) %>%
            # Converted from kWh to MJ (3.6).
            # Houssing units are in million, so results are in TJ.
            mutate(heat_TJ = ifelse(fuel_heat == "v_no_heat",
                0, en_dem_heat * n_units_fuel / 1e6 * 3.6
            )) %>%
            # converted from kWh to MJ (3.6).
            # Houssing units are in million, so results are in TJ.
            mutate(
                cool_TJ =
                    en_dem_cool * shr_acc_cool * n_units_fuel / 1e6 * 3.6
            ) %>%
            # converted from kWh to MJ (3.6).
            # Houssing units are in million, so results are in TJ.
            mutate(
                cool_ac_TJ =
                    en_dem_c_ac * shr_acc_cool * n_units_fuel / 1e6 * 3.6
            ) %>%
            # Note:shr_acc_cool=1 for all cases (access calculated before)
            # converted from kWh to MJ (3.6).
            # Houssing units are in million, so results are in TJ.
            mutate(
                cool_fans_TJ =
                    en_dem_c_fans * shr_acc_cool * n_units_fuel / 1e6 * 3.6
            ) %>%
            # Converted from kWh to MJ (3.6).
            # Houssing units are in million, so results are in TJ.
            mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat",
                0, en_dem_dhw * n_units_fuel / 1e6 * 3.6
            )) %>%
            # Converted from kWh to MJ (3.6).
            # Houssing units are in million, so results are in TJ.
            mutate(other_uses_TJ = en_dem_others * n_units_fuel / 1e6 * 3.6) %>%
            mutate(stock_M = n_units_fuel / 1e6) %>%
            filter(stock_M > 0 & !is.na(stock_M)) %>%
            select_at(paste(c(
                "region_gea", "region_bld",
                "urt", "clim", "inc_cl", "arch", "mat",
                "eneff", "fuel_heat", "fuel_cool",
                "scenario", "year", "stock_M", "floor_Mm2",
                "heat_TJ", "cool_TJ", "cool_ac_TJ",
                "cool_fans_TJ", "hotwater_TJ", "other_uses_TJ"
            )))
    }

    return(en_stock_i)
}
