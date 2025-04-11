library(dplyr)
library(tidyr)


#' @title Format output
#' @description Format output
#' @param i: year to be analysed
#' @param yrs: years to be analysed
#' @param sector: sector to be analysed
#' @param run: run to be analysed
#' @param bld_det_i Data frame with building stock information
#' @param bld_cases_fuel Data frame with building cases information
#' @param ct_fuel_comb Data frame with fuel combinations information
#' @param shr_need_heat Data frame with share of buildings needing heating
#' @param floor_cap Data frame with floor capacity information
#' @param hh_size Data frame with household size information
#' @param report_var: variables to be reported
#' @param report: report object
#' @param en_m2_scen_heat Data frame with energy per m2 for heating
#' @param en_m2_scen_cool Data frame with energy per m2 for cooling
#' @param en_hh_tot Data frame with energy per household
#' @param en_hh_hw_scen Data frame with energy per household for hot water
#' @param en_m2_others Data frame with energy per m2 for other uses
#' @param emission_factors Data frame with emission factors
#' @param ren_det_i Data frame with renovation information
#' @param bld_det_i_sw Data frame with building stock information for space
#' heating
#' @param dem_det_age_i Data frame with demolition information
#' @param dem_det_slum_age_i Data frame with demolition information for slums
#' @param new_det_age_i Data frame with new construction information
#' @param new_det_slum_age_i Data frame with new construction information for
#' slums
#' @param report_turnover Data frame with turnover information
#' @return report object
fun_format_output <- function(i,
                              yrs,
                              stp,
                              sector,
                              run,
                              bld_det_i,
                              bld_cases_fuel,
                              ct_fuel_comb,
                              shr_need_heat,
                              floor_cap,
                              hh_size,
                              pop,
                              report_var,
                              report,
                              en_m2_scen_heat,
                              en_m2_scen_cool,
                              en_hh_tot,
                              en_hh_hw_scen,
                              en_m2_hw_scen,
                              en_m2_others,
                              mat_int,
                              emission_factors,
                              emission_factors_embodied,
                              income,
                              pe_conversion_factor,
                              threshold_poverty = threshold_poverty,
                              new_det_i = NULL,
                              ren_det_i = NULL,
                              bld_det_i_sw = NULL,
                              dem_det_age_i = NULL,
                              dem_det_slum_age_i = NULL,
                              new_det_age_i = NULL,
                              new_det_slum_age_i = NULL,
                              report_turnover = NULL,
                              alpha = NULL,
                              short_term_price_elasticity = -0.2,
                              utility_money = NULL
                              ) {

    # print(head(filter(shr_need_heat, region_bld == "C-WEU-HRV")))                           
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
            en_hh_hw_scen,
            emission_factors,
            threshold_poverty,
            pe_conversion_factor
        )
        en_stock_i <- left_join(en_stock_i, income)

        report$en_stock <- bind_rows(report$en_stock, en_stock_i)
        

        # Aggregate results at fuel level
        det_rows <- en_stock_i %>%
            group_by_at(c("region_bld", "year", "fuel_heat")) %>%
            summarise(
                stock_building = sum(stock_M) * 1e6,
                energy_poverty_10 = sum(energy_poverty_10),
                energy_poverty_thresh = sum(energy_poverty_thresh),
                heat_kWh = sum(heat_TJ) / 3.6 * 1e6,
                heat_pe_kWh = sum(heat_TJ) / 3.6 * 1e6,
                heat_std_kWh = sum(heat_std_TJ) / 3.6 * 1e6,
                heat_EJ = sum(heat_TJ) / 1e6,
                hotwater_EJ = sum(hotwater_TJ) / 1e6,
                heat_tCO2 = sum(heat_tCO2),
                hotwater_tCO2 = sum(hotwater_tCO2),
                cost_heat_EUR = sum(cost_energy_hh_wt),
                floor_m2 = sum(floor_Mm2) * 1e6) %>%
            ungroup() %>%
            rename(resolution = fuel_heat) %>%
            gather(variable, value, stock_building,
                energy_poverty_10, energy_poverty_thresh,
                heat_kWh, heat_pe_kWh, heat_std_kWh, heat_EJ,
                hotwater_EJ,
                heat_tCO2, hotwater_tCO2,
                cost_heat_EUR, floor_m2)

        # Adding total values for all resolutions
        agg_rows <- det_rows %>%
            group_by_at(setdiff(names(det_rows), c("resolution", "value"))) %>%
            summarize(value = sum(value)) %>%
            ungroup() %>%
            mutate(resolution = "all")

        temp <- bind_rows(det_rows, agg_rows)

        # Adding population
        t <- pop %>%
            filter(year == yrs[i]) %>%
            filter(region_bld %in% unique(en_stock_i$region_bld)) %>%
            group_by_at(c("region_bld", "year")) %>%
            summarise(value = sum(pop) * 1e6) %>%
            ungroup() %>%
            mutate(resolution = "all", variable = "population")

        temp <- bind_rows(temp, t)

        # Adding taxes revenues
        t <- en_stock_i %>%
            group_by_at(c("region_bld", "year")) %>%
            summarize(value = (sum(cost_energy_hh) - sum(cost_energy_hh_wt)) * stp) %>%
            ungroup() %>%
            mutate(resolution = "all", variable = "taxes_revenues_EUR")
        temp <- bind_rows(temp, t)

        # Calculating thermal comfort
        if (!is.null(alpha)) {
            thermal_comfort <- en_hh_tot %>%
                left_join(bld_det_i) %>%
                filter(n_units_fuel > 0) %>%
                left_join(alpha) %>%
                left_join(utility_money) %>%
                mutate(rho = - 1 / short_term_price_elasticity) %>%
                mutate(A = coeff_alpha**rho * scaling_factor / 1e3) %>%
                mutate(thermal_comfort =
                    A * heating_intensity**(1 - rho) / (1 - rho)) %>%
                mutate(thermal_comfort = thermal_comfort / scaling_factor * 1e3) %>%
                mutate(thermal_comfort = thermal_comfort * n_units_fuel)


            # name <- paste("thermal_comfort", yrs[[i]], run, sep = "_")
            # t <- filter(thermal_comfort, region_bld == "C-EEU-SVN")
            # write.csv(t, paste(path_out, name, ".csv", sep = ""))

            thermal_comfort <- thermal_comfort %>%
                mutate(thermal_comfort =
                    ifelse(is.na(thermal_comfort), 0, thermal_comfort)) %>%
                group_by_at(c("region_bld", "year")) %>%
                summarize(value = sum(thermal_comfort)) %>%
                ungroup() %>%
                mutate(resolution = "all", variable = "thermal_comfort_EUR")
                
                
            temp <- bind_rows(temp, thermal_comfort)
        }
        
        # Aggregating at eneff level
        det_rows <- en_stock_i %>%
            mutate("efficiency" = ifelse(eneff == "avg" | bld_age == "p5",
                bld_age, eneff)) %>%
            group_by_at(c("region_bld", "year", "efficiency")) %>%
            summarise(
                stock_building = sum(stock_M) * 1e6,
                energy_poverty_10 = sum(energy_poverty_10),
                energy_poverty_thresh = sum(energy_poverty_thresh),
                heat_kWh = sum(heat_TJ) / 3.6 * 1e6,
                heat_std_kWh = sum(heat_std_TJ) / 3.6 * 1e6,
                heat_EJ = sum(heat_TJ) / 1e6,
                hotwater_EJ = sum(hotwater_TJ) / 1e6,
                cool_EJ = sum(cool_TJ) / 1e6,
                heat_tCO2 = sum(heat_tCO2),
                hotwater_tCO2 = sum(hotwater_tCO2),
                cool_tCO2 = sum(cool_tCO2),
                cost_heat_EUR = sum(cost_energy_hh_wt),
                floor_m2 = sum(floor_Mm2) * 1e6
            ) %>%
            ungroup() %>%
            rename(resolution = efficiency) %>%
            gather(variable, value, stock_building,
                    energy_poverty_10, energy_poverty_thresh,
                    heat_kWh, heat_std_kWh, heat_EJ, 
                    hotwater_EJ, cool_EJ,
                    heat_tCO2, hotwater_tCO2, cool_tCO2,
                    cost_heat_EUR, floor_m2)
        temp <- bind_rows(temp, det_rows)
        
        det_rows <- en_stock_i %>%
            group_by_at(c("region_bld", "year", "insulation_level")) %>%
            summarise(
                stock_building = sum(stock_M) * 1e6
            ) %>%
            ungroup() %>%
            rename(resolution = insulation_level) %>%
            gather(variable, value, stock_building)
        temp <- bind_rows(temp, det_rows)

        det_rows <- en_stock_i %>%
            group_by_at(c("region_bld", "year", "energy_class")) %>%
            summarise(
                stock_building = sum(stock_M) * 1e6
            ) %>%
            ungroup() %>%
            rename(resolution = energy_class) %>%
            gather(variable, value, stock_building)
        temp <- bind_rows(temp, det_rows)


        # Adding results at building type level
        det_rows <- en_stock_i %>%
          group_by_at(c("region_bld", "year", "arch")) %>%
          summarise(
            stock_building = sum(stock_M) * 1e6,
            energy_poverty_10 = sum(energy_poverty_10),
            energy_poverty_thresh = sum(energy_poverty_thresh),
            heat_kWh = sum(heat_TJ) / 3.6 * 1e6,
            heat_std_kWh = sum(heat_std_TJ) / 3.6 * 1e6,
            heat_EJ = sum(heat_TJ) / 1e6,
            hotwater_EJ = sum(hotwater_TJ) / 1e6,
            cool_EJ = sum(cool_TJ) / 1e6,
            heat_tCO2 = sum(heat_tCO2),
            hotwater_tCO2 = sum(hotwater_tCO2),
            cool_tCO2 = sum(cool_tCO2),
            cost_heat_EUR = sum(cost_energy_hh_wt),
            floor_m2 = sum(floor_Mm2) * 1e6
          ) %>%
          ungroup() %>%
          rename(resolution = arch) %>%
          gather(variable, value, stock_building,
                 energy_poverty_10, energy_poverty_thresh,
                 heat_kWh, heat_std_kWh, heat_EJ, 
                 hotwater_EJ, cool_EJ,
                 heat_tCO2, hotwater_tCO2, cool_tCO2,
                 cost_heat_EUR, floor_m2)
        
        temp <- bind_rows(temp, det_rows)

        # Adding results at income level
        det_rows <- en_stock_i %>%
            group_by_at(c("region_bld", "year", "inc_cl")) %>%
            summarise(
                stock_building = sum(stock_M) * 1e6,
                energy_poverty_10 = sum(energy_poverty_10),
                energy_poverty_thresh = sum(energy_poverty_thresh),
                cost_energy = sum(cost_energy_hh_wt),
                income = sum(income * stock_M) * 1e6,
                ) %>%
            ungroup() %>%
            rename(resolution = inc_cl) %>%
            gather(variable, value, energy_poverty_10,
                energy_poverty_thresh,
                cost_energy, income, stock_building)

        # agg_rows <- det_rows %>%
        #     group_by_at(setdiff(names(det_rows), c("resolution", "value"))) %>%
        #     summarize(value = sum(value)) %>%
        #     ungroup() %>%
        #     mutate(resolution = "all")
        # det_rows <- bind_rows(det_rows, agg_rows)
        
        temp <- bind_rows(temp, det_rows)

        if (!is.null(new_det_i)) {
            report_new <- new_det_i %>%
                group_by_at(c("region_bld", "year", "arch")) %>%
                summarize(value = sum(n_units_fuel)) %>%
                mutate(variable = "n_new") %>%
                rename(resolution = arch)
            
            total_new <- new_det_i %>%
                group_by_at(c("region_bld", "year")) %>%
                summarize(value = sum(n_units_fuel)) %>%
                mutate(variable = "n_new") %>%
                mutate(resolution = "all")

            report_new <- bind_rows(report_new, total_new)
            temp <- bind_rows(temp, report_new)

        }
        
        # Report material demand - Embodied emissions
        if (!is.null(new_det_i) & "material" %in% report_var) {
          report_mat_in <- new_det_i %>%
            left_join(mat_int, relationship = "many-to-many") %>%
            left_join(hh_size) %>%
            left_join(floor_cap) %>%
            left_join(emission_factors_embodied) %>%
            mutate(floor_new_Mm2 =
                n_units_fuel * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
            mutate(mat_in_Mt =
                n_units_fuel * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
            mutate(mat_in_tCO2 =
                mat_in_Mt * emission_factors_embodied * 1e6) %>% # tCO2/y
            group_by_at(c("region_bld", "year", "material")) %>%
            summarize(mat_in_Mt = sum(mat_in_Mt),
                      mat_in_tCO2 = sum(mat_in_tCO2)) %>%
            ungroup %>%
            pivot_longer(cols = c(mat_in_Mt, mat_in_tCO2), names_to = "variable", values_to = "value") %>%
            rename(resolution = material) %>%
            arrange(variable,region_bld,resolution)
            
          report_mat_in <- report_mat_in %>%
            bind_rows(report_mat_in %>%
                        group_by(region_bld, year, variable) %>%
                        summarise(value = sum(value)) %>%
                        ungroup %>%
                        mutate(resolution = "all"))
          temp <- bind_rows(temp, report_mat_in)
          
        }

        if (!is.null(report_turnover)) {
            report_turnover <- report_turnover %>%
                mutate(n_out = n_dem + n_empty) %>%
                gather(variable, value, n_dem, n_empty, n_out) %>%
                mutate(resolution = "all",
                    year = yrs[i])
            temp <- bind_rows(temp, report_turnover)
        }

        # Adding EU total values
        agg <- temp %>%
            group_by_at(c("year", "variable", "resolution")) %>%
            summarize(value = sum(value)) %>%
            ungroup() %>%
            mutate(region_bld = "EU")

        temp <- bind_rows(temp, agg) %>%
            select(c("region_bld", "year", "variable",
                "resolution", "value")) %>%
            arrange(region_bld, year, variable, resolution)

        report$agg_result <- bind_rows(report$agg_result, temp)

        # Heating intensity by country
        temp_concat <- en_stock_i %>%
            group_by_at(c("region_bld", "year", "inc_cl")) %>%
            summarize(heating_intensity =
                sum(stock_M * heating_intensity, na.rm = TRUE) / sum(stock_M)) %>%
            ungroup() %>%
            rename(resolution = inc_cl) %>%
            gather(variable, value, heating_intensity)
        temp <- en_stock_i %>%
            group_by_at(c("region_bld", "year")) %>%
            summarize(heating_intensity =
                sum(stock_M * heating_intensity, na.rm = TRUE) / sum(stock_M)) %>%
            ungroup() %>%
            mutate(resolution = "all") %>%
            gather(variable, value, heating_intensity)
        temp_concat <- bind_rows(temp_concat, temp)
        
        temp <- en_stock_i %>%
            group_by_at(c("year", "inc_cl")) %>%
            summarize(heating_intensity =
                sum(stock_M * heating_intensity, na.rm = TRUE) / sum(stock_M)) %>%
            ungroup() %>%
            rename(resolution = inc_cl) %>%
            mutate(region_bld = "EU") %>%
            gather(variable, value, heating_intensity)
        temp_concat <- bind_rows(temp_concat, temp)

        temp <- en_stock_i %>%
            group_by_at(c("year")) %>%
            summarize(heating_intensity =
                sum(stock_M * heating_intensity, na.rm = TRUE) / sum(stock_M)) %>%
            ungroup() %>%
            mutate(resolution = "all") %>%
            mutate(region_bld = "EU") %>%
            gather(variable, value, heating_intensity)
        temp_concat <- bind_rows(temp_concat, temp)

        report$agg_result <- bind_rows(report$agg_result, temp_concat)

        if (!is.null(ren_det_i)) {
            det_rows <- ren_det_i %>%
                mutate(
                    total_cost =
                        cost_invest_hh * n_units_fuel * 1e3,
                    total_sub = sub_ren_hh * n_units_fuel * 1e3) %>%
                group_by_at(c("region_bld", "year", "eneff")) %>%
                summarize(
                    cost_renovation = sum(total_cost),
                    sub_renovation = sum(total_sub),
                    n_renovation = sum(n_units_fuel)) %>%
                ungroup() %>%
                rename(resolution = eneff,
                    cost_renovation_EUR = cost_renovation,
                    sub_renovation_EUR = sub_renovation
                    ) %>%
                gather(variable, value, cost_renovation_EUR,
                    sub_renovation_EUR, n_renovation)
            
            # Adding total values for all resolutions
            agg_rows <- det_rows %>%
                group_by_at(setdiff(names(det_rows),
                    c("resolution", "value"))) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(resolution = "all")

            temp <- bind_rows(det_rows, agg_rows)

            # Renovation by income class
            det_rows <- ren_det_i %>%
                mutate(to_pay = cost_invest_hh * n_units_fuel * 1e3) %>%
                group_by_at(c("region_bld", "year", "inc_cl")) %>%
                summarize(
                    n_renovation = sum(n_units_fuel),
                    to_pay_renovation = sum(to_pay)
                    ) %>%
                ungroup() %>%
                rename(resolution = inc_cl,
                    ) %>%
                gather(variable, value, n_renovation, to_pay_renovation)
            
            temp <- bind_rows(temp, det_rows)
 
            agg <- temp %>%
                group_by_at(c("year", "variable", "resolution")) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(region_bld = "EU")

            temp <- bind_rows(temp, agg) %>%
                select(c("region_bld", "year", "variable",
                    "resolution", "value")) %>%
                arrange(region_bld, year, variable, resolution)

            report$agg_result <- bind_rows(report$agg_result, temp)
        }

        if (!is.null(bld_det_i_sw)) {
            
            det_rows <- bld_det_i_sw %>%
                mutate(
                    total_cost =
                    (cost_invest_heat + sub_heat_hh) * n_units_fuel,
                    total_sub = sub_heat_hh * n_units_fuel) %>%
                group_by_at(c("region_bld", "year", "fuel_heat")) %>%
                summarize(
                    cost_heater_EUR = sum(total_cost),
                    sub_heater_EUR = sum(total_sub),
                    n_replacement = sum(n_units_fuel)) %>%
                ungroup() %>%
                rename(resolution = fuel_heat
                    ) %>%
                gather(variable, value, cost_heater_EUR,
                    sub_heater_EUR, n_replacement)
            
            # Adding total values for all resolutions
            agg_rows <- det_rows %>%
                group_by_at(setdiff(names(det_rows),
                    c("resolution", "value"))) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(resolution = "all")

            temp <- bind_rows(det_rows, agg_rows)

            # Switch by income class
            det_rows <- bld_det_i_sw %>%
                mutate(to_pay = cost_invest_heat * n_units_fuel) %>%
                group_by_at(c("region_bld", "year", "inc_cl")) %>%
                summarize(
                    to_pay_heater = sum(to_pay)
                    ) %>%
                ungroup() %>%
                rename(resolution = inc_cl,
                    ) %>%
                gather(variable, value, to_pay_heater)

            temp <- bind_rows(temp, det_rows)

            
            agg <- temp %>%
                group_by_at(c("year", "variable", "resolution")) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(region_bld = "EU")

            temp <- bind_rows(temp, agg) %>%
                select(c("region_bld", "year", "variable",
                    "resolution", "value")) %>%
                arrange(region_bld, year, variable, resolution)

            report$agg_result <- bind_rows(report$agg_result, temp)
        }

    }

    # if ("material" %in% report_var) {
    #     # Stock results - Material
    #     bld_cases_eneff <- bld_cases_fuel %>%
    #         select(-c(fuel_heat, fuel_cool)) %>%
    #         distinct()
    # 
    #     # Aggregate results at eneff level demolitions
    #     dem_eneff_i <- dem_det_age_i %>%
    #         group_by_at(setdiff(names(dem_det_age_i), c(
    #             "bld_age", "fuel_heat", "fuel_cool",
    #             "n_units_fuel_p", "n_dem", "n_empty"
    #         ))) %>%
    #         summarise(
    #             n_dem = sum(n_dem)
    #         ) %>%
    #         ungroup()
    # 
    #     # new constructions
    #     new_eneff_i <- bind_rows(new_det_age_i, new_det_slum_age_i) %>%
    #         group_by_at(setdiff(names(new_det_age_i), c(
    #             "bld_age", "fuel_heat", "fuel_cool", "n_units_fuel"
    #         ))) %>%
    #         summarise(n_new = sum(n_units_fuel)) %>%
    #         ungroup()
    # 
    #     # stock
    #     bld_eneff_i <- bld_det_i %>%
    #         group_by_at(setdiff(names(bld_det_i), c(
    #             "bld_age", "fuel_heat", "fuel_cool", "n_units_fuel"
    #         ))) %>%
    #         summarise(n_units = sum(n_units_fuel)) %>%
    #         ungroup()
    # 
    #     if (sector == "resid") {
    #         # Calculate material stock
    #         mat_stock_i <- bld_cases_eneff %>%
    #             mutate(scenario = run) %>%
    #             # mutate(ssp = ssp_r) %>%
    #             mutate(year = yrs[i]) %>%
    #             left_join(hh_size) %>%
    #             left_join(floor_cap) %>%
    #             left_join(bld_eneff_i) %>%
    #             left_join(dem_eneff_i) %>%
    #             left_join(new_eneff_i) %>%
    #             mutate(
    #                 n_units = ifelse(is.na(n_units), 0, n_units),
    #                 n_new = ifelse(is.na(n_new), 0, n_new),
    #                 n_dem = ifelse(is.na(n_dem), 0, n_dem)
    #             ) %>%
    #             filter(n_units + n_new + n_dem != 0) %>%
    #             filter(mat != "sub") %>% # Exclude slums (no material intensity data)
    #             left_join(mat_int) %>%
    #             # filter(arch != "inf") %>% # Materials not calculated for slums
    #             mutate(floor_tot_Mm2 = n_units * hh_size * floor_cap / 1e6) %>% # Mm2
    #             mutate(floor_new_Mm2 = n_new * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
    #             mutate(floor_dem_Mm2 = n_dem * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
    #             mutate(mat_stock_Mt = n_units * hh_size * floor_cap * mat_int / 1e3 / 1e6) %>% # Mt/y
    #             mutate(mat_demand_Mt = n_new * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
    #             mutate(mat_scrap_Mt = n_dem * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
    #             # Drop yr_con dimension
    #             group_by_at(
    #                 paste(c("region_gea", "region_bld",
    #                     "urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", "scenario",
    #                     "year"
    #                 ))) %>% 
    #             summarise(
    #                 floor_tot_Mm2 = sum(floor_tot_Mm2),
    #                 floor_new_Mm2 = sum(floor_new_Mm2),
    #                 floor_dem_Mm2 = sum(floor_dem_Mm2),
    #                 # mat_int = weighted.mean(mat_int, floor_tot_Mm2),
    #                 mat_stock_Mt = sum(mat_stock_Mt),
    #                 mat_demand_Mt = sum(mat_demand_Mt),
    #                 mat_scrap_Mt = sum(mat_scrap_Mt)
    #             ) %>%
    #             ungroup() %>%
    #             mutate(mat_int = 1e3 * mat_stock_Mt / floor_tot_Mm2) # Recalculate average material intensity
    #     } else {
    #         mat_stock_i <- bld_cases_eneff %>%
    #             mutate(scenario = run) %>%
    #             # mutate(ssp = ssp_r) %>%
    #             mutate(year = yrs[i]) %>%
    #             # left_join(floor_cap) %>%
    #             left_join(bld_eneff_i) %>%
    #             left_join(dem_eneff_i) %>%
    #             left_join(new_eneff_i) %>%
    #             mutate(
    #                 n_units = ifelse(is.na(n_units), 0, n_units),
    #                 n_new = ifelse(is.na(n_new), 0, n_new),
    #                 n_dem = ifelse(is.na(n_dem), 0, n_dem)
    #             ) %>%
    #             filter(n_units + n_new + n_dem != 0) %>%
    #             left_join(mat_int) %>%
    #             # filter(arch != "inf") %>% # Materials not calculated for slums
    #             mutate(floor_tot_Mm2 = n_units / 1e6) %>% # Mm2
    #             mutate(floor_new_Mm2 = n_new / stp / 1e6) %>% # Mm2/yr
    #             mutate(floor_dem_Mm2 = n_dem / stp / 1e6) %>% # Mm2/yr
    #             mutate(mat_stock_Mt = n_units * mat_int / 1e3 / 1e6) %>% # Mt
    #             mutate(mat_demand_Mt = n_new * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
    #             mutate(mat_scrap_Mt = n_dem * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
    #             # Drop yr_con dimension
    #             group_by_at(
    #                 paste(c("region_gea", "region_bld",
    #                     "urt", "clim", "inc_cl", "arch", "mat",
    #                     "eneff", "material", "scenario",
    #                     "year"
    #                 ))) %>%
    #             summarise(
    #                 floor_tot_Mm2 = sum(floor_tot_Mm2),
    #                 floor_new_Mm2 = sum(floor_new_Mm2),
    #                 floor_dem_Mm2 = sum(floor_dem_Mm2),
    #                 # mat_int = weighted.mean(mat_int, floor_tot_Mm2),
    #                 mat_stock_Mt = sum(mat_stock_Mt),
    #                 mat_demand_Mt = sum(mat_demand_Mt),
    #                 mat_scrap_Mt = sum(mat_scrap_Mt)
    #             ) %>%
    #             ungroup() %>%
    #             mutate(mat_int = 1e3 * mat_stock_Mt / floor_tot_Mm2) # Recalculate average material intensity
    #     }
    # 
    #     ## Stock results - Material - Add Cement
    # 
    #     cement_content <- 0.15 ## Cement content in concrete
    # 
    #     mat_stock_cem_i <- mat_stock_i %>%
    #         filter(material == "concrete") %>%
    #         mutate(material = "cement") %>%
    #         mutate(
    #             mat_stock_Mt = mat_stock_Mt * cement_content,
    #             mat_demand_Mt = mat_demand_Mt * cement_content,
    #             mat_scrap_Mt = mat_scrap_Mt * cement_content
    #         )
    # 
    #     mat_stock_i <- rbind(mat_stock_i, mat_stock_cem_i)
    #     report$mat_stock <- bind_rows(report$mat_stock, mat_stock_i)
    # }

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
                                        en_hh_hw_scen,
                                        emission_factors,
                                        threshold_poverty,
                                        pe_conversion_factor
                                        ) {


    test <- bld_det_i %>%
        left_join(en_hh_tot) %>%
        mutate(en_segment = ifelse(is.na(en_hh), 0, en_hh * n_units_fuel)) %>%
        group_by_at("region_bld") %>%
        summarize(en_calculation = sum(en_segment)) %>%
        ungroup() %>%
        mutate(en_calculation = en_calculation / 11630 / 1e6)

    # Aggregate at fuel level for keeping track of the stock
    bld_det_i <- bld_det_i %>%
        # Select all variables, except the ones indicated, for grouping
        group_by_at(setdiff(
            names(bld_det_i),
            c("yr_con", "n_units_fuel", "fuel")
        )) %>%
        summarise(n_units_fuel = sum(n_units_fuel)) %>%
        ungroup()

    en_stock_i <- bld_cases_fuel %>%
        mutate(scenario = run) %>%
        mutate(year = yrs[i]) %>%
        # Issue matching periods of construction when definition is different!
        left_join(bld_det_i) %>%
        filter(!is.na(n_units_fuel)) %>%
        group_by_at(setdiff(names(.),
            c("fuel", "fuel_heat", "fuel_cool", "n_units_fuel"))) %>%
        # Calculate n_units_eneff to account later for buildings with no heating
        mutate(n_units_eneff = sum(n_units_fuel)) %>%
        ungroup()

    no_heat <- en_stock_i %>%
        mutate(fuel_heat = "v_no_heat",
            fuel = "v_no_heat") %>%
        mutate(n_units_fuel = 0) %>%
        distinct()

    en_stock_i <- bind_rows(en_stock_i, no_heat)


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
            # left_join(en_m2_scen_heat) %>%
            left_join(en_m2_scen_cool) %>%
            left_join(en_hh_tot %>% rename(cost_energy_hh_wt = cost_op_wt, cost_energy_hh = cost_op))

        # Calculate the weighted median budget_share for each country
        median_budget_share <- en_stock_i %>%
            group_by(region_bld) %>%
            summarize(median_budget_share =
                weighted_median(budget_share, n_units_fuel))

        en_stock_i <- en_stock_i %>%
            mutate(insulation_level =
                ifelse(u_building < 0.5, "0-0.5",
                ifelse(u_building < 1, "0.5-1",
                ifelse(u_building < 1.5, "1-1.5",
                ifelse(u_building < 2, "1.5-2", ">2"))))) %>%
            mutate(insulation_level = ifelse(is.na(u_building),
                "no_heating", insulation_level)) %>%
            mutate(energy_class =
                ifelse(en_pe_hh_std < 50, "0-50",
                ifelse(en_pe_hh_std < 100, "50-100",
                ifelse(en_pe_hh_std < 150, "100-150",
                ifelse(en_pe_hh_std < 200, "150-200", ">200"))))) %>%
            mutate(energy_class = ifelse(is.na(en_pe_hh_std),
                "no_consumption", energy_class)) %>%
            left_join(median_budget_share) %>%
            mutate(budget_share =
                ifelse(fuel_heat == "v_no_heat", 0, budget_share)) %>%
            mutate(energy_poverty_median =
                ifelse(budget_share >= 2 * median_budget_share,
                n_units_fuel, 0)) %>%
            mutate(energy_poverty_10 =
                ifelse(budget_share >= 0.1, n_units_fuel, 0)) %>%
            left_join(threshold_poverty) %>%
            mutate(energy_poverty_thresh =
                ifelse(budget_share >= threshold_poverty,
                n_units_fuel, 0)) %>%
            mutate(cost_energy_hh_wt = cost_energy_hh_wt * n_units_fuel) %>%
            mutate(cost_energy_hh = cost_energy_hh * n_units_fuel) %>%
            left_join(en_hh_hw_scen) %>%
            # convert n. units to millions
            mutate(floor_Mm2 = n_units_fuel / 1e6 * hh_size * floor_cap) %>%
            mutate(floor_heat_Mm2 = floor_Mm2) %>%
            mutate(floor_cool_Mm2 =
                    ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)
            ) %>%
            # Converted from kWh to MJ (3.6).
            #  Housing units are in million, so results are in TJ.
            mutate(heat_TJ =
                ifelse(fuel_heat == "v_no_heat", 0,
                    en_hh * n_units_fuel / 1e6 * 3.6)) %>%
            left_join(pe_conversion_factor) %>%
            mutate(pe_conversion_factor = ifelse(is.na(pe_conversion_factor),
                1, pe_conversion_factor)) %>%
            mutate(heat_pe_TJ = heat_TJ * pe_conversion_factor) %>%
            mutate(heat_std_TJ =
                ifelse(fuel_heat == "v_no_heat", 0,
                    en_hh_std * n_units_fuel / 1e6 * 3.6)) %>%
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
                n_units_fuel / 1e6 * 3.6) %>%
            # converted from GJ/hh/yr to TJ
            mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0,
                en_dem_dhw * n_units_fuel / 1e3
            )) %>%
            # Other uses not covered for residential
            mutate(other_uses_TJ = 0) %>%
            mutate(stock_M = n_units_fuel / 1e6) %>%
            left_join(emission_factors %>% 
                        rename(emission_factors_heat = emission_factors)) %>%
            left_join(emission_factors %>% filter(fuel == "electricity") %>% select(-fuel) %>% 
                        rename(emission_factors_cool = emission_factors)) %>%
            mutate(heat_tCO2 =
                ifelse(fuel_heat == "v_no_heat", 0,
                heat_TJ * emission_factors_heat)) %>%
            mutate(hotwater_tCO2 =
                    ifelse(fuel_heat == "v_no_heat", 0,
                            hotwater_TJ * emission_factors_heat)) %>%
            mutate(cool_tCO2 = cool_TJ * emission_factors_cool) %>%
            mutate(cost_energy_hh_wt = ifelse(is.na(cost_energy_hh_wt),
                0, cost_energy_hh_wt)) %>%
            mutate(cost_energy_hh = ifelse(is.na(cost_energy_hh),
                0, cost_energy_hh)) %>%
            filter(stock_M > 0 & !is.na(stock_M)) %>%
            select_at(c("region_gea", "region_bld",
                "urt", "clim", "inc_cl", "arch", "mat",
                "eneff", "bld_age", "fuel_heat", "fuel_cool",
                "scenario", "year", "stock_M", "floor_Mm2",
                "heat_TJ", "heat_pe_TJ","heat_std_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ",
                "hotwater_TJ", "other_uses_TJ", "cost_energy_hh",
                "cost_energy_hh_wt",
                "heat_tCO2","hotwater_tCO2", "cool_tCO2", "energy_poverty_median",
                "energy_poverty_10", "energy_poverty_thresh",
                "heating_intensity", "insulation_level", "energy_class"
            ))
            
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
