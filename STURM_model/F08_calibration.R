library(dplyr)
library(data.table)
library(rootSolve)

fun_calibration_ren_shell <- function(yrs,
                          i,
                          bld_det_age_i,
                          ct_bld_age,
                          ct_ren_eneff,
                          hh_size,
                          floor_cap,
                          cost_invest_ren_shell,
                          lifetime_ren,
                          en_hh_tot,
                          rate_shell_ren,
                          ms_shell_ren,
                          stp,
                          discount_ren = 0.05) {
    
    start_calibration <- Sys.time()

    utility_ren_hh <- fun_utility_ren_shell(yrs,
                          i,
                          bld_det_age_i,
                          ct_bld_age,
                          ct_ren_eneff,
                          hh_size,
                          floor_cap,
                          cost_invest_ren_shell,
                          en_hh_tot,
                          lifetime_ren,
                          discount_ren)

    scaling_factor <- utility_ren_hh %>%
        group_by(region_bld) %>%
        summarize(min_value = min(utility_ren),
                    max_value = max(utility_ren)) %>%
        mutate(scaling_factor = (5 - (-5)) / (max_value - min_value)) %>%
        select(-c("min_value", "max_value"))

    utility_ren_hh <- utility_ren_hh %>%
        left_join(scaling_factor) %>%
        mutate(utility_ren = utility_ren * scaling_factor)

    target <- ms_shell_ren %>%
        left_join(rate_shell_ren) %>%
        mutate(value = rate_shell_ren_exo * ms_shell_ren_exo) %>%
        select(-c(rate_shell_ren_exo, ms_shell_ren_exo)) %>%
        rename(eneff_f = eneff, target = value) %>%
        mutate(target = if_else(target == 0, 0.001, target))

    market_share_agg <- function(utility_obs, constant, target) {
        ms_agg <- utility_obs %>%
            left_join(mutate(target, constant = constant),
                by = c("region_bld", "mat", "eneff_f")) %>%
            mutate(utility = utility_ren + constant) %>%
            group_by_at(setdiff(names(utility_obs),
                c("eneff_f", "utility_ren"))) %>%
            mutate(utility_exp_sum = sum(exp(utility)) + 1) %>%
            ungroup() %>%
            mutate(ms = (1 / stp) * exp(utility) / utility_exp_sum) %>%
            select(-c("utility_ren", "utility_exp_sum")) %>%
            mutate(n_renovation = ms * n_units_fuel) %>%
            group_by_at(setdiff(names(target), c("target"))) %>%
            summarize(n_renovation = sum(n_renovation),
                    n_units_fuel = sum(n_units_fuel)) %>%
            ungroup() %>%
            mutate(ms = n_renovation / n_units_fuel) %>%
            left_join(target, by = c("region_bld", "mat", "eneff_f"))
        return(ms_agg)
    }

    objective_function <- function(constant, utility, tgt) {
        ms_agg <- market_share_agg(utility, constant, tgt)
        return(ms_agg$target - ms_agg$ms)
    }

    target <- filter(target, region_bld %in% unique(utility_ren_hh$region_bld))
    
    result <- tibble(region_bld = character(),
                            mat = character(),
                            eneff_f = character(),
                            target = double(),
                            constant = double())
    for (region in unique(utility_ren_hh$region_bld)) {

        print(paste("Region:", region))
        u <- filter(utility_ren_hh, region_bld == region)
        t <- filter(target, region_bld == region)
        initial_constant <- rep(0, times = nrow(t))

        root <- multiroot(objective_function, start = initial_constant,
            maxiter = 1e3, utility = u, tgt = t)
        
        result <- bind_rows(result, mutate(t, constant = root$root))
    }
    
    result <- select(result, -c("target"))
    result <- left_join(target, result)

    report <- market_share_agg(utility_ren_hh, result$constant, target)
    ms_ini <- market_share_agg(utility_ren_hh,
        rep(0, times = nrow(target)), target) %>%
        rename(ms_ini = ms)
    report <- left_join(report, select(ms_ini,
        c("region_bld", "mat", "eneff_f", "ms_ini")))
    report <- left_join(report, result)

    write.csv(report, "STURM_output/report_calibration_ren_shell.csv")
    print("Dumped calibration results in
        STURM_output/report_calibration_ren_shell.csv")

    output <- result %>%
        select(-c("target")) %>%
        left_join(scaling_factor)

    print("End of the calibration process of home shell renovation")
    print(paste("Time to run calibration:",
    round(Sys.time() - start_calibration, 0), "seconds."))

    return(output)
    }

fun_calibration_switch_heat <- function(yrs,
                          i,
                          bld_cases_fuel,
                          ct_bld_age,
                          ct_switch_heat,
                          ct_fuel_excl_reg,
                          cost_invest_heat,
                          en_hh_tot,
                          ms_switch_fuel_exo,
                          lifetime_heat = 20,
                          discount_heat = 0.05) {
    
    start_calibration <- Sys.time()

    utility_heat_hh <- fun_utility_heat(yrs,
                        i,
                        bld_cases_fuel,
                        en_hh_tot,
                        ct_bld_age,
                        ct_switch_heat,
                        ct_fuel_excl_reg,
                        cost_invest_heat,
                        lifetime_heat,
                        discount_heat)

    scaling_factor <- utility_heat_hh %>%
        group_by(region_bld) %>%
        summarize(min_value = min(utility_heat),
                    max_value = max(utility_heat)) %>%
        mutate(scaling_factor = (5 - (-5)) / (max_value - min_value)) %>%
        select(-c("min_value", "max_value"))

    utility_heat_hh <- utility_heat_hh %>%
        select(-c("yr_con")) %>%
        left_join(scaling_factor) %>%
        mutate(utility_heat = utility_heat * scaling_factor) %>%
        filter(fuel_heat != "district_heat")

    ms_switch_fuel_exo <- ms_switch_fuel_exo %>%
        filter(ms_switch_fuel_exo > 0) %>%
        filter(region_bld %in% unique(utility_heat_hh$region_bld)) 

    market_share_agg <- function(utility_obs, constant, target) {
        ms_agg <- utility_obs %>%
            left_join(mutate(target, constant = constant),
                by = c("region_bld", "fuel_heat_f")) %>%
            mutate(constant = ifelse(is.na(constant), 0, constant)) %>%
            mutate(utility = utility_heat + constant) %>%
            group_by_at(setdiff(names(utility_obs),
                c("fuel_heat_f", "utility_heat"))) %>%
            mutate(utility_exp_sum = sum(exp(utility))) %>%
            ungroup() %>%
            mutate(ms = exp(utility) / utility_exp_sum) %>%
            select(-c("utility", "utility_exp_sum")) %>%
            mutate(n_switch = 1 / 20 * ms * n_units_fuel) %>%
            # Grouping all segments in target group
            group_by_at(setdiff(names(target), c("target"))) %>%
            summarize(n_switch_aggr = sum(n_switch)) %>%
            ungroup() %>%
            group_by_at("region_bld") %>%
            mutate(ms = n_switch_aggr / sum(n_switch_aggr)) %>%
            ungroup() %>%
            left_join(target, by = c("region_bld", "fuel_heat_f"))

        return(ms_agg)
    }

    objective_function <- function(constant, utility, tgt) {
        ms_agg <- market_share_agg(utility, constant, tgt) %>%
            filter(!is.na(target))
        return(ms_agg$target - ms_agg$ms)
    }

    result <- tibble(region_bld = character(),
                            fuel_heat_f = character(),
                            target = double(),
                            constant = double())
    for (region in unique(utility_heat_hh$region_bld)) {

        # region <- "C-WEU-FRA"

        print(paste("Region:", region))
        u <- filter(utility_heat_hh, region_bld == region)
        t <- ms_switch_fuel_exo %>%
            rename(target = ms_switch_fuel_exo) %>%
            filter(region_bld == region) %>%
            filter(target != max(target))
        initial_constant <- rep(0, times = nrow(t))
        
        # x <- market_share_agg(u, initial_constant, t)

        root <- multiroot(objective_function, start = initial_constant,
            maxiter = 1e3, utility = u, tgt = t)
        result <- bind_rows(result, mutate(t, constant = root$root))
    }
    
    target <- ms_switch_fuel_exo %>%
        rename(target = ms_switch_fuel_exo)

    result <- target %>%
        left_join(select(result, -c("target"))) %>%
        mutate(constant = ifelse(is.na(constant), 0, constant))

    ms_ini <- market_share_agg(utility_heat_hh,
        rep(0, times = nrow(target)), target) %>%
        rename(ms_ini = ms)

    x <- market_share_agg(utility_heat_hh, result$constant, target)

    report <- ms_switch_fuel_exo %>%
        left_join(market_share_agg(utility_heat_hh, result$constant, target)) %>%
        left_join(select(ms_ini, c("region_bld", "fuel_heat_f", "ms_ini"))) %>%
        left_join(result) %>%
        select(-c("ms_switch_fuel_exo")) %>%
        select(c("region_bld", "fuel_heat_f", "ms_ini",
            "target", "ms", "n_switch_aggr", "constant"))

    write.csv(report, "STURM_output/report_calibration_fuel_heat.csv")
    print("Dumped calibration results in
        STURM_output/report_calibration_fuel_heat.csv")

    output <- result %>%
        select(-c("target")) %>%
        left_join(scaling_factor)
    
    print('End of the calibration process of heat switch')
    print(paste("Time to run calibration:",
    round(Sys.time() - start_calibration, 0), "seconds."))

    return(output)

}