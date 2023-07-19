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
    print("End of the calibration process of home shell renovation")
    result <- select(result, -c("target"))
    result <- left_join(target, result)

    report <- market_share_agg(utility_ren_hh, result$constant, target)
    ms_ini <- market_share_agg(utility_ren_hh,
        rep(0, times = nrow(target)), target) %>%
        rename(ms_ini = ms)
    report <- left_join(report, select(ms_ini,
        c("region_bld", "mat", "eneff_f", "ms_ini")))
    report <- left_join(report, result)

    write.csv(report, "report_calibration.csv")

    output <- result %>%
        select(-c("target")) %>%
        left_join(scaling_factor)

    return(output)
    }

fun_calibration_switch_heat <- function(yrs,
                          i,
                          bld_cases_fuel,
                          ct_bld_age,
                          ct_switch_heat,
                          cost_invest_heat,
                          en_hh_tot,
                          ms_switch_fuel_exo,
                          lifetime_heat = 20,
                          discount_heat = 0.05) {

    utility_heat_hh <- fun_utility_heat(yrs,
                        i,
                        bld_cases_fuel,
                        en_hh_tot,
                        ct_bld_age,
                        ct_switch_heat,
                        cost_invest_heat,
                        lifetime_heat,
                        discount_heat)
}