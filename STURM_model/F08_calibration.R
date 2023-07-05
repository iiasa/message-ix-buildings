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
                          discount_ren = 0.05) {
    
    en_hh_tot_ren_fin <- en_hh_tot %>%
        rename(eneff_f = eneff)
    en_hh_tot_ren_init <- en_hh_tot %>%
        rename(cost_op_m2_init = cost_op_m2)

    temp <- tibble(
        region_bld = unique(cost_invest_ren_shell$region_bld),
        eneff_f = "avg",
        cost_invest_ren_shell = 0)
    # Combine the new row with your tibble
    # cost_invest_ren_shell <- bind_rows(cost_invest_ren_shell, temp)

    discount_factor <- fun_discount_factor(discount_ren, lifetime_ren)

    scaling_factor <- (10 - -10) / (10000 - -10000)

    utility_ren_hh <- bld_det_age_i %>%
        left_join(ct_ren_eneff %>%
            rename(eneff = eneff_i),
            relationship = "many-to-many") %>%
        filter(!is.na(eneff_f)) %>%
        filter(eneff_f %in% c("adv", "std")) %>%
        # Attach year (in the loop)
        mutate(year = yrs[i]) %>%
        left_join(hh_size) %>%
        left_join(floor_cap) %>%
        # Add lifetime ren construction (for investment: based on loan duration)
        left_join(lifetime_ren) %>%
        left_join(discount_factor) %>%
        left_join(cost_invest_ren_shell) %>%
        # Calculate total investment costs
        mutate(cost_invest_hh =
        cost_invest_ren_shell * floor_cap * hh_size) %>%
        # Operation costs after renovation
        left_join(en_hh_tot_ren_fin) %>%
        # Operation costs before renovation
        left_join(en_hh_tot_ren_init) %>%
        # Filter out hh with no operational cost
        filter(cost_op_m2_init > 0) %>%
        # Add operative costs (total)
        mutate(cost_op_hh = cost_op_m2_init - cost_op_m2) %>%
        # mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>%
        # Calculate utility
        mutate(utility_ren =
        - cost_invest_hh + cost_op_hh * discount_factor) %>%
        mutate(utility_ren = utility_ren * scaling_factor) %>%
        # Rename eneff column
        rename(eneff_i = eneff) %>%
        select(-c(
            "hh_size", "floor_cap", "cost_invest_ren_shell",
            "cost_invest_hh", "cost_op_m2", "cost_op_m2_init",
            "cost_op_hh", "lifetime_ren", "discount_factor"))

    # write.csv(utility_ren_hh, "utility_ren_hh.csv")

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
            mutate(ms = exp(utility) / utility_exp_sum) %>%
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
    ms_ini <- market_share_agg(utility_ren_hh, rep(0, times = nrow(target)), target) %>%
        rename(ms_ini = ms)
    report <- left_join(report, select(ms_ini,
        c("region_bld", "mat", "eneff_f", "ms_ini")))
    report <- left_join(report, result)

    write.csv(report, "report_calibration.csv")

    }