library(dplyr)
library(data.table)
library(rootSolve)

ms_agg_ren_shell <- function(utility_obs, constant, scale, stp,
    barrier_rent, barrier_mfh) {

    ren_det <- utility_obs %>%
        left_join(constant,
            by = c("region_bld", "mat", "eneff_f")) %>%
        left_join(scale,
            by = c("region_bld")) %>%
        left_join(barrier_rent,
            by = c("region_bld")) %>%
        left_join(barrier_mfh,
            by = c("region_bld")) %>%
        mutate(utility = utility_ren * scale + constant) %>%
        mutate(utility = ifelse(arch == "mfh",
            utility - barrier_mfh, utility)) %>%
        mutate(utility = ifelse(tenr == "rent",
            utility - barrier_rent, utility)) %>%
        group_by_at(setdiff(names(utility_obs),
            c("eneff_f", "utility_ren"))) %>%
        mutate(utility_exp_sum = sum(exp(utility)) + 1) %>%
        ungroup() %>%
        mutate(ms = (1 / stp) * exp(utility) / utility_exp_sum) %>%
        select(-c("utility_ren", "utility_exp_sum")) %>%
        mutate(n_renovation = ms * n_units_fuel)
    
    elasticity <- ren_det %>%
        filter(eneff_f == "std") %>%
        mutate(elasticity =
            - scale * scaling_factor * cost_invest_hh * (1 - ms)) %>%
        group_by_at("region_bld") %>%
        summarize(elasticity =
            sum(n_renovation * elasticity) / sum(n_renovation)) %>%
        ungroup()

    ms_agg <- ren_det %>%
        group_by_at(setdiff(names(constant), c("constant"))) %>%
        summarize(n_renovation = sum(n_renovation),
                n_units_fuel = sum(n_units_fuel)) %>%
        ungroup() %>%
        mutate(ms = n_renovation / n_units_fuel)
    
    ren_det <- ren_det %>%
        group_by_at(setdiff(names(ren_det),
            c("n_units_fuel", "floor_size", "eneff_f", "n_renovation",
                "cost_invest_hh", "en_hh_init", "constant",
                "en_saving", "cost_op_saving",
                "ms", "utility", "scale", "scaling_factor"))) %>%
        summarize(n_renovation = sum(n_renovation),
            n_units_fuel = first(n_units_fuel)) %>%
        ungroup()
    
    # Multi-family dwellings
    temp <- ren_det %>%
        group_by_at(c("region_bld", "arch")) %>%
        summarize(rate = sum(n_renovation) / sum(n_units_fuel)) %>%
        ungroup()
    ratio_mfh <- filter(temp, arch == "mfh")$rate / filter(temp, arch == "sfh")$rate

    # Rental dwellings
    temp <- ren_det %>%
        group_by_at(c("region_bld", "tenr")) %>%
        summarize(rate = sum(n_renovation) / sum(n_units_fuel)) %>%
        ungroup()
    ratio_rent <- filter(temp, tenr == "rent")$rate / filter(temp, tenr == "own")$rate


    output <- list(
        ms_agg = ms_agg,
        elasticity = elasticity$elasticity,
        ratio_mfh = ratio_mfh,
        ratio_rent = ratio_rent

    )
    return(output)
}

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
                          coeff_barriers_mfh,
                          coeff_barriers_rent,
                          stp,
                          path_out,
                          discount_ren) {
    
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
                          discount_ren,
                          full = TRUE)
    
    report <- utility_ren_hh %>%
        rename(upfront_cost = cost_invest_hh) %>%
        mutate(upfront_cost = upfront_cost * 1e3) %>%
        rename(profitability = utility_ren) %>%
        mutate(profitability = profitability * 1e3) %>%
        rename(energy_savings = en_saving)

    write.csv(report, paste0(path_out, "report_static_ren_shell.csv"))
    print("Dumped static analysis results in
        STURM_output/report_static_ren_shell.csv")
    
    # Initial guess for the optimization
    scaling_factor <- utility_ren_hh %>%
        group_by(region_bld) %>%
        summarize(min_value = min(utility_ren),
                  max_value = max(utility_ren)) %>%
        mutate(scaling_factor = ifelse(max_value > 0,
            (4 - (-4)) / (max_value - min_value),
            4 / abs(min_value))) %>%
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

    objective_function <- function(x, utility, tgt, stp, elasticity, b_mfh, b_rent) {
        constant <- x[1:nrow(tgt)]
        scale <- x[nrow(tgt) + 1]

        if (b_rent != 1) {
            barrier_rent <- x[nrow(tgt) + 2]
            if (b_mfh != 1) {
                barrier_mfh <- x[nrow(tgt) + 3]
            } else {
                barrier_mfh <- 0
            }
        } else {
            barrier_rent <- 0
            if (b_mfh != 1) {
                barrier_mfh <- x[nrow(tgt) + 2]
            } else {
                barrier_mfh <- 0
            }
        }

        cst <- tgt %>%
            mutate(constant = constant) %>%
            select(-c("target"))

        scale <- tibble(
            region_bld = unique(tgt$region_bld),
            scale = scale)
        barrier_rent <- tibble(
            region_bld = unique(tgt$region_bld),
            barrier_rent = barrier_rent)
        barrier_mfh <- tibble(
            region_bld = unique(tgt$region_bld),
            barrier_mfh = barrier_mfh)

         
        output <- ms_agg_ren_shell(utility, cst, scale, stp,
            barrier_rent, barrier_mfh)

        ms_agg <- output$ms_agg %>%
            left_join(tgt, by = c("region_bld", "mat", "eneff_f"))
        

        objective <- c(
            ms_agg$target - ms_agg$ms,
            output$elasticity - elasticity
            )

        if (b_rent != 1) {
            objective <- c(objective, output$ratio_rent - b_rent)
            if (b_mfh != 1) {
                objective <- c(objective, output$ratio_mfh - b_mfh)
            }
        } else {
            if (b_mfh != 1) {
                objective <- c(objective, output$ratio_mfh - b_mfh)
            }
        }

        return(objective)
    }

    target <- filter(target, region_bld %in% unique(utility_ren_hh$region_bld))
    
    result <- tibble(region_bld = character(),
                            mat = character(),
                            eneff_f = character(),
                            target = double(),
                            constant = double(),
                            scale = double(),
                            barrier_rent = double(),
                            barrier_mfh = double())
    for (region in unique(utility_ren_hh$region_bld)) {

        print(paste("Region:", region))

        u <- filter(utility_ren_hh, region_bld == region)
        t <- filter(target, region_bld == region)
        x <- rep(0, times = nrow(t))
        x <- c(x, 1)

        # We add constraint if barriers if ratio are less than 1
        b_rent <- filter(coeff_barriers_rent,
            region_bld == region)$barriers_rent
        if (b_rent != 1) {
            x <- c(x, 0)
        }
        b_mfh <- filter(coeff_barriers_mfh,
            region_bld == region)$barriers_mfh
        if (b_mfh != 1) {
            x <- c(x, 0)
        }

        root <- multiroot(objective_function, start = x,
            maxiter = 1e3, utility = u, tgt = t, stp = stp,
            b_mfh = b_mfh, b_rent = b_rent,
            elasticity = -1)

        constant <- root$root[1:nrow(t)]
        scale <- root$root[nrow(t) + 1]
        if (b_rent != 1) {
            barrier_rent <- root$root[nrow(t) + 2]
            if (b_mfh != 1) {
                barrier_mfh <- root$root[nrow(t) + 3]
            } else {
                barrier_mfh <- 0
            }
        } else {
            barrier_rent <- 0
            if (b_mfh != 1) {
                barrier_mfh <- root$root[nrow(t) + 2]
            } else {
                barrier_mfh <- 0
            }
        }

        result <- bind_rows(result,
            mutate(t, constant = constant, scale = scale,
                barrier_rent = barrier_rent, barrier_mfh = barrier_mfh))
    }

    constant <- select(result,
        -c("target", "scale", "barrier_rent", "barrier_mfh"))
    scale <- result %>%
        group_by(region_bld) %>%
        summarise(scale = first(scale)) %>%
        mutate(scale = ifelse(is.na(scale), 1, scale))
    barrier_rent <- result %>%
        group_by(region_bld) %>%
        summarise(barrier_rent = first(barrier_rent)) %>%
        mutate(barrier_rent = ifelse(is.na(barrier_rent), 0, barrier_rent))
    barrier_mfh <- result %>%
        group_by(region_bld) %>%
        summarise(barrier_mfh = first(barrier_mfh)) %>%
        mutate(barrier_mfh = ifelse(is.na(barrier_mfh), 0, barrier_mfh))

    ms_ini <- ms_agg_ren_shell(utility_ren_hh,
        mutate(constant, constant = 0), mutate(scale, scale = 1), stp,
        mutate(barrier_rent, barrier_rent = 0),
        mutate(barrier_mfh, barrier_mfh = 0))$ms_agg %>%
        rename(ms_ini = ms) %>%
        select(-c("n_renovation", "n_units_fuel"))

    ms <- ms_agg_ren_shell(utility_ren_hh, constant, scale, stp,
        barrier_rent, barrier_mfh)$ms_agg %>%
        left_join(target) %>%
        left_join(constant) %>%
        left_join(barrier_rent) %>%
        left_join(barrier_mfh) %>%
        left_join(ms_ini) %>%
        left_join(scale) %>%
        left_join(scaling_factor %>%
            rename(scale_ini = scaling_factor)) %>%
        mutate(scaling_factor = scale_ini * scale) %>%
        select(c("region_bld", "mat", "eneff_f", "ms_ini",
            "target", "ms", "constant", "barrier_rent", "barrier_mfh",
            "scale", "scale_ini", "scaling_factor"))

    write.csv(ms, paste0(path_out, "report_calibration_ren_shell.csv"))
    print("Dumped calibration results in
        STURM_output/report_calibration_ren_shell.csv")

    output <- ms %>%
        select(c("region_bld", "mat", "eneff_f", "constant",
            "barrier_rent", "barrier_mfh")) %>%
        left_join(scale) %>%
        left_join(scaling_factor %>%
            rename(scale_ini = scaling_factor)) %>%
        mutate(scaling_factor = scale_ini * scale) %>%
        select(-c("scale", "scale_ini"))

    write.csv(output %>% rename(value = constant),
        paste0(path_out, "parameters_renovation.csv"), row.names = FALSE)
    print("End of the calibration process of home shell renovation")
    print(paste("Time to run calibration:",
    round(Sys.time() - start_calibration, 0), "seconds."))

    return(output)
    }

fun_calibration_switch_heat <- function(yrs,
                          i,
                          bld_stock,
                          ct_bld_age,
                          ct_switch_heat,
                          ct_fuel_excl_reg,
                          cost_invest_heat,
                          en_hh_tot,
                          ms_switch_fuel_exo,
                          ct_heat,
                          ct_heat_new,
                          path_out,
                          discount_heat,
                          lifetime_heat = 20,
                          inertia = NULL) {
    
    start_calibration <- Sys.time()

    utility_heat_hh <- fun_utility_heat(yrs,
                        i,
                        bld_stock,
                        en_hh_tot,
                        ct_switch_heat,
                        ct_fuel_excl_reg,
                        ct_heat,
                        ct_heat_new,
                        cost_invest_heat,
                        lifetime_heat,
                        discount_heat,
                        inertia = inertia,
                        full = TRUE)

    scaling_factor <- utility_heat_hh %>%
        group_by(region_bld) %>%
        summarize(min_value = min(utility_heat),
                  max_value = max(utility_heat)) %>%
        mutate(scaling_factor = ifelse(max_value > 0,
            (4 - (-4)) / (max_value - min_value),
            4 / abs(min_value))) %>%
        select(-c("min_value", "max_value"))

    utility_heat_hh <- utility_heat_hh %>%
        select(-c("yr_con")) %>%
        left_join(scaling_factor) %>%
        mutate(utility_heat = utility_heat * scaling_factor)

    ms_switch_fuel_exo <- ms_switch_fuel_exo %>%
        filter(ms_switch_fuel_exo > 0) %>%
        filter(region_bld %in% unique(utility_heat_hh$region_bld))

    market_share_agg <- function(utility_obs, constant, scale) {

        switch_det <- utility_obs %>%
            left_join(constant,
                by = c("region_bld", "fuel_heat_f")) %>%
            left_join(scale, by = c("region_bld")) %>%
            mutate(constant = ifelse(is.na(constant), 0, constant)) %>%
            mutate(utility = utility_heat * scale + constant) %>%
            group_by_at(setdiff(names(utility_obs),
                c("fuel_heat_f", "utility_heat", "cost_invest_heat"))) %>%
            mutate(utility_exp_sum = sum(exp(utility))) %>%
            ungroup() %>%
            mutate(ms = exp(utility) / utility_exp_sum) %>%
            select(-c("utility", "utility_exp_sum")) %>%
            mutate(n_switch = 1 / 20 * ms * n_units_fuel)


        elasticity <- switch_det %>%
            filter(fuel_heat_f == "heat_pump") %>%
            mutate(elasticity =
                - scale * scaling_factor * cost_invest_heat * (1 - ms) / 1e3) %>%
            group_by_at("region_bld") %>%
            summarize(elasticity =
                sum(n_switch * elasticity) / sum(n_switch)) %>%
            ungroup()

        ms_agg <- switch_det %>%
            # Grouping all segments in target group
            group_by_at(setdiff(names(constant), c("constant"))) %>%
            summarize(n_switch_aggr = sum(n_switch)) %>%
            ungroup() %>%
            group_by_at("region_bld") %>%
            mutate(ms = n_switch_aggr / sum(n_switch_aggr)) %>%
            ungroup()

        output <- list(ms_agg = ms_agg,
            elasticity = elasticity$elasticity)

        return(output)
    }

    objective_function <- function(x, utility, tgt) {

        constant <- x[1:length(x) - 1]
        scale <- x[length(x)]

        constant <- tgt %>%
            filter(target != max(target)) %>%
            mutate(constant = constant) %>%
            select(-c("target"))
        
        scale <- tibble(
            region_bld = unique(utility$region_bld),
            scale = scale
        )

        output <- market_share_agg(utility, constant, scale)

        ms_agg <- output$ms_agg %>%
            left_join(tgt, by = c("region_bld", "fuel_heat_f")) %>%
            filter(target != max(target))

        objective <- c(ms_agg$target - ms_agg$ms, output$elasticity - (-1))

        return(objective)
    }

    result <- tibble(region_bld = character(),
                    fuel_heat_f = character(),
                    target = double(),
                    constant = double(),
                    scale = double())
    for (region in unique(utility_heat_hh$region_bld)) {
        print(paste("Region:", region))
        u <- filter(utility_heat_hh, region_bld == region)
        t <- ms_switch_fuel_exo %>%
            rename(target = ms_switch_fuel_exo) %>%
            filter(region_bld == region)
        x <- c(rep(0, times = nrow(t) - 1))
        x <- c(x, 1)
        # utility <- u
        # tgt <- t

        root <- multiroot(objective_function, start = x,
            utility = u, tgt = t)
        
        constant <- root$root[1:length(root$root) - 1]
        scale <- root$root[length(root$root)]

        temp <- t %>%
            filter(target != max(target)) %>%
            mutate(constant = constant) %>%
            right_join(t, by = c("region_bld", "fuel_heat_f", "target")) %>%
            mutate(constant = ifelse(is.na(constant), 0, constant)) %>%
            mutate(scale = scale)

        result <- bind_rows(result, temp)
    }
    
    constant <- select(result, -c("target", "scale"))
    scale <- result %>%
        group_by_at("region_bld") %>%
        summarize(scale = first(scale)) %>%
        mutate(scale = ifelse(is.na(scale), 1, scale)) %>%
        ungroup()

    ms_ini <- market_share_agg(utility_heat_hh,
        mutate(constant, constant = 0), mutate(scale, scale = 1))$ms_agg %>%
        rename(ms_ini = ms) %>%
        select(-c("n_switch_aggr"))

    ms <- market_share_agg(utility_heat_hh, constant, scale)$ms_agg %>%
        select(-c("n_switch_aggr"))

    stock <- bld_stock %>%
        group_by_at(c("region_bld", "fuel_heat")) %>%
        summarize(n_units_fuel = sum(n_units_fuel)) %>%
        ungroup() %>%
        group_by_at(c("region_bld")) %>%
        mutate(n_units_fuel = n_units_fuel / sum(n_units_fuel)) %>%
        ungroup() %>%
        filter(!is.na(fuel_heat)) %>%
        rename(fuel_heat_f = fuel_heat) %>%
        rename(share_stock = n_units_fuel)

    report <- stock %>%
        left_join(result) %>%
        left_join(ms) %>%
        left_join(ms_ini) %>%
        left_join(scale) %>%
        left_join(scaling_factor %>%
            rename(scale_ini = scaling_factor)) %>%
        mutate(scaling_factor = scale_ini * scale) %>%
        select(c("region_bld", "fuel_heat_f", "share_stock", "ms_ini",
            "target", "ms", "constant", "scaling_factor"))
    report[is.na(report)] <- 0

    write.csv(report, paste0(path_out, "report_calibration_fuel_heat.csv"))
    print("Dumped calibration results in
        report_calibration_fuel_heat.csv")

    output <- result %>%
        select(-c("target")) %>%
        left_join(scale) %>%
        left_join(scaling_factor %>%
            rename(scale_ini = scaling_factor)) %>%
        mutate(scaling_factor = scale_ini * scale) %>%
        select(-c("scale", "scale_ini"))

    write.csv(output %>% rename(value = constant),
        paste0(path_out, "parameters_heater.csv"), row.names = FALSE)
    
    print('End of the calibration process of heat switch')
    print(paste("Time to run calibration:",
    round(Sys.time() - start_calibration, 0), "seconds."))

    return(output)

}


fun_calibration_consumption <- function(bld_det_ini,
                                        en_hh_tot,
                                        hh_size,
                                        floor_cap,
                                        en_consumption,
                                        path_out
                                        ) {

    # Calibration of energy demand
    bld_energy <- bld_det_ini %>%
      left_join(en_hh_tot) %>%
      left_join(hh_size) %>%
      left_join(floor_cap) %>%
      mutate(floor_size = floor_cap * hh_size * n_units_fuel) %>%
      mutate(en_segment = en_hh * n_units_fuel,
        en_std_segment = en_hh_std * n_units_fuel)

    # Calculate the weighted median budget_share for each country
    median_budget_share <- bld_energy %>%
      group_by(region_bld) %>%
      summarize(median_budget_share =
        weighted_median(budget_share, n_units_fuel))

    bld_energy <- bld_energy %>%
      left_join(median_budget_share) %>%
      mutate(energy_poverty =
        ifelse(budget_share >= 2 * median_budget_share, n_units_fuel, 0)) %>%
      group_by_at(setdiff(names(en_consumption), "en_consumption")) %>%
      summarize(en_calculation = sum(en_segment),
        en_calculation_std = sum(en_std_segment),
        n_units_fuel = sum(n_units_fuel),
        floor_size = sum(floor_size),
        en_poverty = sum(energy_poverty)) %>%
      ungroup() %>%
      # Conversion from kWh to ktoe
      mutate(
        en_calculation_dw = en_calculation / n_units_fuel,
        en_calculation_unit = en_calculation / floor_size,
        en_calculation_std_unit = en_calculation_std / floor_size,
        en_calculation = en_calculation / 11630 / 1e3,
        en_calculation_std = en_calculation_std / 11630 / 1e3,
        n_units_fuel = n_units_fuel / 1e6,
        en_poverty = en_poverty / 1e6,
        sh_en_poverty = en_poverty / n_units_fuel,
        floor_size = floor_size / 1e6) %>%
      left_join(en_consumption) %>%
      mutate(shr_en = en_consumption / en_calculation)
    write.csv(bld_energy, paste0(path_out, "calibration_consumption.csv"))

    shr_en <- select(bld_energy, -c("en_consumption", "en_calculation",
      "n_units_fuel", "floor_size", "en_calculation_std",
      "en_calculation_unit",  "en_calculation_std_unit",
      "en_calculation_dw", "en_poverty", "sh_en_poverty"))
    return(shr_en)
}