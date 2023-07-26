    # Find the constant that minimizes the objective function
    print("Starting calibration (can take some t)")

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "BFGS", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "CG", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "DE", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    start_time <- Sys.time()
    result <- optim(par = initial_constant, fn = objective_function,
        method = "SANN", control = control)
     end_time <- Sys.time()
    print(paste("Elapsed time:",
        round(start_time - end_time, 0), "seconds."))

    # Retrieve the constant solution
    # write.csv(left_join(ms_i,  mutate(target, constant = result$par)), "test.csv")

    constant <- result$par

    objective_function(rep(0, times = nrow(target)))
    constant <- rep(0, times = nrow(target))

    # Find the constant that minimizes the objective function
    result <- uniroot(objective_function, interval = c(-1000, 0))
    constant_solution <- result$root

    # Print the constant solution
    cat("Constant solution:", constant_solution)




  # Create a new row with eneff_f as "avg" and cost_invest_ren_shell as 0
  temp <- tibble(
    region_bld = unique(cost_invest_ren_shell$region_bld),
    eneff_f = "avg",
    cost_invest_ren_shell = 0
  )
  # Combine the new row with your tibble
  cost_invest_ren_shell <- bind_rows(cost_invest_ren_shell, temp)

    
        parameters_heater <- fun_calibration_switch_heat(yrs,
                          i,
                          bld_cases_fuel,
                          cat$ct_bld_age,
                          cat$ct_switch_heat,
                          d$cost_invest_heat,
                          en_hh_tot,
                          d$ms_switch_fuel_exo,
                          lifetime_heat = 20,
                          discount_heat = 0.05
                        
        )

          region <- "C-WEU-FRA"
  target <- filter(shr_fuel_heat_base, region_bld == region)
  factors <- rep(0, times = nrow(target))

  target <- bld_det_age_i %>%
    group_by_at(setdiff(names(shr_fuel_heat_base),
      c("fuel_heat", "shr_fuel_heat_base"))) %>%
    summarise(n_units_eneff = sum(n_units_eneff)) %>%
    ungroup() %>%
    left_join(shr_fuel_heat_base) %>%
    mutate(n_units_fuel = n_units_eneff * shr_fuel_heat_base) %>%
    select(-c(n_units_eneff, shr_fuel_heat_base))

  temp <- bld_det_age_i %>%
    group_by_at(c("region_bld",
      intersect(names(ct_heat), names(bld_det_age_i)))) %>%
    summarise(n_units = sum(n_units_eneff)) %>%
    ungroup() %>%
    group_by_at("region_bld") %>%
    mutate(sh = n_units / sum(n_units)) %>%
    ungroup() %>%
    left_join(ct_heat, relationship = "many-to-many") %>%
    left_join(target_fuel) %>%
    mutate(n_fuels = ifelse(is.na(n_fuels), 0, n_fuels)) %>%



  objective_function <- function(factor, bld_det_age_i, tgt) {
      
      factor <- tgt %>%
        mutate(share = factor) %>%
        select(-n_fuels)

      bld <- filter(bld_det_age_i, region_bld == region) %>%
        left_join(ct_heat, relationship = "many-to-many", by = "urt") %>%
        left_join(factor, by = c("region_bld", "fuel_heat")) %>%
        mutate(n = ct_heat * abs(share) * n_units_eneff) %>%
        group_by_at(setdiff(names(tgt), c("n_fuels"))) %>%
        summarize(n = sum(n)) %>%
        ungroup() %>%
        left_join(tgt, by = c("region_bld", "fuel_heat")) %>%
        filter(!is.na(n_fuels))

      return(bld$n - bld$n_fuels)
  }


  for (region in unique(bld_det_age_i$region_bld)) {
        region <- "C-WEU-FRA"
        print(paste("Region:", region))
        t <- filter(target_fuel, region_bld == region)
        initial_factor <- rep(0.5, times = nrow(t))
        # objective_function(factor, bld_det_age_i, tgt)

        root <- multiroot(objective_function, start = initial_factor,
            maxiter = 1e3, bld_det_age_i = bld_det_age_i, tgt = t)
        
        result <- t %>%
          mutate(share = root$root)

    }