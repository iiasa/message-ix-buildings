

## key notes:


# #### function 1: calculating in-use appliances stocks with cohort, inflows, and outflows with cohort ####
fun_dynamic_stock_flow_app <- function(
                                       # current_scenario,
                                       stock_app_base,
                                       future_years,
                                       lifetime_app,
                                       n_hh,
                                       access_app,
                                       ownership_app,
                                       reuse_rate_app,
                                       energy_efficiency_class_app,
                                       yrs){
  
  # revise the input dataframes
  
  # lifetime_app_filter <- lifetime_app %>%
  #   filter(scenario == lifetime_consideration) %>%
  #   select(-scenario)
  
  lifetime_app_filter <- lifetime_app
  
  # access_app_filtered <- access_app %>%
  #   filter(scenario == 'baseline') %>%
  #   select(-scenario)
  
  access_app_filtered <- access_app %>%
    filter(type %in% c("AIR_COND", "WASH_MACH", "WASH_MACH_COM", "DISHWASHER", "FRIDGE", "ELEC_OVEN", "GAS_OVEN"))
  
  
  # ownership_app_filtered <- ownership_app %>%
  #   filter(scenario == SSP) %>%
  #   select(-scenario)
  
  ownership_app_filtered <- ownership_app
  
  # energy_efficiency_class_app <- energy_efficiency_class_app %>%
  #   rename(region = R61) %>%
  #   filter(scenario == 'baseline') %>%
  #   select(-scenario, -units)
  
  
  # future_years <- seq(2025, 2050, by = time_step)
  
  stock_app_list <- list()
  inflow_app_list <- list()
  outflow_app_list <- list()
  
  for (i in future_years) {
    if (i == min(future_years)) {
      stock_previous_app <- stock_app_base
    } else {
      stock_previous_app <- stock_app_list[[as.character(i - time_step)]]
    }
    
    #### future stocks with generation and yrs_prod: 1) Beginning-of-year BOY stock - 2) retirement within this year - 3) the abandoned / empty + 4) reuse + 5) new productions ####
    
    #### model overview - logic description
    
    ### current logic - energy driven approach as energy is associated with stocks (not inflows) - it all starts with this question: what are the energy uses in year 2050 (or any other future years)
    #@ 1 - stock_app: first calculate the detailed appliances stocks in year i - which is used also to connect to the next time step i + 1
    #@ 2 - outflow:
    #@ 3 - inflow:
    
    ### alternative logic - materials driven approach as materials are associated with flows (not stocks) - it'd start with this: what are the wastes/materials/needs for production in year 2050
    #@ 1 - outflow
    #@ 2 - inflow
    #@ 3 - stock detailed - which is used to connect to the next time step?
    
    
    ### 1. Beginning-of-year (BOY) stock
    ## Important! Unless specified, stocks in this model represent the End-of-year (EOY) stocks, which already account for all the discard and new productions within the year, and should be directly passed to the next time step unchanged
    ## BOY(i)=EOY(i-1)
    stock_begin <- stock_previous_app %>%
      mutate(year = i) %>%
      rename(n_app_begin = n_app)
    
    ### 2. The retirements - tracking generation and yrs_prod
    retd_app_i <- stock_begin %>% # Important! Retirement applies only to the BOY stock, not new productions!!
      left_join(lifetime_app_filter) %>%
      mutate(p_retirement = pweibull(year-yrs_prod, shape = shape, scale = scale)) %>%
      # mutate(p_retirement = case_when(
      #   generation == 1 ~ 1 / lifetime, # different from above, inflows from the previous time step may get retired here
      #   generation == 2 ~ 1)) %>%
      mutate(n_retired_i = p_retirement * n_app_begin) %>%
      select(-c(n_app_begin, lifetime, shape, scale, p_retirement))
    
    ### 3. The inflows - tracking generation and yrs_prod
    ## 3.1 Overall inflows = the stock increases + outflows (the retired - the reused) ~ "Stock Balance Equation" ~ not tracking generation and yrs_prod
    ## 3.1.1 stock increases = EOY stock - BOY stock
    stock_begin_aggr <- stock_begin %>%
      # group_by(region, urt, year, type, capacity, energy_label) %>%
      group_by(region, urt, year, type) %>%
      summarize(n_app_begin = sum(n_app_begin)) %>%
      ungroup()
    
    stock_end_aggr <- access_app_filtered %>%
      filter(year == i) %>%
      left_join(n_hh, by = c("region", "urt", "year")) %>%
      ## difference 1 from the main model - ownership does not have the year dimension
      # left_join(ownership_app_filtered, by = c("region", "urt", "type", "year")) %>%
      left_join(ownership_app_filtered, by = c("region", "urt", "type")) %>%
      ## difference 2 from the main model - rename value to ownership
      # rename(ownership = value) %>%
      mutate(n_app_end = round(n_hh * access * ownership, rnd)) %>%
      select(-c(n_hh, access, ownership))
    
    # stock increases
    incr_app_i <- stock_end_aggr %>%
      left_join(stock_begin_aggr) %>%
      mutate(n_increase_i = n_app_end - n_app_begin) %>%
      # select(region, urt, year, type, capacity, energy_label, n_increase_i)
      select(region, urt, year, type, n_increase_i)
    
    ## 3.1.2 outflows aggregated to calculate overall inflows including reuse and new, therefore outflows here include reuse too
    # aggregate the outflows to the same level as the stock increases
    retd_app_aggr_i <- retd_app_i %>%
      # group_by(region, urt, year, type, capacity, energy_label) %>%
      group_by(region, urt, year, type) %>%
      summarize(n_retired_i = sum(n_retired_i)) ## important, not n_outflow_i but n_retired_i
    
    ## 3.1.3 overall inflows = stock increases + actual outflows + reuse!! (important!! make sure here reuse is included!!)
    inflow_app_aggr_i <- incr_app_i %>% ## important! here actually could calculate the inflows with less granularity with no capacity and energy_label, then add them using market shares during disaggregations
      # left_join(retd_app_aggr_i, by = c("region", "urt", "type", "capacity", "energy_label", "year")) %>%
      left_join(retd_app_aggr_i, by = c("region", "urt", "type", "year")) %>%
      mutate(
        n_inflow_i = ifelse(n_increase_i + n_retired_i >= 0, n_increase_i + n_retired_i, 0),
        n_empty_i = ifelse(n_increase_i + n_retired_i >= 0, 0, -n_increase_i - n_retired_i) # n_empty differs from n_unused below, the latter being a part of n_retired
      ) %>%
      mutate(
        n_inflow_i = ifelse(abs(n_increase_i) < 1e-9 & abs(n_retired_i) < 1e-9, 0, n_inflow_i),
        n_empty_i = ifelse(abs(n_increase_i) < 1e-9 & abs(n_retired_i) < 1e-9, 0, n_empty_i)
      ) %>%
      # select(c(region, urt, year, type, capacity, energy_label, n_inflow_i, n_empty_i)) %>%
      select(c(region, urt, year, type, n_inflow_i, n_empty_i)) %>%
      # arrange(region, urt, type, capacity, energy_label)
      arrange(region, urt, type)
    
    ## 3.2 the overall inflows having same dimensions with incr_app need to be dis-aggregated into reused and new, tracking generations and yrs_prod
    # Incorrect: reuse is not known yet, so the remaining inflows cannot yet be treated as new inflows.
    # 3.2.1 inflows from the reused
    # Correction: it may seem that reuse should be calculated from inflow_app_aggr_i above; however, it should still be calculated from retd_app_i because the generation column is needed.    
    inflow_app_reuse_i <- retd_app_i %>%
      left_join(reuse_rate_app) %>%
      mutate(n_reuse_potential_i = n_retired_i * reuse_rate) %>%
      filter(generation == 1) %>% # no more than once of reuse
      
      ## calculate actual reuse based on n_increase_i
      # group_by(region, urt, year, type, capacity, energy_label, generation) %>%
      group_by(region, urt, year, type, energy_label, generation) %>%
      summarise(n_reuse_potential_i = sum(n_reuse_potential_i)) %>%
      left_join(inflow_app_aggr_i) %>%
      
      ## important!! reuse is never larger than overall inflow. when inflow is 0, no need to reuse.
      # easy mistake is: the total inflow repeats across energy labels per region
      # mutate(n_reuse_i = if_else(n_inflow_i == 0, 0, pmin(n_reuse_potential_i, n_inflow_i))) %>% # this is wrong as the total inflow repeats across energy labels
      # First, make sure the data is sorted by energy_label (A, B, C, ...)
      arrange(region, urt, year, type, energy_label) %>%
      group_by(region, urt, year, type) %>%
      mutate(
        # Calculate cumulative reuse potential as we go through each energy label
        cumulative_reuse_potential_i = cumsum(n_reuse_potential_i),
        
        # Calculate remaining inflow after allocating to more efficient labels (first A, then B, C, D, E, F, G)
        remaining_inflow_i = pmax(0, n_inflow_i - lag(cumulative_reuse_potential_i, default = 0)),
        
        # n_reuse_j is the minimum of reuse potential and remaining inflow
        n_reuse_i = if_else(n_inflow_i == 0, 0, 
                            pmin(n_reuse_potential_i, remaining_inflow_i))
      ) %>%
      ungroup() %>%
      select(-cumulative_reuse_potential_i, -remaining_inflow_i) %>%
    
      ## generation increases after reuse
      mutate(generation = generation + 1) %>%
      ## the lifetime of reused appliances is reset and starts counting from the current year
      mutate(yrs_prod = i) %>%
      # select(region, urt, year, type, capacity, energy_label, generation, yrs_prod, n_reuse_i)
      select(region, urt, year, type, energy_label, generation, yrs_prod, n_reuse_i)
    
    # inflow_app_reuse_i <- n_retired_i %>%
    #   left_join(reuse_rate_app) %>%
    #     mutate(n_reuse_i = n_retired_i * reuse_rate) %>%
    #     filter(generation == 1) %>% # no more than once of reuse
    #     mutate(generation = generation + 1) %>%
    #     mutate(yrs_prod = year) ## the lifetime of reused appliances is reset and starts counting from the current year
    
    
    # 3.2.2 new inflows
    # an aggregated reuse is temporarily needed to match the dimensions of the overall inflows
    inflow_app_reuse_aggr_i <- inflow_app_reuse_i %>%
      group_by(region, urt, year, type) %>%
      summarise(n_reuse_i = sum(n_reuse_i))
    
    inflow_app_new_i <- inflow_app_aggr_i %>%
      # left_join(inflow_app_reuse_i) %>%
      left_join(inflow_app_reuse_aggr_i) %>%
      # mutate(n_new_i = ifelse(n_inflow_i - n_reuse_i < 0, 0, n_inflow_i - n_reuse_i)) %>%
      mutate(n_new_i = n_inflow_i - n_reuse_i) %>% # the above already ensured that n_reuse_i is never larger than n_inflow_i
      select(-c(n_inflow_i, n_reuse_i)) %>%
      mutate(generation = 1, yrs_prod = i) %>%
      # select(region, urt, year, type, capacity, energy_label, generation, yrs_prod, n_new_i) %>%
      select(region, urt, year, type, generation, yrs_prod, n_new_i)
    
    # disaggregate the new inflows across energy efficiency classes, which is needed to be in the end-of-year stocks for energy calculations
    # this is using exogenous assumptions on market shares of energy efficiency classes
    
    inflow_app_new_disaggr_i <- inflow_app_new_i %>%
      left_join(energy_efficiency_class_app) %>%
      # GBR has no data so remove it
      filter(!is.na(energy_class_distribution)) %>%
      mutate(n_new_disaggr_i = n_new_i * energy_class_distribution / 100) %>%
      rename(n_inflow_i = n_new_disaggr_i) %>%
      select(region, urt, year, type, energy_label, generation, yrs_prod, n_inflow_i)
    
    # 2.2.3 bind reused and new vertically - because their generations are different
    inflow_app_generation_cohort_i <- inflow_app_reuse_i %>%
      # select(-c(n_retired_i, reuse_rate)) %>%
      rename(n_inflow_i = n_reuse_i) %>%
      bind_rows(inflow_app_new_disaggr_i) %>%
      # arrange(region, urt, year, type, capacity, energy_label, generation, yrs_prod) 
      arrange(region, urt, year, type, energy_label, generation, yrs_prod) 
    
    # test 1: inflow_aggr and inflow_detailed
    inflow_app_i_test <- inflow_app_generation_cohort_i %>%
      # group_by(region, urt, year, type, capacity, energy_label) %>%
      group_by(region, urt, year, type) %>%
      summarise(n_inflow_i_detailed = sum(n_inflow_i)) %>%
      # left_join(stock_end_aggr %>% group_by(region, type) %>% summarise(n_app_end = sum(n_app_end)))
      left_join(inflow_app_aggr_i) %>%
      mutate(difference = ifelse(abs(n_inflow_i_detailed - n_inflow_i) < 1e-9, 0, n_inflow_i_detailed - n_inflow_i))
    
    # **Test & Debug Output**
    if (all(abs(inflow_app_i_test$difference) < 1e-3 | is.na(inflow_app_i_test$difference))) {
      cat("\n====================================\n")
      cat("✅ Good: All  overall inflows match disaggregated  inflows in year", i, "\n")
      cat("====================================\n\n")
    } else {
      cat("\n❌ WARNING: Some discrepancies found in the  inflow comparison in year", i, "\n")
      cat("====================================\n")
      inflow_app_i_test %>% filter(difference != 0 | is.na(difference)) %>% print()
      cat("====================================\n\n")
    }
    
    
    ## 3. future stocks = (✅the beginning stocks -✅retirements +✅reuse -✅empty +✅new productions) - all with generation and yrs_prod
    # first, need to allocate the overall empty ones across the beginning stocks - older yrs_prod, lower-efficiency, and higher generations are prioritized
    allocate_empty_stock_vec <- function(data) {
      data <- data %>%
        arrange(yrs_prod, desc(energy_label), desc(generation))
      
      rem <- dplyr::first(data$n_empty_i)
      prev_cum <- c(0, head(cumsum(data$n_in_service), -1))
      remaining_before_row <- pmax(rem - prev_cum, 0)
      data$n_empty_alloc <- pmin(data$n_in_service, remaining_before_row)
      data
    }
    
    empty_disaggr_i <- inflow_app_aggr_i %>%
      select(-n_inflow_i) %>%
      ## here not allocate across the stock_begin, instead allocate across the not-retired
      left_join(stock_begin) %>%
      left_join(lifetime_app_filter) %>%
      mutate(p_retirement = pweibull(year-yrs_prod, shape = shape, scale = scale)) %>%
      mutate(n_retired_i = p_retirement * n_app_begin) %>%
      mutate(n_in_service = n_app_begin - n_retired_i) %>%
      
      # Sort so that older `yrs_prod` and higher `generation` come first
      # arrange(region, urt, year, type, capacity, energy_label, yrs_prod, desc(generation)) %>%
      arrange(region, urt, year, type, yrs_prod) %>%
      # group_by(region, urt, year, type, capacity, energy_label) %>%
      group_by(region, urt, year, type) %>%
      
      # Split groups and apply allocation function
      group_split() %>%
      map_dfr(allocate_empty_stock_vec) %>%
      ungroup() %>%
      select(-c(lifetime, shape, scale, p_retirement, n_empty_i))
    
    
    ## now build the detailed stocks (end_year_stocks)
    stock_app_disaggr_i <- stock_begin %>%
      # join retirements (outflows and reuse) - reuse is 'removed' here and then 'rejoined' in a new form!!
      left_join(retd_app_i) %>%
      # join empty ones
      left_join(empty_disaggr_i) %>%
      # minus retirement and empty ones - there might be overlap when empty is very large
      # empty can be as large as begin stock, retirement is a part of the begin stock
      mutate(n_app = pmax(n_app_begin - n_retired_i - n_empty_alloc, 0)) %>%
      # select(region, urt, year, type, capacity, energy_label, generation, yrs_prod, n_app) %>%
      select(region, urt, year, type, energy_label, generation, yrs_prod, n_app) %>%
      # plus inflow including both reused and brand new - using bind_rows not left_join, why?
      bind_rows(inflow_app_generation_cohort_i %>% rename(n_app = n_inflow_i)) %>%
      # group_by(region, urt, year, type, capacity, energy_label, generation, yrs_prod) %>%
      group_by(region, urt, year, type, energy_label, generation, yrs_prod) %>%
      summarise(n_app = sum(n_app)) %>%
      ungroup()
    
    # important test: compare the aggregated stocks and the disaggregated stocks
    stock_app_i_test <- stock_app_disaggr_i %>%
      # group_by(region, urt, year, type, capacity, energy_label) %>%
      group_by(region, urt, year, type) %>% # no energy_label in the group
      # group_by(region, type) %>%
      summarise(n_app = sum(n_app)) %>%
      # left_join(stock_end_aggr %>% group_by(region, type) %>% summarise(n_app_end = sum(n_app_end)))
      left_join(stock_end_aggr) %>%
      mutate(difference = ifelse(abs(n_app_end - n_app) < 1e-3, 0, n_app_end - n_app))
    
    # **Test & Debug Output**
    if (all(abs(stock_app_i_test$difference) < 1e-5 | is.na(stock_app_i_test$difference))) {
      cat("\n====================================\n")
      cat("✅ Good: All aggregated stocks match disaggregated stocks in year", i, "\n")
      cat("====================================\n\n")
    } else {
      cat("\n❌ WARNING: Some discrepancies found in the stock comparison in year", i, "\n")
      cat("====================================\n")
      stock_app_i_test %>% filter(difference != 0 | is.na(difference)) %>% print()
      cat("====================================\n\n")
    }
    
    # 
    #### inflow calculations - already done above ####
    inflow_app_output_i <- inflow_app_generation_cohort_i %>%
      rename(n_inflow = n_inflow_i)
    
    #### outflows by cohort - two versions ####
    # outflow_app <- retd_app_to_reuse %>%
    #   rename(n_outflow_nominal = n_retired) %>% # total outflow including the those recollected
    #   mutate(n_outflow_actual = n_outflow_nominal - n_reuse_potential) %>% # actual outflow not including the recollected (reuse + recollected but unused)
    #   select(-c(reuse_rate, n_reuse_potential))
    
    # below is new, above is wrong! - outflow should be the retirement of the current stocks rather than the previous stocks
    # outflow_app <- stock_app %>%
    #   select(-c(n_retired, reuse_rate)) %>%
    #   left_join(lifetime_app) %>%
    #   mutate(p_retirement = case_when(
    #     generation == 1 ~ 1 / lifetime,
    #     generation == 2 ~ 1)) %>%
    #   mutate(n_outflow_nominal = ifelse(n_app > 0,
    #                                     round(p_retirement * n_app, rnd), 0)) %>%
    #   mutate(n_reuse_potential = n_outflow_nominal * reuse_rate) %>%
    #   mutate(n_outflow_actual = n_outflow_nominal - n_reuse_potential)
    
    #### outflows by cohort - two versions - with/without reuse ####
    ## the key is to consider the reuse without losing resolution - disaggregate reuse to have the same resolution
    ## disaggregate reuse across yrs_prod
    ## the total quantity of reuse needed to be calculated at the more aggregated level, but this quantity will have to come from the retirement
    allocate_reuse <- function(data) {
      data <- data %>%
        arrange(desc(yrs_prod))  # Sort within each group
      
      remaining_reuse <- first(data$n_reuse_i)
      
      for (i in seq_len(nrow(data))) {
        data$reuse_alloc[i] <- min(data$n_retired_i[i], remaining_reuse)
        remaining_reuse <- max(remaining_reuse - data$reuse_alloc[i], 0)
      }
      
      return(data)
    }
    
    retd_reuse_disaggr_i <- inflow_app_reuse_i %>%
      # as part of the retirement, reuse has a generation of 1, not 2
      mutate(generation = 1) %>%
      # as part of the retirement, reuse's yrs_prod is not i, and needs to be disaggregated across original yrs_prod
      # meaning that, the total quantity of reuse needed to be calculated at the more aggregated level, but this quantity will have to come from the retirement
      select(-yrs_prod) %>%
      # 
      left_join(retd_app_i) %>%
      
      # mutate(n_retired_i = 50000) %>%
      
      # Sort so latest yrs_prod is first for correct progressive allocation
      # arrange(region, urt, year, type, capacity, energy_label, generation, desc(yrs_prod)) %>%
      # group_by(region, urt, year, type, capacity, energy_label, generation) %>%
      arrange(region, urt, year, type, energy_label, generation, desc(yrs_prod)) %>%
      group_by(region, urt, year, type, energy_label, generation) %>%
      
      group_split() %>%  # Splits groups while keeping full data
      map_dfr(allocate_reuse) %>%  # Apply function to each group separately
      ungroup()
    
    
    outflow_app_i <- retd_reuse_disaggr_i %>%
      # total retirements - including the reused
      rename(n_outflow_nominal = n_retired_i) %>%
      # actual outflow excluding the recollected
      mutate(n_outflow_actual = n_outflow_nominal - reuse_alloc) %>%
      # a percentage of retired devices typically transitions into “hibernating” or “storage” stocks
      mutate(n_outflow_store = n_outflow_nominal * 0.1) %>%
      select(-c(n_reuse_i, reuse_alloc))
    
    #### building a list for stocks / inflows / outflows across different years
    stock_app_list[[as.character(i)]] <- stock_app_disaggr_i
    inflow_app_list[[as.character(i)]] <- inflow_app_output_i
    outflow_app_list[[as.character(i)]] <- outflow_app_i
    
    # print(paste0("Future stocks and flows - under scenario ", as.character(current_scenario), " in year", as.character(i)))
    print(paste0("Future stocks and flows - under scenario ", as.character(scenario_selection), " in year ", as.character(i)))
    
    
  }
  
  stock_app_future_years <- do.call(rbind, stock_app_list)
  rownames(stock_app_future_years) <- NULL # Reset the row names
  
  inflow_app_future_years <- do.call(rbind, inflow_app_list)
  rownames(inflow_app_future_years) <- NULL # Reset the row names
  
  outflow_app_future_years <- do.call(rbind, outflow_app_list)
  rownames(outflow_app_future_years) <- NULL # Reset the row names
  
  # Combine the outputs into a list
  output <- list(
    stock_app_future_years = stock_app_future_years,
    inflow_app_future_years = inflow_app_future_years,
    outflow_app_future_years = outflow_app_future_years
  )
  
  return(output)
  
  
  
}



