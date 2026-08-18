# Fuel-switching decision for the zhu renovation and fuel-switching
# structure. This module calculates probabilities only; stock changes are
# applied later in F06_stock_dyn_complete_rev.R

fun_sw_pref_zhu <- function(yrs, i,
                            renov_status,
                            hh_income,
                            cost_inv_sw_heat,
                            cost_om_sw_heat,
                            cost_fuel_sw_heat,
                            eff_sw_heat,
                            capacity_factor_sw_heat,
                            discount_rate_sw_heat,
                            lifetime_sw,
                            cost_int_sw_heat_base,
                            alpha_sw_heat,
                            eta_sw_heat,
                            price_en,
                            geo_data,
                            geo_level,
                            geo_level_aggr,
                            nu = 8) {

  print(paste0(
    "Running endogenous fuel preference (",
    renov_status,
    ") - year ",
    yrs[i]
  ))

  if (!renov_status %in% c("renov", "norenov")) {
    stop("renov_status must be either 'renov' or 'norenov'")
  }

  current_year <- yrs[i]
  income_reference_year <- 2020
  selected_regions <- geo_data %>%
    distinct(across(all_of(geo_level_aggr))) %>%
    pull(all_of(geo_level_aggr))

  # Y_ref: fixed 2020 geometric-mean income by region and urban/rural group.
  # Income deciles have equal weights in the supplied income table.
  income_ref_i <- hh_income %>%
    filter(
      year == income_reference_year,
      .data[[geo_level_aggr]] %in% selected_regions
    ) %>%
    group_by(across(all_of(c(geo_level_aggr, "urt")))) %>%
    summarise(
      income_ref = exp(mean(log(hh_income))),
      .groups = "drop"
    )

  income_i <- hh_income %>%
    filter(
      year == current_year,
      .data[[geo_level_aggr]] %in% selected_regions
    ) %>%
    left_join(
      income_ref_i,
      by = c(geo_level_aggr, "urt")
    )

  # Keep the fuel-cost level used for the 2020 calibration, while using
  # price_en only to evolve that level over time. Heat pumps follow the
  # electricity-price index.
  price_by_year <- price_en %>%
    filter(year %in% c(income_reference_year, current_year)) %>%
    left_join(
      geo_data %>%
        select(all_of(c(geo_level, geo_level_aggr))) %>%
        distinct(),
      by = geo_level
    ) %>%
    group_by(across(all_of(c(geo_level_aggr, "year", "fuel")))) %>%
    summarise(
      price_en = mean(price_en, na.rm = TRUE),
      .groups = "drop"
    )

  price_i <- price_by_year %>%
    filter(year == current_year) %>%
    rename(price_en_current = price_en) %>%
    left_join(
      price_by_year %>%
        filter(year == income_reference_year) %>%
        select(-year) %>%
        rename(price_en_base = price_en),
      by = c(geo_level_aggr, "fuel")
    ) %>%
    mutate(
      price_multiplier = price_en_current / price_en_base
    )

  # Technology-specific LCOH in 2020 USD/kWh of useful heat. The FTT
  # capacity-factor input is expressed in thousand full-load hours/year.
  technology_i <- cost_inv_sw_heat %>%
    filter(
      year == current_year,
      .data[[geo_level_aggr]] %in% selected_regions
    ) %>%
    left_join(
      cost_om_sw_heat %>% filter(year == current_year),
      by = c(geo_level_aggr, "fuel_heat", "year")
    ) %>%
    left_join(
      cost_fuel_sw_heat,
      by = c(geo_level_aggr, "fuel_heat")
    ) %>%
    left_join(
      eff_sw_heat %>% filter(year == current_year),
      by = c(geo_level_aggr, "fuel_heat", "year")
    ) %>%
    left_join(
      capacity_factor_sw_heat %>% filter(year == current_year),
      by = c(geo_level_aggr, "fuel_heat", "year")
    ) %>%
    left_join(
      discount_rate_sw_heat %>% filter(year == current_year),
      by = c(geo_level_aggr, "fuel_heat", "year")
    ) %>%
    left_join(
      lifetime_sw,
      by = geo_level_aggr
    ) %>%
    mutate(
      price_fuel = ifelse(fuel_heat == "heat pump", "electricity", fuel_heat)
    ) %>%
    left_join(
      price_i,
      by = setNames(
        c(geo_level_aggr, "year", "fuel"),
        c(geo_level_aggr, "year", "price_fuel")
      )
    ) %>%
    mutate(
      discount_sum =
        (1 - (1 + discount_rate_sw_heat)^(-lifetime_sw)) /
        (1 - 1 / (1 + discount_rate_sw_heat)),
      investment_cost =
        cost_inv_sw_heat /
        (capacity_factor_sw_heat * 1000) /
        discount_sum,
      om_cost =
        cost_om_sw_heat /
        (capacity_factor_sw_heat * 1000),
      fuel_cost =
        cost_fuel_sw_heat * price_multiplier / eff_sw_heat,
      lcoh = investment_cost + om_cost + fuel_cost
    )

  behavior_i <- cost_int_sw_heat_base %>%
    filter(
      .data$renov_status == .env$renov_status,
      .data[[geo_level_aggr]] %in% selected_regions
    ) %>%
    left_join(
      alpha_sw_heat %>%
        filter(.data$renov_status == .env$renov_status),
      by = c(geo_level_aggr, "urt", "renov_status", "fuel_heat")
    ) %>%
    left_join(
      eta_sw_heat %>%
        filter(.data$renov_status == .env$renov_status),
      by = c(geo_level_aggr, "urt", "renov_status", "fuel_heat")
    )

  preference_weight_i <- income_i %>%
    inner_join(
      behavior_i,
      by = c(geo_level_aggr, "urt")
    ) %>%
    inner_join(
      technology_i %>%
        select(
          all_of(c(geo_level_aggr, "year", "fuel_heat")),
          lcoh
        ),
      by = c(geo_level_aggr, "year", "fuel_heat")
    ) %>%
    mutate(
      log_income_ratio = log(hh_income / income_ref),
      intangible_cost =
        cost_int_sw_heat_base *
        exp(alpha_sw_heat + eta_sw_heat * log_income_ratio),
      gcoh = lcoh + intangible_cost,
      attractiveness = gcoh^(-nu)
    )

  if (
    nrow(income_i) == 0 ||
    nrow(technology_i) == 0 ||
    nrow(behavior_i) == 0 ||
    nrow(preference_weight_i) == 0
  ) {
    stop(
      paste0(
        "Missing endogenous fuel-preference inputs for ",
        renov_status,
        " in ",
        current_year
      )
    )
  }

  if (any(!is.finite(preference_weight_i$attractiveness))) {
    print(
      preference_weight_i %>%
        filter(!is.finite(attractiveness)) %>%
        select(
          all_of(c(geo_level_aggr, "urt", "inc_cl", "year")),
          fuel_heat,
          hh_income,
          income_ref,
          lcoh,
          cost_int_sw_heat_base,
          alpha_sw_heat,
          eta_sw_heat,
          intangible_cost,
          gcoh,
          attractiveness
        ) %>%
        distinct()
    )
    stop(paste0(
      "Non-finite fuel attractiveness for ",
      renov_status,
      " in ",
      current_year
    ))
  }

  preference_i <- preference_weight_i %>%
    select(
      all_of(c(geo_level_aggr, "urt", "inc_cl", "year")),
      fuel_heat_i = fuel_heat,
      attractiveness_i = attractiveness
    )

  preference_f <- preference_weight_i %>%
    select(
      all_of(c(geo_level_aggr, "urt", "inc_cl", "year")),
      fuel_heat_f = fuel_heat,
      attractiveness_f = attractiveness
    )

  relative_preference_i <- preference_i %>%
    inner_join(
      preference_f,
      by = c(geo_level_aggr, "urt", "inc_cl", "year")
    ) %>%
    mutate(
      relative_preference =
        attractiveness_f /
        (attractiveness_i + attractiveness_f)
    ) %>%
    select(
      all_of(c(geo_level_aggr, "urt", "inc_cl", "year")),
      fuel_heat_i,
      fuel_heat_f,
      relative_preference
    )

  if (
    any(relative_preference_i$relative_preference < 0) ||
    any(relative_preference_i$relative_preference > 1)
  ) {
    stop("Relative fuel preference must be between zero and one")
  }

  print(paste0(
    "Completed endogenous fuel preference (",
    renov_status,
    ") - year ",
    current_year
  ))

  relative_preference_i
}


fun_fuel_transition_probability_zhu <- function(stock_fuel_i,
                                                 det_age_i,
                                                 relative_preference,
                                                 transition_matrix,
                                                 yrs, i,
                                                 geo_level,
                                                 geo_level_aggr) {

  fuel_group_cols <- unique(
    c(geo_level, geo_level_aggr, "urt", "inc_cl")
  )

  # S_g,j,t: share of candidate fuel j in the building stock at the start
  # of the current model period.
  stock_fuel_share_i <- stock_fuel_i %>%
    select(
      all_of(c(fuel_group_cols, "year")),
      fuel_heat_f = fuel_heat,
      S_stock
    )

  transition_probability_i <- det_age_i %>%
    mutate(year = yrs[i]) %>%
    select(
      all_of(c(fuel_group_cols, "year")),
      fuel_heat_i = fuel_heat
    ) %>%
    distinct() %>%
    inner_join(
      transition_matrix,
      by = c("year", "fuel_heat_i")
    ) %>%
    left_join(
      stock_fuel_share_i,
      by = c(fuel_group_cols, "year", "fuel_heat_f")
    ) %>%
    left_join(
      relative_preference,
      by = c(
        geo_level_aggr, "year", "urt", "inc_cl",
        "fuel_heat_i", "fuel_heat_f"
      )
    ) %>%
    mutate(
      mod_decision = replace_na(as.numeric(ct_sw_fuel_heat), 0),
      S_stock = replace_na(S_stock, 0),
      relative_preference = replace_na(relative_preference, 0),
      K = mod_decision * S_stock * relative_preference
    ) %>%
    group_by(
      across(all_of(c(fuel_group_cols, "year", "fuel_heat_i")))
    ) %>%
    mutate(
      sum_K = sum(K),
      T = ifelse(
        sum_K > 0,
        K / sum_K,
        as.numeric(fuel_heat_i == fuel_heat_f)
      )
    ) %>%
    ungroup() %>%
    select(-ct_sw_fuel_heat)

  transition_probability_i
}


fun_fuel_switch_decision_zhu <- function(yrs, i,
                                         bld_det_age_i,
                                         relative_preference_fuel_ren,
                                         relative_preference_fuel_noren,
                                         transition_matrix_fuel_renov,
                                         transition_matrix_fuel_norenov,
                                         geo_level,
                                         geo_level_aggr) {

  print(paste0("Running fuel-switch decision - year ", yrs[i]))

  fuel_group_cols <- unique(
    c(geo_level, geo_level_aggr, "urt", "inc_cl")
  )

  # The current-period decision uses the building stock available at the
  # start of the timestep. F06b subsequently applies demolition, renovation,
  # switching and the remaining stock dynamics once, in their original order.
  stock_det_age_i <- bld_det_age_i %>%
    filter(year == yrs[i - 1])

  stock_fuel_i <- stock_det_age_i %>%
    transmute(
      across(all_of(fuel_group_cols)),
      year = yrs[i],
      fuel_heat,
      n_units_fuel_stock = n_units_fuel
    ) %>%
    group_by(
      across(all_of(c(fuel_group_cols, "year", "fuel_heat")))
    ) %>%
    summarise(
      n_units_fuel_stock = sum(n_units_fuel_stock),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(c(fuel_group_cols, "year")))) %>%
    mutate(
      S_stock = ifelse(
        sum(n_units_fuel_stock) > 0,
        n_units_fuel_stock / sum(n_units_fuel_stock),
        0
      )
    ) %>%
    ungroup()

  sw_prob_fuel_ren_i <-
    fun_fuel_transition_probability_zhu(
      stock_fuel_i = stock_fuel_i,
      det_age_i = stock_det_age_i,
      relative_preference = relative_preference_fuel_ren,
      transition_matrix = transition_matrix_fuel_renov,
      yrs = yrs,
      i = i,
      geo_level = geo_level,
      geo_level_aggr = geo_level_aggr
    )

  sw_prob_fuel_noren_i <-
    fun_fuel_transition_probability_zhu(
      stock_fuel_i = stock_fuel_i,
      det_age_i = stock_det_age_i,
      relative_preference = relative_preference_fuel_noren,
      transition_matrix = transition_matrix_fuel_norenov,
      yrs = yrs,
      i = i,
      geo_level = geo_level,
      geo_level_aggr = geo_level_aggr
    )

  print(paste0("Completed fuel-switch decision - year ", yrs[i]))

  list(
    stock_fuel_i = stock_fuel_i,
    sw_prob_fuel_ren_i = sw_prob_fuel_ren_i,
    sw_prob_fuel_noren_i = sw_prob_fuel_noren_i
  )
}
