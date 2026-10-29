
## HOUSEHOLD-TYPE OVERLAY
## Used when mod_hh_type = "overlay" (residential only).
## At each time step, after the stock turnover, households of each type are allocated to the occupied stock.
## The stock turnover does not depend on household types; the allocation is recomputed at each time step.
##
## Inputs (input list):
##   ct_hh_type:     household types (column hh_type)
##   shr_hh_type:    share of households by type; sums to 1 over hh_type. Columns: hh_type, value, and any of
##                   the block columns (region, urt, clim, inc_cl, year)
##   shr_hh_housing: share of households of each type living in each housing type (e.g. from surveys);
##                   sums to 1 over the housing columns within each hh_type. Housing columns are any building
##                   case columns other than the block columns (e.g. arch, mat, eneff)
## Allocation: iterative proportional fitting (IPF) of the survey distribution to the modelled stock, within
## each block. It matches both the number of households by type and the occupied stock by housing type,
## keeping the relative preferences of the survey data. Scarcity of a housing type is shared proportionally.


# Columns defining a block: households are allocated within a block and do not move across blocks
fun_hh_block_cols <- function(geo_levels) c(geo_levels, "urt", "clim", "inc_cl", "year")

# Interpolate values to the model years (linear; constant outside the data range)
fun_hh_interp <- function(x, y, xout) {
  if (length(x) == 1) return(rep(y, length(xout)))
  approx(x, y, xout = xout, rule = 2)$y
}

# Fill the model years of an input with a "year" column
fun_hh_fill_years <- function(df, value_col, yrs) {
  if (!"year" %in% names(df)) return(df)
  keys <- setdiff(names(df), c("year", value_col))
  df %>%
    group_by_at(keys) %>%
    group_modify(~ tibble(year = yrs, !!value_col := fun_hh_interp(.x$year, .x[[value_col]], yrs))) %>%
    ungroup()
}

# Stop if shares do not sum to 1 within groups
fun_hh_check_sum <- function(df, value_col, sum_over, name, tol = 1e-6) {
  bad <- df %>%
    group_by_at(setdiff(names(df), c(sum_over, value_col))) %>%
    summarise(tot = sum(.data[[value_col]]), .groups = "drop") %>%
    filter(abs(tot - 1) > tol)
  if (nrow(bad) > 0) {
    print(head(as.data.frame(bad), 10))
    stop(paste0(name, ": shares do not sum to 1 over (", paste(sum_over, collapse = ", "), ") in ",
                nrow(bad), " groups (first ones printed above)"))
  }
}


## Check and prepare the household-type inputs
fun_hh_prepare <- function(d, sector, yrs, geo_levels) {

  print("Household-type overlay: check inputs")

  if (sector != "resid") stop("mod_hh_type = 'overlay' is available for the residential sector only")

  missing <- hh_inputs[sapply(hh_inputs, function(x) is.null(d[[x]]))]
  if (length(missing) > 0) stop(paste0("mod_hh_type = 'overlay' requires these inputs in the input list: ",
                                       paste(missing, collapse = ", ")))

  block_cols <- fun_hh_block_cols(geo_levels)
  bld_cols <- names(d$bld_cases_fuel)
  types <- unique(d$ct_hh_type$hh_type)

  # Share of households by type
  extra <- setdiff(names(d$shr_hh_type), c(block_cols, "hh_type", "shr_hh_type"))
  if (length(extra) > 0) stop(paste0("shr_hh_type: unexpected columns (only block columns allowed): ",
                                     paste(extra, collapse = ", ")))

  # Share of households by housing type
  housing_cols <- setdiff(names(d$shr_hh_housing), c(block_cols, "hh_type", "shr_hh_housing"))
  if (length(housing_cols) == 0) stop("shr_hh_housing: no housing columns (e.g. arch, mat)")
  unknown <- setdiff(housing_cols, bld_cols)
  if (length(unknown) > 0) stop(paste0("shr_hh_housing: columns not in the building cases: ",
                                       paste(unknown, collapse = ", ")))

  for (x in c("shr_hh_type", "shr_hh_housing")) {
    if (any(d[[x]][[x]] < 0 | is.na(d[[x]][[x]]))) stop(paste0(x, ": negative or missing values"))
    undefined <- setdiff(unique(d[[x]]$hh_type), types)
    if (length(undefined) > 0) stop(paste0(x, ": household types not in ct_hh_type: ", paste(undefined, collapse = ", ")))
    not_covered <- setdiff(types, unique(d[[x]]$hh_type))
    if (length(not_covered) > 0) stop(paste0(x, ": household types missing: ", paste(not_covered, collapse = ", ")))
  }

  fun_hh_check_sum(d$shr_hh_type, "shr_hh_type", "hh_type", "shr_hh_type")
  fun_hh_check_sum(d$shr_hh_housing, "shr_hh_housing", housing_cols, "shr_hh_housing")

  # Fill model years
  d$shr_hh_type <- fun_hh_fill_years(d$shr_hh_type, "shr_hh_type", yrs)
  d$shr_hh_housing <- fun_hh_fill_years(d$shr_hh_housing, "shr_hh_housing", yrs)

  print(paste0("Household-type overlay: ", length(types), " types; housing columns: ", paste(housing_cols, collapse = ", ")))

  d
}


## Allocate households by type to the occupied stock of one year
# stock: building stock by detailed case (bld_det_age_i), with occupied units in n_units_fuel
# Returns a list of:
#   alloc:    households by block, housing type and hh_type: n_hh (allocated), n_hh_target (survey distribution
#             applied to the households of the block), n_units (occupied units of the housing type)
#   mismatch: by block, share of households not living where the survey distribution would place them,
#             with IPF iterations and convergence
fun_hh_allocate <- function(stock, yr, shr_hh_type, shr_hh_housing, ct_hh_type, geo_levels,
                            eps = 1e-9, tol = 1e-8, max_iter = 100) {

  if (!"year" %in% names(stock)) stock <- stock %>% mutate(year = yr)
  stock <- stock %>% filter(year == yr)

  block_cols <- intersect(fun_hh_block_cols(geo_levels), names(stock))
  housing_cols <- setdiff(names(shr_hh_housing), c(fun_hh_block_cols(geo_levels), "hh_type", "shr_hh_housing"))

  # Occupied units by block and housing type
  units <- stock %>%
    group_by_at(c(block_cols, housing_cols)) %>%
    summarise(n_units = sum(n_units_fuel), .groups = "drop") %>%
    filter(n_units > 0)

  grid <- units %>%
    cross_join(ct_hh_type %>% select(hh_type)) %>%
    ljoin(shr_hh_type) %>%
    ljoin(shr_hh_housing) %>%
    mutate(shr_hh_housing = coalesce(shr_hh_housing, 0)) # housing types not in the survey data

  if (any(is.na(grid$shr_hh_type))) stop(paste0("shr_hh_type: no data for some blocks in ", yr))

  # Group ids (in order of first appearance) and group sums
  gid <- function(cols) { k <- do.call(paste, c(grid[cols], sep = "\r")); match(k, unique(k)) }
  gsum <- function(x, g) rowsum(x, g, reorder = FALSE)[g]
  g_bh <- gid(c(block_cols, "hh_type")) # block x household type (rows of the IPF table)
  g_bk <- gid(c(block_cols, housing_cols)) # block x housing type (columns of the IPF table)
  g_b <- gid(block_cols)

  S <- grid$n_units # occupied units of the housing type
  N <- gsum(S, g_bh) * grid$shr_hh_type # households of the type in the block

  # Survey distribution, renormalised over the housing types available in the block
  P <- grid$shr_hh_housing
  P_tot <- gsum(P, g_bh)
  n_no_option <- length(unique(g_bh[P_tot == 0 & N > 0])) # block x type with no available housing type in the survey data
  P <- ifelse(P_tot > 0, P / P_tot, 0)
  P <- P + eps # small floor: every available housing type can host every household type
  P <- P / gsum(P, g_bh)

  D <- N * P # target: survey distribution

  # IPF: alternately match households by type (rows) and occupied units by housing type (columns)
  X <- D
  converged <- FALSE
  for (it in 1:max_iter) {
    X <- X * S / gsum(X, g_bk)
    rs <- gsum(X, g_bh)
    X <- ifelse(rs > 0, X * N / rs, 0)
    err <- max(abs(gsum(X, g_bk) - S) / gsum(S, g_bh))
    if (err < tol) { converged <- TRUE; break }
  }
  X <- X * S / gsum(X, g_bk) # end on the stock: occupied units are matched exactly

  if (!converged) warning(paste0("Household-type allocation ", yr, ": IPF not converged after ", max_iter,
                                 " iterations (max relative error ", signif(err, 3), ")"))
  if (n_no_option > 0) print(paste0("Household-type allocation ", yr, ": ", n_no_option,
                                    " block x type combinations without available housing in the survey data (allocated with the eps floor)"))

  alloc <- grid %>%
    select(all_of(c(block_cols, housing_cols, "hh_type", "n_units"))) %>%
    mutate(n_hh = X, n_hh_target = D)

  mismatch <- alloc %>%
    group_by_at(block_cols) %>%
    summarise(mismatch = 0.5 * sum(abs(n_hh - n_hh_target)) / sum(n_hh), .groups = "drop") %>%
    mutate(ipf_iter = it, ipf_converged = converged)

  list(alloc = alloc, mismatch = mismatch)
}


## Run the allocation for one year and add the results to the report
fun_hh_overlay <- function(report, bld_det_age_i, yr, d, geo_levels) {

  print(paste0("Household-type allocation - year ", yr))

  lst <- fun_hh_allocate(bld_det_age_i, yr, d$shr_hh_type, d$shr_hh_housing, d$ct_hh_type, geo_levels)

  report$hh_stock <- bind_rows(report$hh_stock, lst$alloc)
  report$hh_mismatch <- bind_rows(report$hh_mismatch, lst$mismatch)

  report
}
