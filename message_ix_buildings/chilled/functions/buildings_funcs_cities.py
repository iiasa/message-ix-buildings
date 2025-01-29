# -*- coding: utf-8 -*-
"""
Created on Wed Jan 25 13:34:32 2017

@author: mastrucci & byers
"""

# File to store buildings functions
import numpy as np
import pytz  # type: ignore
import timezonefinder  # type: ignore

## Input data (only possibly needed for testing)
t_sp_h = 20  # FIX Indoor setpoint temperature for heating (°C)
t_sp_c = 26  # FIX Indoor setpoint temperature for cooling (°C)
t_sp_c_max = 28  # FIX Indoor max temperature when fans are on (°C)
rh_sp = 60  # FIX Indoor setpoint for relative humidity (%)
# u_val = 1.0 # TAB Average U-value of building envelope (W/m2K)
# vol = 3 # TAB Building volume per floor surface unit (m3/m2) (tot)
# area_env = 2 # TAB Area of the building envelope (m2) (tot)
# fl_cnd = 1 # TAB Conditioned Floor area (%) (conditioned area)
# gn_int = 5 # FIX Internal gains (W/m2)
# i_sol = 50 # CLIM Heat gains   #SOLAR IRRADIATION
# gl_perc = 0.1 # TAB ratio window surface / floor surface
# gl_g = 0.85 # TAB g-value for glazing
# gl_sh = 0.9 # TAB shading coeff for openings
# t_bal_fix = 18.3 # Balance temperature (fix) for simple degree days calculation
# ach_cl = 0.5 # TAB - ACH (1/h) - closed windows
# ach_op = 1.5 # TAB - ACH (1/h) - open windows
f_h = 1.0  # FIX fraction of time heating is used
f_c = 0.333  # FIX fraction of time A/C is used
f_f = 0.2  # FIX fraction of time fans are used
#
# hw_bal_fix = 50 # hot water heating temperature
cop = 3.2  # FIX coeff. of performance A/C (-)
P_f = 55  # FIX power fans (W)
eff = 0.9  # FIX efficiency heating system

dm = np.array(
    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
)  # number of days in a month

# Input for standard degree days calc
# m_dot = 0.11 # air mass flow rate (kg/s)
# c_p = 1.02 # specific heat capacity of air (kJ/kgK)
# fl_ac = 10 # floor served by one AC unit

## Sample input data for climate
# t_out_min = 2
# t_out_max = 10
# rh_out = 80 # CLIM Relative humidity outdoor  # NEED TO IMPORT RH DATASET
# P_tot = 1013.25 # CLIM Total ambient pressure (hPa)   # NEED TO IMPORT P_TOT DATASET

# ==============================================================================
# # Definition of functions for average outdoor temperature calculation
# ==============================================================================

# def calc_t_out_ave(t_out_min, t_out_max):
#    "This returns the daily average temperature"
#    return (t_out_min + t_out_max)/2.0 # not correct - not needed


def calc_t_out_med(tasmin, tasmax):
    "This returns the daily median temperature - midpoint of tasmax and tasmin"
    return (tasmax.tasmaxAdjust + tasmin.tasminAdjust) / 2.0


# ==============================================================================
# # Definition of functions for solar irradiation
# ==============================================================================


def calc_i_sol(i_sol_vert_wh):
    "This converts monthly averages of daily solar irradiation (Wh/m2) to irradiance (W/m2)"
    return (i_sol_vert_wh) / 24


def calc_i_sol_h(i_sol_horiz_wh):
    "This converts monthly averages of daily solar irradiation (Wh/m2) to irradiance (W/m2)"
    return (i_sol_horiz_wh) / 24


##==============================================================================
## # Functions for Simple Degree Days Calculation
##==============================================================================
#
# def calc_dd_c(t_out_ave, t_bal_fix): # TO EDIT
#    "This returns the cooling degree days (fixed balance temperature)"
##    ddc = t_out_ave.copy(deep=True)
###    ddc.values = t_out_ave.values
##    ddc.values[t_out_ave.values <= t_bal_fix] = 0.0
##    ddc.values[t_out_ave.values > t_bal_fix] =  t_out_ave.values[t_out_ave.values > t_bal_fix] - t_bal_fix
#    ddc = t_out_ave.where(t_out_ave > t_bal_fix) - t_bal_fix
#    return ddc
#
# def calc_dd_h(t_out_ave, t_bal_fix): #TO EDIT
#    "This returns the heating degree days (fixed balance temperature)"
##    ddh = t_out_ave.copy(deep=True)
###    ddh.values = t_out_ave.values
##    ddh.values[t_out_ave.values < t_bal_fix] =  t_bal_fix - t_out_ave.values[t_out_ave.values < t_bal_fix]
##    ddh.values[t_out_ave.values >= t_bal_fix] = 0.0
#    ddh = t_bal_fix - t_out_ave.where(t_out_ave < t_bal_fix)
#    return ddh
#
##==============================================================================
## # Functions for Cooling Energy Calculation - Standard Degree-Days
##==============================================================================
#
# def Q_c_std1_day(m_dot, c_p, dd_c, f_c, fl_ac):
#    import numpy as np
#    "This returns the daily cooling energy (MJ) based on standard degree days - Method 1"
#    return m_dot * c_p * dd_c * f_c * 86400 / (fl_ac * np.power(10,3))
#
# def Q_c_std2_day(u_val, area_env, dd_c):
#    import numpy as np
#    "This returns the daily cooling energy (MJ) based on standard degree days - Method 2"
#    return u_val * area_env * dd_c * f_c * 86400 / np.power(10,6)
#
##==============================================================================
## # Electricity calculation - Standard Degree-Days
##==============================================================================
#
# def calc_E_c_std1(Q_c_std1_day, cop): #
#    "This returns the monthly electricity requirement for air conditioning"
#    return  Q_c_std1_day / cop
#
# def calc_E_c_std2(Q_c_std2_day, cop): #
#    "This returns the monthly electricity requirement for air conditioning"
#    return  Q_c_std1_day / cop


# ==============================================================================
# #  heat gains - heat transfer calculation
# ==============================================================================

# def calc_gn_sol(i_sol_h, i_sol_v, gl_perc, gl_g, gl_sh, roof_area, roof_abs, u_roof): #i_sol_v excluded. to update with average exposition
#    "This returns the solar heat gains"
#    return ((i_sol_v * gl_perc * gl_g * gl_sh)/24 + i_sol_h * 0.04 * u_roof * roof_area * roof_abs) #
## 0.04 external surface resistance.


# ONLY GAINS FROM WINDOWS
def calc_gn_sol(df, var_col, gl_perc, gl_g, gl_sh):
    "This returns the solar heat gains through vertical transparent surfaces"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df = df.merge(
        gl_perc[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    )
    merged_df = merged_df.merge(
        gl_g[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
        suffixes=("_gl_perc", "_gl_g"),
    )
    merged_df = merged_df.merge(
        gl_sh[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    )

    # Calculate solar heat gains
    i_sol_v = merged_df[var_col]
    merged_df["gn_sol"] = (
        i_sol_v
        * merged_df["urban_gl_perc"]
        * merged_df["urban_gl_g"]
        * merged_df["urban"]
    )

    # Keep original columns and add gn_sol
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "gn_sol"]]


def calc_gn_sol_v(
    i_sol_v, gl_perc, gl_g, gl_sh
):  # transparent surfaces only - vertical solar irradiation
    "This returns the solar heat gains through vertical transparent surfaces"
    return i_sol_v * gl_perc * gl_g * gl_sh  #


def calc_gn_sol_h(df, var_col, roof_area, roof_abs, u_roof):
    "This returns the solar heat gains through the rooftop"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df = df.merge(
        roof_area[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    )
    merged_df = merged_df.merge(
        roof_abs[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
        suffixes=("_roof_area", "_roof_abs"),
    )
    merged_df = merged_df.merge(
        u_roof[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    )

    # Calculate solar heat gains
    i_sol_h = merged_df[var_col]
    merged_df["gn_sol_h"] = (
        i_sol_h
        * 0.04
        * merged_df["urban_roof_area"]
        * merged_df["urban_roof_abs"]
        * merged_df["urban"]
    )

    # Keep original columns and add gn_sol_h
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "gn_sol"]]


# 0.04 external surface resistance.

# def calc_gn_sol_tot(i_sol_v, gl_perc, gl_g, gl_sh, i_sol_h, roof_area, roof_abs, u_roof): #transparent surfaces + roof
#    "This returns the solar heat gains through vertical transparent surfaces"
#    return (i_sol_v * gl_perc * gl_g * gl_sh / 24 + i_sol_h * 0.04 * u_roof * roof_area * roof_abs) #Vertical irr. is currently in monthly Wh for 24 hours


def calc_gn_sol_tot(
    df_v, var_col_v, df_h, var_col_h, gl_perc, gl_g, gl_sh, roof_area, roof_abs, u_roof
):
    "This returns the solar heat gains through vertical transparent surfaces and the rooftop"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df_v = df_v.merge(
        gl_perc[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "gl_perc"})
    merged_df_v = merged_df_v.merge(
        gl_g[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "gl_g"})
    merged_df_v = merged_df_v.merge(
        gl_sh[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "gl_sh"})

    merged_df_h = df_h.merge(
        roof_area[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "roof_area"})
    merged_df_h = merged_df_h.merge(
        roof_abs[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "roof_abs"})
    merged_df_h = merged_df_h.merge(
        u_roof[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "u_roof"})

    # Calculate solar heat gains
    merged_df_v["gn_sol_v"] = (
        merged_df_v[var_col_v]
        * merged_df_v["gl_perc"]
        * merged_df_v["gl_g"]
        * merged_df_v["gl_sh"]
    )
    merged_df_h["gn_sol_h"] = (
        merged_df_h[var_col_h]
        * 0.04
        * merged_df_h["roof_area"]
        * merged_df_h["roof_abs"]
        * merged_df_h["u_roof"]
    )

    # Combine vertical and horizontal gains
    merged_df = merged_df_v.merge(
        merged_df_h[["city", "city_lat", "city_lon", "month", "gn_sol_h"]],
        on=["city", "city_lat", "city_lon", "month"],
    )
    merged_df["gn_sol"] = merged_df["gn_sol_v"] + merged_df["gn_sol_h"]

    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "gn_sol"]]


def calc_H_tr(u_val_df, area_env_df):
    "This returns the heat transfer through the building envelope"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df = u_val_df.merge(
        area_env_df[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
        suffixes=("_u_val", "_area_env"),
    )

    # Calculate heat transfer
    merged_df["H_tr"] = merged_df["urban_u_val"] * merged_df["urban_area_env"]

    # Keep original columns and add H_tr
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "H_tr"]]


def calc_H_v_cl(vol_df, ach_cl_df):
    "This returns ventilation heat transfer (closed windows)"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df = vol_df.merge(
        ach_cl_df[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
        suffixes=("_vol", "_ach_cl"),
    )

    # Calculate ventilation heat transfer
    merged_df["H_v_cl"] = (
        1200 * merged_df["urban_vol"] * merged_df["urban_ach_cl"] / 3600
    )

    # Keep original columns and add H_v_cl
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "H_v_cl"]]


def calc_H_v_op(vol_df, ach_op_df):
    "This returns ventilation heat transfer (open windows)"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df = vol_df.merge(
        ach_op_df[["city", "city_lat", "city_lon", "urban"]],
        on=["city", "city_lat", "city_lon"],
        suffixes=("_vol", "_ach_op"),
    )

    # Calculate ventilation heat transfer
    merged_df["H_v_op"] = (
        1200 * merged_df["urban_vol"] * merged_df["urban_ach_op"] / 3600
    )

    # Keep original columns and add H_v_op
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "H_v_op"]]


##==============================================================================
## # Functions for Variable Heating Degree Day (HDD) calculation
##==============================================================================
#
def calc_t_bal_h(t_sp_h, gn_int_df, gn_sol_df, H_tr_df, H_v_cl_df):
    "This returns the balance temperature"
    # Merge dataframes by city, city_lat, city_lon, and month
    merged_df = gn_int_df.merge(
        gn_sol_df[["city", "city_lat", "city_lon", "month", "gn_sol"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "gn_int"})

    merged_df = merged_df.merge(
        H_tr_df[["city", "city_lat", "city_lon", "H_tr"]],
        on=["city", "city_lat", "city_lon"],
    )
    merged_df = merged_df.merge(
        H_v_cl_df[["city", "city_lat", "city_lon", "H_v_cl"]],
        on=["city", "city_lat", "city_lon"],
    )

    # Calculate balance temperature
    merged_df["t_bal_h"] = t_sp_h - (
        (merged_df["gn_int"] + merged_df["gn_sol"])
        / (merged_df["H_tr"] + merged_df["H_v_cl"])
    )

    # Keep original columns and add t_bal_h
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "t_bal_h"]]


# def calc_vdd_h(t_out_ave, t_bal_h):
#    "This returns the variable heating degree days"
##    vdd_h = t_out_ave.copy(deep=True)
##    temmask = xr.open_dataset('H:/Modelling/esywat/landmaskmap0.nc')['EnsembleMean']
##    vdd_h.values[t_out_ave <= t_bal_h] = (t_bal_h - t_out_ave)[t_out_ave <= t_bal_h]  # Where heating is required
#    vdd_h = (t_bal_h - t_out_ave).where((t_out_ave <= t_bal_h))
#    vdd_h = vdd_h
#    vdd_h = vdd_h.fillna(0)
#
##    vdd_h.values[t_out_ave.values <= t_bal_h] =  t_bal_h - t_out_ave.values[t_out_ave.values <= t_bal_h]
##    vdd_h.values[t_out_ave > t_bal_h] = 0.0
#    return vdd_h
#
##==============================================================================
## # Functions for Variable Cooling Degree Day (CDD) calculation
##==============================================================================
#
def calc_t_bal_c(t_sp_c, gn_int_df, gn_sol_df, H_tr_df, H_v_cl_df):
    "This returns the balance temperature"
    # Merge dataframes by city, city_lat, city_lon, and month
    merged_df = gn_int_df.merge(
        gn_sol_df[["city", "city_lat", "city_lon", "month", "gn_sol"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "gn_int"})

    merged_df = merged_df.merge(
        H_tr_df[["city", "city_lat", "city_lon", "H_tr"]],
        on=["city", "city_lat", "city_lon"],
    )
    merged_df = merged_df.merge(
        H_v_cl_df[["city", "city_lat", "city_lon", "H_v_cl"]],
        on=["city", "city_lat", "city_lon"],
    )

    # Calculate balance temperature
    merged_df["t_bal_c"] = t_sp_c - (
        (merged_df["gn_int"] + merged_df["gn_sol"])
        / (merged_df["H_tr"] + merged_df["H_v_cl"])
    )

    # Keep original columns and add t_bal_c
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "t_bal_c"]]


def calc_vdd_h(t_out_ave_df, t_out_ave_col, t_bal_h_df):
    "This returns the variable heating degree days based on t_bal_h"
    # Merge dataframes by city, city_lat, city_lon, lat, lon, and month
    merged_df = t_out_ave_df.merge(
        t_bal_h_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "t_bal_h"]],
        on=["city", "city_lat", "city_lon", "lat", "lon", "month"],
        how="left",
    )

    # Calculate variable heating degree days
    merged_df["vdd_h"] = (merged_df["t_bal_h"] - merged_df[t_out_ave_col]).where(
        merged_df["t_bal_h"] >= merged_df[t_out_ave_col]
    )
    merged_df["vdd_h"] = merged_df["vdd_h"].fillna(0)

    return merged_df.drop(columns=["t_bal_h", t_out_ave_col])


# def calc_vdd_c(t_out_ave, t_bal_c, arb_fan = 0):
#    """This returns the variable heating degree days"
#    t_out_ave is the input daily mean air temperature as an xarray dataArray
#    t_bal_c calculated with the calc_tbal_c function is the balance temperature
#    arb_fan is a arbitrary subtraction if you want to reduce the degree day calculation, e.g. by 2°C"""
#
##    vdd_c = t_out_ave.copy(deep=True)
##    vdd_c.values = t_out_ave.values
##    vdd_c.values[t_out_ave.values < t_bal_c] = 0.0
##    vdd_c.values[t_out_ave.values >= t_bal_c.values] =  (t_out_ave.values - t_bal_c.values)[t_out_ave.values >= t_bal_c]
#    vdd_c = (t_out_ave - t_bal_c).where((t_out_ave >= t_bal_c))
#    vdd_c = vdd_c-arb_fan
#    vdd_c = vdd_c.fillna(0)
#    return vdd_c

# ==============================================================================
# # Functions for Variable Cooling Degree Day (CDD) calculation with fans
# ==============================================================================


# def calc_t_bal_c(t_sp_c, gn_int, i_sol, gl_perc, gl_g, gl_sh, u_val, vol, area_env, ach_cl, x_diff):
#    "This returns the balance temperature"
#    x_diff = x_diff.where(x_diff > 0)
#    t_bal_c = t_sp_c - ((gn_int + i_sol * gl_perc * gl_g * gl_sh ) / # i_sol: monthly irradiance (W/m2)
#                        (u_val * area_env + 1200 * vol * ach_cl / 3600)) - 2400*x_diff
#    return t_bal_c


def calc_t_max_c(t_sp_c_max, gn_int_df, gn_sol_df, H_tr_df, H_v_op_df):
    "This returns the max temperature (fans - open windows)"
    # Merge dataframes by city, city_lat, and city_lon
    merged_df = gn_int_df.merge(
        gn_sol_df[["city", "city_lat", "city_lon", "month", "gn_sol"]],
        on=["city", "city_lat", "city_lon"],
    ).rename(columns={"urban": "gn_int"})

    merged_df = merged_df.merge(
        H_tr_df[["city", "city_lat", "city_lon", "H_tr"]],
        on=["city", "city_lat", "city_lon"],
    )
    merged_df = merged_df.merge(
        H_v_op_df[["city", "city_lat", "city_lon", "H_v_op"]],
        on=["city", "city_lat", "city_lon"],
    )

    # Calculate max temperature
    merged_df["t_max_c"] = t_sp_c_max - (
        (merged_df["gn_int"] + merged_df["gn_sol"])
        / (merged_df["H_tr"] + merged_df["H_v_op"])
    )

    # Keep original columns and add t_max_c
    return merged_df[["city", "city_lat", "city_lon", "lat", "lon", "month", "t_max_c"]]


def calc_vdd_tmax_c(t_out_ave_df, t_out_ave_col, t_max_c_df, nyrs, climate_zones):
    "This returns the variable cooling degree days based on Tmax"
    # DEGREE DAYS should be calculated month by month

    # Merge dataframes by city, city_lat, city_lon, lat, lon, month, and year
    base_cols = ["city", "city_lat", "city_lon", "lat", "lon", "month"]

    df_cols = base_cols + ["year", t_out_ave_col]

    if climate_zones:
        df_cols += ["lcz"]

    tmax_cols = base_cols + ["t_max_c"]

    merged_df = t_out_ave_df[df_cols].merge(
        t_max_c_df[tmax_cols],
        on=base_cols,
        how="left",
    )

    # Calculate variable cooling degree days
    merged_df["vdd_tmax_c"] = (merged_df[t_out_ave_col] - merged_df["t_max_c"]).where(
        merged_df[t_out_ave_col] >= merged_df["t_max_c"]
    )
    merged_df["vdd_tmax_c"] = merged_df["vdd_tmax_c"].fillna(0)

    groupby_cols = base_cols

    if climate_zones:
        groupby_cols += ["lcz"]

    # Divide by years
    vdd_tmax_c = merged_df.groupby(groupby_cols).sum("time") / nyrs
    vdd_tmax_c = vdd_tmax_c.reset_index()

    return vdd_tmax_c.drop(columns=["t_max_c", t_out_ave_col])


def calc_Nd(df, t_out_ave_col: str, t_max_c_df, nyrs, climate_zones=False):
    base_cols = ["city", "city_lat", "city_lon", "lat", "lon", "month"]

    df_columns = base_cols + [t_out_ave_col]
    if climate_zones:
        df_columns += ["lcz"]

    ras_scen_reduced = df[df_columns].drop_duplicates().reset_index()

    t_max_c_columns = base_cols + ["t_max_c"]

    t_max_c_reduced = t_max_c_df[t_max_c_columns].drop_duplicates()

    # Perform the merge operation
    merged_df = ras_scen_reduced.merge(
        t_max_c_reduced,
        on=base_cols,
        how="inner",
        copy=False,
    ).reset_index(drop=True)

    # Calculate anomalies
    merged_df["anoms"] = merged_df[t_out_ave_col] - merged_df["t_max_c"]

    # Calculate number of days per month (averaged across nyrs) where t_out_ave > t_max_c
    group_by_columns = ["city", "city_lat", "city_lon", "lat", "lon", "month"]
    if climate_zones:
        group_by_columns += ["lcz"]

    Nd = (
        merged_df.groupby(group_by_columns)["anoms"]
        .apply(lambda x: (x > 0).sum())
        .div(nyrs)
    ).reset_index(name="Nd")
    return Nd


def calc_Ndyr(t_out_ave, t_max_c):
    # Number of days per year where t_out_ave > t_max_c
    # Look into doing for longer climatology
    anoms = t_out_ave - t_max_c
    Nd = anoms.where(anoms > 0)
    Ndyr = Nd.groupby("time.year").count(dim="time").mean(dim="year")
    return Ndyr


def calc_Nf(df, t_out_ave_col, t_bal_c_df, nyrs, climate_zones=False):
    base_cols = ["city", "city_lat", "city_lon", "lat", "lon", "month"]

    df_columns = base_cols + [t_out_ave_col]
    if climate_zones:
        df_columns += ["lcz"]

    ras_scen_reduced = df[df_columns].drop_duplicates().reset_index()

    t_bal_c_columns = base_cols + ["t_bal_c"]
    t_bal_c_reduced = t_bal_c_df[t_bal_c_columns].drop_duplicates()

    # Perform the merge operation
    merged_df = ras_scen_reduced.merge(
        t_bal_c_reduced,
        on=base_cols,
        how="inner",
        copy=False,
    ).reset_index(drop=True)

    # Calculate anomalies
    merged_df["anoms"] = merged_df[t_out_ave_col] - merged_df["t_bal_c"]

    # Calculate number of days per month where t_out_ave > t_bal_c
    group_by_columns = ["city", "city_lat", "city_lon", "lat", "lon", "month"]
    if climate_zones:
        group_by_columns += ["lcz"]

    Nf = (
        merged_df.groupby(group_by_columns)["anoms"]
        .apply(lambda x: (x > 0).sum())
        .div(nyrs)
    ).reset_index(name="Nf")

    return Nf


# def calc_Nf_lim(t_out_ave, t_max_c, t_bal_c, nyrs):
#    # Number of days per month where t_out_ave > t_bal_c & < t_max_c
#    # Look into doing for longer climatology
##    anoms = t_out_ave.groupby('time.month') - t_max_c # Subtract
##    t_diff = t_max_c - t_bal_c
##    Nf_lim = anoms.where(anoms>0 & anoms < t_diff ).groupby('time.month').count(dim='time')/nyrs # Find only positive values
#    Nf_lim = t_out_ave.where((t_out_ave.groupby('time.month') < t_max_c) & (t_out_ave.groupby('time.month') > t_bal_c)).groupby('time.month').count(dim='time')/nyrs
#    return Nf_lim


# Days over threshold
def calc_SCDD_y(t_out_ave, bal_temp, nyrs):
    cooldays = t_out_ave.where(t_out_ave > bal_temp)
    newda = (cooldays - bal_temp).sum("time") / nyrs
    return newda


def calc_SCDD_m(df, t_out_ave_col, city_col, bal_temp, nyrs):
    """
    df should have columns year, month, and city
    """
    cooldays = df[df[t_out_ave_col] > bal_temp]
    newda = (cooldays[t_out_ave_col] - bal_temp).groupby(
        [cooldays[city_col], cooldays.index.month]
    ).sum() / nyrs
    return newda.reset_index(name="SCDD").rename(columns={"index": "month"})


def calc_SHDD_y(t_out_ave, bal_temp, nyrs):
    heatdays = t_out_ave.where(t_out_ave < bal_temp)
    newda = (bal_temp - heatdays).sum("time") / nyrs
    return newda


def calc_SHDD_m(df, t_out_ave_col, city_col, bal_temp, nyrs):
    """
    df should have columns year, month, and city
    """
    heatdays = df[df[t_out_ave_col] < bal_temp]
    newda = (bal_temp - heatdays[t_out_ave_col]).groupby(
        [heatdays[city_col], heatdays.index.month]
    ).sum() / nyrs
    return newda.reset_index(name="SHDD").rename(columns={"index": "month"})


#
# def calc_DoT(t_out_ave, bal_temp)
#    # Days over threshold
#    DoT = t_out_ave.where(t_out_ave<bal_temp)
#    newds = DoT.count(dim='time')/nyrs
#    nv = list(newds.data_vars)[0]
#    newds.rename({nv:'DoTsum'}, inplace=True)
#
#    # HDDsum
#    newds['HDDsum'] = ((Thr-DoT.temperature).sum('time')/nyrs)

# % Stull equation
# http://journals.ametsoc.org/doi/full/10.1175/JAMC-D-11-0143.1


def Twb_Stull(tas, hurs):
    import numpy as np

    Tw = (
        tas * np.arctan(0.151977 * np.power((hurs + 8.313659), 0.5))
        + np.arctan(tas + hurs)
        - np.arctan(hurs - 1.676331)
        + 0.00391838 * np.power(hurs, 1.5) * np.arctan(0.023101 * hurs)
        - 4.686035
    )
    return Tw


# ==============================================================================
# # Functions for Heating and Cooling Energy Calculation - Variable Degree-Days
# ==============================================================================


# def Q_c_day(u_val, area_env, vdd_c):
#    import numpy as np
#    "This returns the daily cooling energy (MJ) based on variable degree days"
#    return u_val * area_env * vdd_c * 86400 / np.power(10,6)
#
def Q_h(H_tr_df, H_v_cl_df, f_h, vdd_h_df):
    "This returns the monthly heating energy (MJ) based on variable degree days"
    # Merge dataframes by city, city_lat, city_lon, lat, lon, and month
    merged_df = vdd_h_df.merge(
        H_tr_df[["city", "city_lat", "city_lon", "lat", "lon", "H_tr"]],
        on=["city", "city_lat", "city_lon", "lat", "lon"],
        how="left",
    )
    merged_df = merged_df.merge(
        H_v_cl_df[["city", "city_lat", "city_lon", "lat", "lon", "H_v_cl"]],
        on=["city", "city_lat", "city_lon", "lat", "lon"],
        how="left",
    )

    # Calculate heating energy
    merged_df["Q_h"] = (
        (merged_df["H_tr"] + merged_df["H_v_cl"])
        * f_h
        * merged_df["vdd_h"]
        * 86400.0
        / np.power(10.0, 6.0)
    )

    return merged_df.drop(columns=["H_tr", "H_v_cl", "vdd_h"])


#
# def Q_c_year(Q_c_day):
#    "This returns the yearly cooling energy (MJ) based on variable degree days"
#    return Q_c_day.groupby('time.year').sum(dim='time')
#
# def Q_h_year(Q_h_day):
#    "This returns the yearly heating energy (MJ) based on variable degree days"
#    return Q_h_day.groupby('time.year').sum(dim='time')

# ==============================================================================
# # Functions for Cooling Energy Calculation with Fans
# ==============================================================================


def Q_c_tmax(
    H_tr_df, H_v_cl_df, vdd_tmax_c_df, t_max_c_df, t_bal_c_df, Nd_df, f_c, climate_zones
):  #
    "This returns the monthly cooling energy (MJ) based on variable degree days"

    base_cols = ["city", "city_lat", "city_lon", "lat", "lon"]

    month_cols = base_cols + ["month"]

    H_tr_cols = base_cols + ["H_tr"]
    H_v_cl_cols = base_cols + ["H_v_cl"]
    vdd_tmax_cols = base_cols + ["month", "vdd_tmax_c"]
    t_max_c_cols = base_cols + ["month", "t_max_c"]
    t_bal_c_cols = base_cols + ["month", "t_bal_c"]
    Nd_cols = base_cols + ["month", "Nd"]

    if climate_zones:
        vdd_tmax_cols += ["lcz"]
        Nd_cols += ["lcz"]

    # Merge dataframes by city, city_lat, city_lon, and month
    merged_df = H_tr_df[H_tr_cols].merge(
        H_v_cl_df[H_v_cl_cols],
        on=base_cols,
        how="outer",
    )
    merged_df = merged_df.merge(
        vdd_tmax_c_df[vdd_tmax_cols],
        on=base_cols,
        how="outer",
    )
    merged_df = merged_df.merge(
        t_max_c_df[t_max_c_cols],
        on=month_cols,
        how="outer",
    )
    merged_df = merged_df.merge(
        t_bal_c_df[t_bal_c_cols],
        on=month_cols,
        how="outer",
    )
    if climate_zones:
        merged_df = merged_df.merge(
            Nd_df[Nd_cols],
            on=month_cols + ["lcz"],
            how="outer",
        )
    else:
        merged_df = merged_df.merge(
            Nd_df[Nd_cols],
            on=month_cols,
            how="outer",
        )

    # Calculate cooling energy
    merged_df["Q_c_tmax"] = (
        (merged_df["H_tr"] + merged_df["H_v_cl"])
        * f_c
        * (
            merged_df["vdd_tmax_c"]
            + ((merged_df["t_max_c"] - merged_df["t_bal_c"]) * merged_df["Nd"])
        )
        * 86400.0
        / np.power(10.0, 6.0)
    )

    return merged_df.drop(
        columns=["H_tr", "H_v_cl", "vdd_tmax_c", "t_max_c", "t_bal_c", "Nd"]
    )


##==============================================================================
## # Functions for Latent Cooling loads calculation
##==============================================================================
## Notes:Partial vapor pressure calculation: P_w = P_ws * RH / 100 [hPa]
##       Water saturation pressure P_ws = A * 10^(m*T/(T+Tn)) [hPa]
##           A = 6.116441
##           m = 7.591386
##           Tn = 240.7263
##       Humidity ratio calculation: X = (B * P_w)/(P_tot - P_w) [g/kg]
##           B = 621.9907
##       Latent load calculation: Q_lat = q * rho * h_we * d_X [kW]
##           q = air volume flow [m3/s]
##           rho * h_we = 3010 kJ/m3
##               rho = density of air = ~1.202 [kg/m3]
##               h_we = latent heat of vaporization of water = ~2465.56 [kJ/kg]
##           d_X = humidity ratio difference [kg/kg]
#
# def P_w_out(t_out_ave, RH_out):
#    "Outdoor partial vapour pressure calculation (hPa)"
#    return (6.116441 * np.power(10,((7.591386 * t_out_ave) / (t_out_ave + 240.7263))) * RH_out / 100.0).squeeze() #Double check formula
#
# def X_out(P_w_out, P_tot):
#    "Outdoor humidity ratio calculation (kg/kg)"
#    return ((621.9907 * P_w_out) / (P_tot - P_w_out) / 1000.0).squeeze()
#
# def P_w_in(t_sp_c, RH_out):
#    "Indoor partial vapour pressure calculation (hPa)"
#    import numpy as np
#    return (6.116441 * np.power(10,((7.591386 * t_sp_c) / (t_sp_c + 240.7263))) * RH_out / 100.0).squeeze()
#
# def X_in(P_w_in, P_tot):
#    "Indoor humidity ratio calculation (kg/kg)"
#    return ((621.9907 * P_w_in) / (P_tot - P_w_in) / 1000.0).squeeze()
#
# def Q_lat(ach_cl, vol, x_in, x_out):
#    "Latent load calculation (MJ)"
#    qlat = (ach_cl * vol / 3600.0) * 3010 * (x_out - x_in) * 86400.0 / 1000.0
#    qlat.values[(x_out.values < x_in.values)] = 0.0
#    return qlat
#
# def Q_lat_month(Q_lat):
#    "Monthly Latent load calculation (MJ)"
#    return Q_lat.groupby('time.month').sum(dim='time')
#
# def Q_lat_year(Q_lat_day):
#    "Yearly Latent load calculation (MJ)"
#    return Q_lat_day.groupby('time.year').sum(dim='time')


# ==============================================================================
# # Electricity calculation
# ==============================================================================


def calc_E_c_ac(Q_c_tmax_df, cop):  #
    "This returns the monthly electricity requirement for air conditioning (sensible cooling only)"

    E_c_ac = Q_c_tmax_df.copy(deep=True)

    E_c_ac["E_c_ac"] = E_c_ac["Q_c_tmax"] / cop

    # Drop rows with E_c_ac NaN
    E_c_ac = E_c_ac.dropna(subset=["E_c_ac"])

    return E_c_ac.drop(columns=["Q_c_tmax"])


# def calc_E_c_tot(Q_c_tmax, Q_lat_month, cop): #
#    "This returns the monthly electricity requirement for air conditioning (sensible and latent cooling)"
#    return  (Q_c_tmax + Q_lat_month) / cop


def calc_E_c_fan(f_f, P_f, Nf_df, area_fan):  #
    "This returns the monthly electricity requirement for fans"

    E_c_fan = Nf_df.copy(deep=True)
    E_c_fan["E_c_fan"] = (
        f_f * P_f * E_c_fan["Nf"] * 86400.0 / (np.power(10.0, 6.0) * area_fan)
    )  # Nf = number of days when (t_bal_c <= t_out_ave < t_max_c)

    # Drop rows with E_c_ac NaN
    E_c_fan = E_c_fan.dropna(subset=["E_c_ac"])

    return E_c_fan.drop(columns=["Nf"])


# ==============================================================================
# # Final energy calculation - Heating
# ==============================================================================


def calc_E_h(Q_h_df, eff):  #
    "This returns the monthly final energy requirement for heating"
    Q_h_df["E_h"] = Q_h_df["Q_h"] / eff
    return Q_h_df.drop(columns=["Q_h"])


##==============================================================================
## HWDD - hot water degree days
##==============================================================================
#
# def calc_dd_hw(t_out_min, hw_bal_fix, cw_in_min):
#    "This returns the hot water heating degree days differential (fixed balance temperature)"
##    ddh = t_out_ave.copy(deep=True)
###    ddh.values = t_out_ave.values
##    ddh.values[t_out_ave.values < t_bal_fix] =  t_bal_fix - t_out_ave.values[t_out_ave.values < t_bal_fix]
##    ddh.values[t_out_ave.values >= t_bal_fix] = 0.0
##    ddh = hw_bal_fix - t_out_min.where(t_out_min < hw_bal_fix) #Values below the heating set point value
#    ddh = t_out_min.where(t_out_min < hw_bal_fix)
#    ddh.values[ddh.values < cw_in_min] = cw_in_min
#    ddh = hw_bal_fix - ddh
#    return ddh

# ==============================================================================
# # AC Access calculation
# ==============================================================================


def calc_AC_acc(cdd, gdp):
    # cdd=cooling degree days (yearly); gdp=GDP per capita in $PPP (from WDI).
    # Sources: Isaac & Vuuren, Energy Policy, 2009, Eq.9; McNeil Letschert, 2008 'Future Air Conditioning Energy Consumption in Developing Countries…' Pg 5
    "This returns the AC access based on CDD and GDP"
    return (1 - 0.949 * np.exp(-0.00187 * cdd)) * (
        1 / (1 + np.exp(4.152) * np.exp(-0.237 * gdp / 1000))
    )


""""NEW FUNCTIONS ADDED FOR DEGREE HOURS CALCULATIONS"""

##==============================================================================
## Time zone
##==============================================================================


# get the time zone according to lat and lon
def find_localtimezone(lat, lon):
    obj = timezonefinder.TimezoneFinder()
    latitude = lat
    longitude = lon
    return obj.timezone_at(lng=lon, lat=lat)


# convert utc of teh dataset to loccal time, retuen loccal time and local hours
def utc2localtime(dst_utc, loacltime):
    data = dst_utc.time.to_index().tz_localize(pytz.UTC)
    dst_local = data.tz_convert(pytz.timezone(loacltime))
    locat_hr = dst_local.hour[:24]  # use for plottiing/preesentating
    return dst_local, locat_hr


##==============================================================================
## Simple degree hours calculations
##==============================================================================


## return degree hours for each hour, each month (lat, lon, 21hours*12months)
def calc_SHDH_mo(t_out_ave, bal_temp, grouper):
    heatdays = t_out_ave.where(t_out_ave < bal_temp)
    newda = bal_temp - heatdays
    nyrs = len(t_out_ave.time) / 8760
    mohrda = newda.groupby(grouper).sum()
    return mohrda


def calc_SCDH_mo(t_out_ave, bal_temp, grouper):
    cooldays = t_out_ave.where(t_out_ave > bal_temp)
    newda = cooldays - bal_temp
    nyrs = len(t_out_ave.time) / 8760
    mohrda = newda.groupby(grouper).sum()
    return mohrda
