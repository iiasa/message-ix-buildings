# -*- coding: utf-8 -*-
"""
Created on Wed Jan 25 13:34:32 2017

@author: mastrucci & byers
"""

# File to store buildings functions
import numpy as np

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
def calc_gn_sol(
    i_sol_v, gl_perc, gl_g, gl_sh
):  # transparent surfaces only - vertical solar irradiation
    "This returns the solar heat gains through vertical transparent surfaces"
    return i_sol_v * gl_perc * gl_g * gl_sh


def calc_gn_sol_v(
    i_sol_v, gl_perc, gl_g, gl_sh
):  # transparent surfaces only - vertical solar irradiation
    "This returns the solar heat gains through vertical transparent surfaces"
    return i_sol_v * gl_perc * gl_g * gl_sh  #


def calc_gn_sol_h(
    i_sol_h, roof_area, roof_abs, u_roof
):  # rooftop only - horiz. solar irradiation
    "This returns the solar heat gains through the rooftop"
    return i_sol_h * 0.04 * u_roof * roof_area * roof_abs


# 0.04 external surface resistance.

# def calc_gn_sol_tot(i_sol_v, gl_perc, gl_g, gl_sh, i_sol_h, roof_area, roof_abs, u_roof): #transparent surfaces + roof
#    "This returns the solar heat gains through vertical transparent surfaces"
#    return (i_sol_v * gl_perc * gl_g * gl_sh / 24 + i_sol_h * 0.04 * u_roof * roof_area * roof_abs) #Vertical irr. is currently in monthly Wh for 24 hours


def calc_gn_sol_tot(
    i_sol_v, gl_perc, gl_g, gl_sh, i_sol_h, roof_area, roof_abs, u_roof
):  # transparent surfaces + roof
    "This returns the solar heat gains through vertical transparent surfaces"
    return (
        i_sol_v * gl_perc * gl_g * gl_sh
        + i_sol_h * 0.04 * u_roof * roof_area * roof_abs
    )


def calc_H_tr(u_val, area_env):
    "This returns the solar heat gains"
    return u_val * area_env


def calc_H_v_cl(vol, ach_cl):  # i_sol_v excluded. to update with average exposition
    "This returns ventilation heat transfer (closed windows)"
    return 1200 * vol * ach_cl / 3600


def calc_H_v_op(vol, ach_op):  # i_sol_v excluded. to update with average exposition
    "This returns ventilation heat transfer (open windows)"
    return 1200 * vol * ach_op / 3600


##==============================================================================
## # Functions for Variable Heating Degree Day (HDD) calculation
##==============================================================================
#
def calc_t_bal_h(t_sp_h, gn_int, gn_sol, H_tr, H_v_cl):
    "This returns the balance temperature"
    t_bal_h = t_sp_h - (
        (gn_int + gn_sol) / (H_tr + H_v_cl)  # i_sol: monthly irradiance (W/m2)
    )  # - 2400*x_diff  # change for sigma: monthly SD of x_out
    return t_bal_h


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
def calc_t_bal_c(t_sp_c, gn_int, gn_sol, H_tr, H_v_cl):  # , x_diff):
    "This returns the balance temperature"
    #    x_diff = x_diff.where(x_diff > 0)
    t_bal_c = t_sp_c - (
        (gn_int + gn_sol) / (H_tr + H_v_cl)  # i_sol: monthly irradiance (W/m2)
    )  # - 2400*x_diff  # change for sigma: monthly SD of x_out
    return t_bal_c


def calc_vdd_h(t_out_ave, t_bal_h):  # DEGREE DAYS should be calculated month by month
    "This returns the variable heating degree days based on t_bal_h"
    vdd_h = (t_bal_h - t_out_ave).where((t_bal_h >= t_out_ave))
    vdd_h = vdd_h.fillna(0)
    return vdd_h


#
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


def calc_t_max_c(t_sp_c_max, gn_int, gn_sol, H_tr, H_v_op):  # , x_diff0):
    "This returns the max temperature (fans - open windows)"
    #    x_diff = x_diff.where(x_diff > 0)
    t_max_c = t_sp_c_max - ((gn_int + gn_sol) / (H_tr + H_v_op))  # - 2400*x_diff
    return t_max_c


def calc_vdd_tmax_c(
    t_out_ave, t_max_c
):  # DEGREE DAYS should be calculated month by month
    "This returns the variable cooling degree days based on Tmax"
    vdd_tmax_c = (t_out_ave - t_max_c).where((t_out_ave >= t_max_c))
    vdd_tmax_c = vdd_tmax_c.fillna(0)
    return vdd_tmax_c


def calc_Nd(t_out_ave, t_max_c, nyrs):
    # Number of days per month where t_out_ave > t_max_c
    # Look into doing for longer climatology
    anoms = t_out_ave.groupby("time.month") - t_max_c  # Subtract
    Nd = (
        anoms.where(anoms > 0).groupby("time.month").count(dim="time") / nyrs
    )  # Find only positive values
    return Nd


def calc_Ndyr(t_out_ave, t_max_c):
    # Number of days per year where t_out_ave > t_max_c
    # Look into doing for longer climatology
    anoms = t_out_ave - t_max_c
    Nd = anoms.where(anoms > 0)
    Ndyr = Nd.groupby("time.year").count(dim="time").mean(dim="year")
    return Ndyr


def calc_Nf(t_out_ave, t_bal_c, nyrs):
    # Number of days per month where t_out_ave > t_bal_c
    # Look into doing for longer climatology
    #    anoms = t_out_ave.groupby('time.month') - t_max_c # Subtract
    #    t_diff = t_max_c - t_bal_c
    #    Nf = anoms.where(anoms>0 & anoms < t_diff ).groupby('time.month').count(dim='time')/nyrs # Find only positive values
    Nf = (
        t_out_ave.where(t_out_ave.groupby("time.month") > t_bal_c)
        .groupby("time.month")
        .count(dim="time")
        / nyrs
    )
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
def calc_SCDD_y(t_out_ave, bal_temp):
    cooldays = t_out_ave.where(t_out_ave > bal_temp)
    nyrs = len(t_out_ave.time) / 365
    newda = (cooldays - bal_temp).sum("time") / nyrs
    return newda


def calc_SCDD_m(t_out_ave, bal_temp):
    cooldays = t_out_ave.where(t_out_ave > bal_temp)
    nyrs = len(t_out_ave.time) / 365
    newda = (cooldays - bal_temp).groupby("time.month").sum(dim="time") / nyrs
    return newda


def calc_SHDD_y(t_out_ave, bal_temp):
    heatdays = t_out_ave.where(t_out_ave < bal_temp)
    nyrs = len(t_out_ave.time) / 365
    newda = (bal_temp - heatdays).sum("time") / nyrs
    return newda


def calc_SHDD_m(t_out_ave, bal_temp):
    heatdays = t_out_ave.where(t_out_ave < bal_temp)
    nyrs = len(t_out_ave.time) / 365
    newda = (bal_temp - heatdays).groupby("time.month").sum(dim="time") / nyrs
    return newda


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
def Q_h(H_tr, H_v_cl, f_h, vdd_h):
    "This returns the monthly heating energy (MJ) based on variable degree days"
    return (H_tr + H_v_cl) * f_h * vdd_h * 86400.0 / np.power(10.0, 6.0)


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


def Q_c_tmax(H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c):  #
    "This returns the monthly cooling energy (MJ) based on variable degree days"
    return (
        (H_tr + H_v_cl)
        * f_c
        * (vdd_tmax_c + ((t_max_c - t_bal_c) * Nd))
        * 86400.0
        / np.power(10.0, 6.0)
    )  # Nd = number of days when (t_out_ave >= t_max_c)


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


def calc_E_c_ac(Q_c_tmax, cop):  #
    "This returns the monthly electricity requirement for air conditioning (sensible cooling only)"
    return Q_c_tmax / cop


# def calc_E_c_tot(Q_c_tmax, Q_lat_month, cop): #
#    "This returns the monthly electricity requirement for air conditioning (sensible and latent cooling)"
#    return  (Q_c_tmax + Q_lat_month) / cop


def calc_E_c_fan(f_f, P_f, Nf, area_fan):  #
    "This returns the monthly electricity requirement for fans"
    return (
        f_f * P_f * Nf * 86400.0 / (np.power(10.0, 6.0) * area_fan)
    )  # Nf = number of days when (t_bal_c <= t_out_ave < t_max_c)


# ==============================================================================
# # Final energy calculation - Heating
# ==============================================================================


def calc_E_h(Q_h, eff):  #
    "This returns the monthly final energy requirement for heating"
    return Q_h / eff


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
