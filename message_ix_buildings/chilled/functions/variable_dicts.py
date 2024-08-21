YEARS_BASELINE = {
    "2015": ("2015", "2020"),
    "2020": ("2015", "2020"),
    "2030": ("2015", "2020"),
    "2040": ("2015", "2020"),
    "2050": ("2015", "2020"),
    "2060": ("2015", "2020"),
    "2070": ("2015", "2020"),
    "2080": ("2015", "2020"),
    "2090": ("2015", "2020"),
    "2100": ("2015", "2020"),
}

YEARS_OTHERS = {
    "2015": ("2015", "2020"),
    "2020": ("2015", "2025"),
    "2030": ("2015", "2045"),
    "2040": ("2025", "2055"),
    "2050": ("2035", "2065"),
    "2060": ("2045", "2075"),
    "2070": ("2055", "2085"),
    "2080": ("2065", "2095"),
    "2090": ("2080", "2100"),
    "2100": ("2095", "2100"),
}


VARS_ARCHETYPES = [
    "area_env",
    "gl_perc",
    "vol",
    "fl_cnd",
    "u_val",
    "ach_cl",
    "ach_op",
    "gn_int",
    "gl_g",
    "gl_sh",
    "roof_area",
    "roof_abs",
    "u_roof",
]

VARDICT_COOL = {
    "E_c_perpix": "Energy for cooling per person incl floor area",
    "E_c_ac_popwei": "Energy for air conditioning, population weighted",
    "E_c_fan_popwei": "Energy for fans, population weighted",
    "E_c_popwei": "Total energy for cooling, population weighted",
    "E_c_ac_wAccess": "Energy for cooling, population with access to AC",
    "E_c_ac_gap": "Energy for cooling, population without access to AC",
    "E_c_fan_wAccess": "Energy for fans, \
        population with access to electricity",
    "E_c_fan_gap": "Energy for fans, population without electricity access",
    "E_c_wAccess": "Energy for cooling and fans, population with access",
    "E_c_gap": "Energy for cooling and fans, \
        population without access to either or both",
    "Nf": "Number of days per month where t_out_ave > t_bal_c",
    "Nd": "Number of days per month where t_out_ave > t_max_c",
    "vdd_c_popwei": "Sum of variable cooling degree days per year",
    # 'sdd_c': 'Sum of simple cooling degree days per year',
    "P_c_ac_potential": "Population living in locations requiring AC for DL",
    "P_c_ac_gap": "Population without AC access in locations \
        requiring AC for DL",
    "P_c_fan_gap": "Population without fan access in locations \
        requiring fans for DL",
    "P_c_fanNoAC": "Population with fan access but requiring AC access for DL",
}

VARDICT_HEAT = {
    "E_h_perpix": "Energy for heating per person incl floor area",
    "E_h_popwei": "Energy for heating, population weighted",
    "P_h_potential": "Population living in locations requiring heating for DL",
    # 'sdd_h': 'Sum of simple heating degree days per year',
    "vdd_h_popwei": "Sum of variable heating degree days per year",
}

VARUNDICT_COOL = {
    "E_c_perpix": "MJ/month",
    "E_c_ac_popwei": "MJ/month",
    "E_c_fan_popwei": "MJ/month",
    "E_c_popwei": "MJ/month",
    "E_c_ac_wAccess": "MJ/month",
    "E_c_ac_gap": "MJ/month",
    "E_c_fan_wAccess": "MJ/month",
    "E_c_fan_gap": "MJ/month",
    "E_c_wAccess": "MJ/month",
    "E_c_gap": "MJ/month",
    "Nf": "days/month",
    "Nd": "days/month",
    "vdd_c_popwei": "degreedays/month",
    # 'sdd_c': 'degreedays/month',
    "P_c_ac_potential": "people",
    "P_c_ac_gap": "people",
    "P_c_fan_gap": "people",
    "P_c_fanNoAC": "people",
}

VARUNDICT_HEAT = {
    "E_h_perpix": "MJ/month",
    "E_h_popwei": "MJ/month",
    "P_h_potential": "people",
    # 'sdd_h': 'degreedays/month',
    "vdd_h_popwei": "degreedays/month",
}

SCENARIO_NAMES = {
    "SSP1-26": "ssp126",
    "SSP3-Baseline": "ssp370",
    "SSP5-Baseline": "ssp585",
}
