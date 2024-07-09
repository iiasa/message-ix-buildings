import datetime
import os
from dataclasses import dataclass
from typing import Literal

import numpy as np
from functions.user_settings import DICT_USER_SETTINGS  # type: ignore


@dataclass
class Config:
    """Configuration for :mod:`.message_ix_buildings.chilled`."""

    # SPECIFY USER
    user: Literal["ALE", "ED", "MEAS", "MEAS_EBRO", "MEAS_UNICC"] = "MEAS_UNICC"
    # print(f"USER: {user}")

    project_path = DICT_USER_SETTINGS[user]["dle_path"]
    dle_path = DICT_USER_SETTINGS[user]["dle_path"]
    message_region_file = DICT_USER_SETTINGS[user]["message_region_map_file"]
    isimip_bias_adj_path = DICT_USER_SETTINGS[user]["isimip_bias_adj_path"]
    isimip_ewemib_path = DICT_USER_SETTINGS[user]["isimip_ewembi_path"]
    chunk_size = DICT_USER_SETTINGS[user]["chunk_size"]
    input_path = os.path.join(dle_path, "input_data")  # type: ignore

    # RUN SETTINGS
    paranalysis_mode = 1  # 1 = run entire parametric analysis; 0 = run only ref case
    runsdd = 0  # 1= run simple (standard) degree days calculation; 0 = don't run

    # Netcdf settings
    netcdf4_format = "NETCDF4_CLASSIC"
    comp = dict(zlib=True, complevel=5)  # Compression between 0 and 9 (highest)

    # TESTING MODE
    testing_mode = 0  # 1= selects only two years for testing; 0= select all years (full calculation)

    # VERSION SETTINGS
    vstr: str = "ALPS2023"  # version input data
    vstrcntry = "v4"  # version string for country data and floor surface
    gcm: Literal[
        "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0"
    ] = "GFDL-ESM4"

    # SCENARIO SETTINGS
    rcps = ["ssp126", "ssp370", "ssp585", "baseline"]  # list all possible scenarios
    rcp: Literal["ssp126", "ssp370", "ssp585", "baseline"] = "baseline"

    if rcp == "baseline":
        rcpdata = "ssp126"
    else:
        rcpdata = rcp

    # CLIMATIC DATA INPUTS
    if rcp == "baseline":
        yeardic = {
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
    else:
        yeardic = {
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

    # climatic inputs for sensitivity runs
    # clims = ["hist"]  # options: "hist", "1p5"
    clims = list(yeardic.keys())

    # POPULATION SETTINGS
    popfix = True  # If True, fix to SSP2, else.... (see script 4/5)

    # BUILDING SCENARIO SETTINGS

    # CONMSTRUCTION MATERIAL SHARES
    constr_setting = 0

    # FLOOR_SETTING CHOICE
    # Choose per_cap for different values by MESSAGE region or std_cap for fixed values
    floor_setting = "std_cap"  # v16; options: "std_cap", "per_cap"

    # BUILDING ARCHETYPE SETTINGS
    # Choose "regional" for different values by MESSAGE region or "fixed" for fixed values
    # (same for all regions)
    arch_setting = "regional"  # options: ["fixed", "regional"]

    # URBAN/RURAL DISAGGREGATIONS
    urts = ["urban", "rural"]  # options (mult): "urban", "rural"

    # ARCHETYPES
    archs = ["new", "exist"]

    # PARAMETERS FOR STEP 2 SCRIPT
    verbose = True

    ## SWITCH COOLING/HEATING CALCULATIONS
    ## 1=calculate; 0=skip
    heat = 0
    cool = 1

    # CHOICE: SOLAR GAIN CALCULATION
    solar_gains = "TOT"  # from windows and roof
    # solar_gains = 'VERT' #from windows only
    # solar_gains = 'HOR' #from windows only

    # SELECT VAR
    var = "tas"

    if var == "tas":
        davar = "tas"
    elif var == "twb":
        davar = "twb"

    overwrite = 0

    # BULDING PARAMETERS
    # Fixed values
    bal_temps = [18.3, 21.1, 26]  # [21.1] #  For simple cooling degree days
    arb_fan = 2
    t_sp_h = np.int8(20)  # Indoor setpoint temperature for heating
    P_f = 55  # power of fan (W)
    area_fan = 25  # Numer of m2 per fan
    gridshape2 = (360, 720)

    y2_attrs_dic = {
        "title": "map_area_env",
        "authors": "Alessio Mastrucci & Edward Byers",
        "date": str(datetime.datetime.now()),
        "institution": "IIASA Energy Program",
        "contact": "mastrucc@iiasa.ac.at; byers@iiasa.ac.at; ",
        "arch_setting": arch_setting,
    }

    # Final maps (y4) parameters
    nd_thresh = 5

    # # Load scenarios data
    # s_runs = load_all_scenarios_data(
    #     input_dle_path=DICT_USER_SETTINGS[user]["dle_path"], input_version_name=vstr
    # )

    # # Load paramtric analysis data
    # par_var = load_parametric_analysis_data(
    #     input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
    #     input_version_name=vstr,
    #     input_paranalysis_mode=paranalysis_mode,
    # )

    # #: Base year for projections.
    # base_year: int = BASE_YEAR

    # #: Year of convergence; used when :attr:`.method` is "convergence". See
    # #: :func:`.create_projections_converge`.
    # convergence_year: int = 2050

    # #: Rate of increase/decrease of fixed operating and maintenance costs.
    # fom_rate: float = 0.025

    # #: Format of output. One of:
    # #:
    # #: - "iamc": IAMC time series data structure.
    # #: - "message": :mod:`message_ix` parameter data.
    # format: Literal["iamc", "message"] = "message"

    #: Spatial resolution
    node: Literal["R11", "R12", "R20"] = "R11"

    # #: Projection method; one of:
    # #:
    # #: - "convergence": uses :func:`.create_projections_converge`
    # #: - "gdp": :func:`.create_projections_gdp`
    # #: - "learning": :func:`.create_projections_converge`
    # method: Literal["convergence", "gdp", "learning"] = "gdp"

    # #: Model variant to prepare data for.
    # module: Literal["base", "materials"] = "base"

    # #: Reference region; default "{node}_NAM".
    # ref_region: Optional[str] = None

    # #: Set of SSPs referenced by :attr:`scenario`. One of:
    # #:
    # #: - "original": :obj:`SSP_2017`
    # #: - "updated": :obj:`SSP_2024`
    # scenario_version: Literal["original", "updated"] = "updated"

    # #: Scenario(s) for which to create data.
    # scenario: Literal["all", "LED", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5"] = "all"

    # def __post_init__(self):
    #     if self.ref_region is None:
    #         self.ref_region = f"{self.node}_NAM"
