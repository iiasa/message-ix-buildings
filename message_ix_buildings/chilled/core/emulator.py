import os

import pandas as pd
import pyam  # type: ignore
import statsmodels.formula.api as smf  # type: ignore

from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config

log = get_logger(__name__)


def apply_ols_and_quantile_reg(df, var: str):
    quantiles = [0.05, 0.25, 0.5, 0.75, 0.95]
    df = df.query("cumCO2.notnull()")

    # log.info("Create quantile regression model... ")
    mod = smf.quantreg(f"{var} ~ cumCO2", df)
    # res = mod.fit(q=0.5)
    # log.info("...Finished creating quantile regression model")

    def fit_model(q):
        res = mod.fit(q=q)
        return [
            q,
            res.params["Intercept"],
            res.params["cumCO2"],
        ] + res.conf_int().loc["cumCO2"].tolist()

    # log.info("Fitting quantile regression model... ")
    models = [fit_model(x) for x in quantiles]
    # log.info("...Finished fitting quantile regression model")

    # log.info("Creating DataFrame from quantile regression model... ")
    models = pd.DataFrame(models, columns=["q", "a", "b", "lb", "ub"])
    # log.info("...Finished creating DataFrame from quantile regression model")

    # log.info("Create OLS model... ")
    ols = smf.ols(f"{var} ~ cumCO2", df).fit()
    # log.info("...Finished creating OLS model")

    # log.info("Calculating OLS confidence interval... ")
    ols_ci = ols.conf_int().loc["cumCO2"].tolist()
    # log.info("...Finished calculating OLS confidence interval")

    # log.info("Creating OLS DataFrame... ")
    ols = dict(
        q="ols",
        a=ols.params["Intercept"],
        b=ols.params["cumCO2"],
        lb=ols_ci[0],
        ub=ols_ci[1],
    )
    ols = pd.DataFrame(ols, index=[0])
    # log.info("...Finished creating OLS DataFrame")

    # concat quantile regression and ols
    # log.info("Concatenating quantile regression and OLS DataFrames... ")
    models_ols = pd.concat([models, ols]).reset_index(drop=True)
    # log.info("...Finished concatenating quantile regression and OLS DataFrames")

    # keep unique columns of df: region, name_run, urt, arch
    un_df = df.reindex(["region", "name_run", "urt", "arch"], axis=1).drop_duplicates()

    # merge unique columns with all_res
    res = pd.merge(un_df.assign(key="z"), models_ols.assign(key="z"), on="key").drop(
        columns="key"
    )

    return res


def quantile_regression(reg_data, var: str):
    regr_data = reg_data.query("cumCO2.notnull()").query(
        "scenario.str.startswith('ssp')"
    )

    grouped_ei_cumCO2 = regr_data.groupby(["region", "name_run", "urt", "arch"])
    list_df_group = [g for _, g in grouped_ei_cumCO2]

    log.info("Applying apply_ols_and_quantile_reg()... ")
    reg = pd.Series(list_df_group).apply(lambda df: apply_ols_and_quantile_reg(df, var))
    log.info("...Finished applying apply_ols_and_quantile_reg()")

    reg = pd.concat(reg.tolist()).reset_index(drop=True)

    log.info("Calculating IQR... ")
    # calculate iqr
    iqr_b = (
        reg.query("q == 0.25 or q == 0.75")
        .pivot_table(
            values="b", index=["region", "name_run", "urt", "arch"], columns="q"
        )
        .reset_index()
        .assign(iqr_b=lambda x: x[0.75] - x[0.25])
        .drop(columns=[0.25, 0.75])
    )

    iqr_a = (
        reg.query("q == 0.25 or q == 0.75")
        .pivot_table(
            values="a", index=["region", "name_run", "urt", "arch"], columns="q"
        )
        .reset_index()
        .assign(iqr_a=lambda x: x[0.75] - x[0.25])
        .drop(columns=[0.25, 0.75])
    )

    df_iqr = pd.merge(iqr_a, iqr_b, on=["region", "name_run", "urt", "arch"])
    log.info("...Finished calculating IQR")

    log.info("Merging regression parameters with IQR... ")
    out = pd.merge(reg, df_iqr, on=["region", "name_run", "urt", "arch"])
    log.info("...Finished merging regression parameters with IQR")

    return out


def produce_regression_files(
    variable: str,
    save_path: str,
    cumco2_file: str,
    file_str: str,
    variable_str: str,
    scenarios: list,
):
    log.info("Reading in EI vs cumCO2 data... ")
    # Load EI vs cumCO2 data
    EI_cumCO2 = pd.read_csv(os.path.join(save_path, cumco2_file))
    log.info("...Finished reading in EI vs cumCO2 data")

    log.info("Reading in engage emissions... ")
    # Load engage emissions
    engage_cumCO2 = (
        pyam.IamDataFrame(
            os.path.join(save_path, "engage_emissions_tempAR6_cumCO2.xlsx")
        )
        .filter(variable="Cumulative CO2 infilled*")
        .as_pandas()
        .assign(variable="Cumulative Emissions|CO2")
    )
    log.info("...Finished reading in engage emissions")

    log.info("Applying quantile_regression()... ")
    regr_params = quantile_regression(EI_cumCO2, variable)
    log.info("...Finished applying quantile_regression()")

    # join engage emissions with regression parameters to calculate climate change EI
    log.info("Join engage emissions w/ regression parameters to calculate CC EI... ")
    df_cc = (
        pd.merge(
            regr_params.assign(key="z"),
            engage_cumCO2.assign(key="z").drop(columns=["region"]),
            on="key",
        )
        .drop(columns="key")
        .rename(columns={"value": "cumCO2"})
        .assign(
            value=lambda x: x.b * x.cumCO2 + x.a,
            model="MESS-CHILL-URM",
            variable=lambda x: variable_str + x.arch,
            unit="MJ / m2 / yr",
        )
        .reindex(
            [
                "model",
                "scenario",
                "name_run",
                "urt",
                "arch",
                "q",
                "region",
                "year",
                "cumCO2",
                "variable",
                "unit",
                "value",
            ],
            axis=1,
        )
    )
    log.info(
        "...Finished joining engage emissions w/ regression parameters to calculate CC EI"
    )

    # calculate no climate change EI
    # take median across GCMs
    log.info("Calculating no climate change EI... ")
    single_nocc = (
        EI_cumCO2.query("scenario == 'baseline'")
        .reindex(
            ["name_run", "urt", "arch", "clim", "region", "year", variable],
            axis=1,
        )
        .groupby(["name_run", "urt", "arch", "clim", "region", "year"])
        .median()
        .reset_index()
    )
    log.info("...Finished calculating no climate change EI")

    # Repeat single_cc for each scenario
    log.info("Repeating single_cc for each scenario... ")
    single_nocc_list = []
    for s in scenarios:
        log.info(f"...Repeating single_cc for scenario: {s}")
        single_noccnew = single_nocc.assign(scenario=s)
        single_nocc_list.append(single_noccnew)
    log.info("...Finished repeating single_cc for each scenario")

    log.info("Creating DataFrame for no climate change EI... ")
    df_nocc = (
        pd.concat(single_nocc_list)
        .assign(
            q="baseline",
            model="MESS-CHILL-URM",
            variable=lambda x: variable_str + x.arch,
            unit="MJ / m2 / yr",
        )
        .rename(columns={variable: "value"})
        .reindex(
            [
                "model",
                "scenario",
                "name_run",
                "urt",
                "arch",
                "q",
                "region",
                "year",
                "cumCO2",
                "variable",
                "unit",
                "value",
            ],
            axis=1,
        )
    )
    log.info("...Finished creating DataFrame for no climate change EI")

    log.info("Saving files... ")
    regr_params.to_csv(
        os.path.join(save_path, "regression_results_" + file_str + ".csv"), index=False
    )

    log.info(
        "Saved: " + os.path.join(save_path, "regression_results_" + file_str + ".csv")
    )

    df_cc.to_csv(
        os.path.join(save_path, "REGIONAL_EI_PATHWAYS_cc_long_" + file_str + ".csv"),
        index=False,
    )

    log.info(
        "Saved: "
        + os.path.join(save_path, "REGIONAL_EI_PATHWAYS_cc_long_" + file_str + ".csv")
    )

    df_nocc.to_csv(
        os.path.join(save_path, "REGIONAL_EI_PATHWAYS_nocc_long_" + file_str + ".csv"),
        index=False,
    )

    log.info(
        "Saved: "
        + os.path.join(save_path, "REGIONAL_EI_PATHWAYS_nocc_long_" + file_str + ".csv")
    )


def apply_emulator(
    config: "Config",
):
    project_path = get_paths(config, "project_path")
    version_path = os.path.join(project_path, "out", "version", config.vstr)
    out_path = os.path.join(version_path, "output")

    # % Add regional populations
    scenarios = ["EN_NPi2020_300f", "EN_NPi2020_1400f", "EN_NPi2100"]
    log.info("Selected scenarios: " + ", ".join(scenarios))

    log.info("Reading in population data... ")
    popdata = pyam.IamDataFrame(os.path.join(out_path, "population_by_region.csv"))
    for scenario in scenarios:
        popnew = popdata.rename({"scenario": {"SSP2": scenario}})
        popdata.append(popnew.filter(scenario=scenario), inplace=True)
    popdata.filter(scenario="SSP2", keep=False, inplace=True)
    log.info("...Finished reading in population data")

    if config.cool == 1:
        produce_regression_files(
            variable="EI_ac_m2",
            save_path=out_path,
            cumco2_file="region_EI_cool_cumCO2_pre-regress.csv",
            file_str="cool",
            variable_str="Elec_Intensity|AC|",
            scenarios=scenarios,
        )

    if config.heat == 1:
        produce_regression_files(
            variable="EI_h_m2",
            save_path=out_path,
            cumco2_file="region_EI_heat_cumCO2_pre-regress.csv",
            file_str="heat",
            variable_str="Elec_Intensity|Heat|",
            scenarios=scenarios,
        )
