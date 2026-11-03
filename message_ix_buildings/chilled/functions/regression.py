import os

import pandas as pd
import pyam  # type: ignore
import statsmodels.formula.api as smf  # type: ignore

from message_ix_buildings.chilled.util.common import get_logger

log = get_logger(__name__)

# from message_ix_buildings.chilled.config import Config

# cfg = Config()

# dle_path = cfg.dle_path
# version_name = cfg.vstr

# version_output_path = os.path.join(
#     dle_path,
#     f"output_data_{version_name}",
#     "output",
# )

# # Load engage emissions
# engage_cumCO2 = (
#     pyam.IamDataFrame(
#         os.path.join(version_output_path, "engage_emissions_tempAR6_cumCO2.xlsx")
#     )
#     .filter(variable="Cumulative CO2 infilled*")
#     .as_pandas()
#     .assign(variable="Cumulative Emissions|CO2")
# )

# # % Add regional populations
# scenarios = ["EN_NPi2020_300f", "EN_NPi2020_1400f", "EN_NPi2100"]
# popdata = pyam.IamDataFrame(
#     os.path.join(version_output_path, "population_by_region.csv")
# )
# for scenario in scenarios:
#     popnew = popdata.rename({"scenario": {"SSP2": scenario}})
#     popdata.append(popnew.filter(scenario=scenario), inplace=True)
# popdata.filter(scenario="SSP2", keep=False, inplace=True)

# EI_cumCO2 = pd.read_csv(
#     os.path.join(version_output_path, "region_EI_cumCO2_pre-regress.csv")
# )


def apply_regression(
    dle_path,
    version_name,
):
    version_output_path = os.path.join(
        dle_path,
        f"output_data_{version_name}",
        "output",
    )

    # Load engage emissions
    engage_cumCO2 = (
        pyam.IamDataFrame(
            os.path.join(version_output_path, "engage_emissions_tempAR6_cumCO2.xlsx")
        )
        .filter(variable="Cumulative CO2 infilled*")
        .as_pandas()
        .assign(variable="Cumulative Emissions|CO2")
    )

    # % Add regional populations
    scenarios = ["EN_NPi2020_300f", "EN_NPi2020_1400f", "EN_NPi2100"]
    popdata = pyam.IamDataFrame(
        os.path.join(version_output_path, "population_by_region.csv")
    )
    for scenario in scenarios:
        popnew = popdata.rename({"scenario": {"SSP2": scenario}})
        popdata.append(popnew.filter(scenario=scenario), inplace=True)
    popdata.filter(scenario="SSP2", keep=False, inplace=True)

    EI_cumCO2 = pd.read_csv(
        os.path.join(version_output_path, "region_EI_cumCO2_pre-regress.csv")
    )

    def quantile_regression(reg_data):
        regr_data = reg_data.query("cumCO2.notnull()").query(
            "scenario.str.startswith('ssp')"
        )

        grouped_ei_cumCO2 = regr_data.groupby(["region", "name_run", "urt", "arch"])
        list_df_group = [g for _, g in grouped_ei_cumCO2]

        def apply_ols_and_quantile_reg(df):
            quantiles = [0.05, 0.25, 0.5, 0.75, 0.95]
            df = df.query("cumCO2.notnull()")

            mod = smf.quantreg("EI_ac_m2 ~ cumCO2", df)
            # res = mod.fit(q=0.5)

            def fit_model(q):
                res = mod.fit(q=q)
                return [
                    q,
                    res.params["Intercept"],
                    res.params["cumCO2"],
                ] + res.conf_int().loc["cumCO2"].tolist()

            models = [fit_model(x) for x in quantiles]
            models = pd.DataFrame(models, columns=["q", "a", "b", "lb", "ub"])

            ols = smf.ols("EI_ac_m2 ~ cumCO2", df).fit()
            ols_ci = ols.conf_int().loc["cumCO2"].tolist()
            ols = dict(
                q="ols",
                a=ols.params["Intercept"],
                b=ols.params["cumCO2"],
                lb=ols_ci[0],
                ub=ols_ci[1],
            )
            ols = pd.DataFrame(ols, index=[0])

            # concat quantile regression and ols
            models_ols = pd.concat([models, ols]).reset_index(drop=True)

            # keep unique columns of df: region, name_run, urt, arch
            un_df = df.reindex(
                ["region", "name_run", "urt", "arch"], axis=1
            ).drop_duplicates()

            # merge unique columns with all_res
            res = pd.merge(
                un_df.assign(key="z"), models_ols.assign(key="z"), on="key"
            ).drop(columns="key")

            return res

        reg = pd.Series(list_df_group).apply(apply_ols_and_quantile_reg)
        reg = pd.concat(reg.tolist()).reset_index(drop=True)

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

        # merge regression parameters with iqr
        out = pd.merge(reg, df_iqr, on=["region", "name_run", "urt", "arch"])

        return out

    regr_params = quantile_regression(EI_cumCO2)

    # join engage emissions with regression parameters to calculate climate change EI
    df_cc = (
        pd.merge(
            regr_params.assign(key="z"),
            engage_cumCO2.assign(key="z").drop(columns=["exclude", "region"]),
            on="key",
        )
        .drop(columns="key")
        .rename(columns={"value": "cumCO2"})
        .assign(
            value=lambda x: x.b * x.cumCO2 + x.a,
            model="MESS-CHILL-URM",
            variable=lambda x: "Elec_Intensity|AC|" + x.arch,
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

    # calculate no climate change EI
    # take median across GCMs
    single_nocc = (
        EI_cumCO2.query("scenario == 'baseline'")
        .reindex(
            ["name_run", "urt", "arch", "clim", "region", "year", "EI_ac_m2"],
            axis=1,
        )
        .groupby(["name_run", "urt", "arch", "clim", "region", "year"])
        .median()
        .reset_index()
    )

    # Repeat single_cc for each scenario
    single_nocc_list = []
    for s in scenarios:
        single_noccnew = single_nocc.assign(scenario=s)
        single_nocc_list.append(single_noccnew)

    df_nocc = (
        pd.concat(single_nocc_list)
        .assign(
            q="baseline",
            model="MESS-CHILL-URM",
            variable=lambda x: "Elec_Intensity|AC|" + x.arch,
            unit="MJ / m2 / yr",
        )
        .rename(columns={"EI_ac_m2": "value"})
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

    regr_params.to_csv(
        os.path.join(version_output_path, "regression_results.csv"), index=False
    )

    log.info("Saved: " + os.path.join(version_output_path, "regression_results.csv"))

    df_cc.to_csv(
        os.path.join(version_output_path, "REGIONAL_EI_PATHWAYS_cc_long.csv"),
        index=False,
    )

    log.info(
        "Saved: "
        + os.path.join(version_output_path, "REGIONAL_EI_PATHWAYS_cc_long.csv")
    )

    df_nocc.to_csv(
        os.path.join(version_output_path, "REGIONAL_EI_PATHWAYS_nocc_long.csv"),
        index=False,
    )

    log.info(
        "Saved: "
        + os.path.join(version_output_path, "REGIONAL_EI_PATHWAYS_nocc_long.csv")
    )
