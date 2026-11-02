Usage
*****

.. contents::
   :local:
   :depth: 2

Installation
============

STURM is distributed with the :mod:`message_ix_buildings` package but runs in R.
Get the files in either of these ways:

1. Clone the repository from GitHub::

     git clone git@github.com:iiasa/message-ix-buildings.git

2. Install the Python package and locate the installation directory::

     pip install message-ix-buildings
     pip show message-ix-buildings

All STURM files are in the subdirectory :file:`message_ix_buildings/sturm/`.

Requirements
------------

- R with the ``tidyverse`` packages.
  ``dplyr`` must be version 1.1.0 or later.
- ``rstudioapi``, used by the runner scripts to set the working directory when run from RStudio.
- ``readxl`` is loaded by :file:`model/F10_scenario_runs_MESSAGE_2100.R` but not used.

Running STURM
=============

STURM is run from one of the runner scripts in :file:`message_ix_buildings/sturm/`.
Each runner sets the user options, checks them, loops over the selected scenarios and calls :func:`run_scenario`.

.. list-table::
   :header-rows: 1
   :widths: 30 10 30 30

   * - Runner
     - Sector
     - Mode
     - Input list(s)
   * - :file:`run_STURM_resid_main_2026.R`
     - ``resid``
     - Endogenous new construction and renovation; optional vacancy (EU-27 only)
     - :file:`input_list_resid_2026_SSP_CE.csv` (global), :file:`input_list_resid_CircEUlar_2026.csv` (EU-27)
   * - :file:`run_STURM_resid_exo_main_2026.R`
     - ``resid``
     - Exogenous new construction and renovation
     - :file:`input_list_resid_2026_exogenous.csv`
   * - :file:`run_STURM_comm_main_2026.R`
     - ``comm``
     - Exogenous (the commercial sector has no endogenous choice model)
     - :file:`input_list_comm_2026_SSP_CE.csv` (global), :file:`input_list_comm_CircEUlar_2026.csv` (EU-27)

To run:

1. Open the runner in RStudio and edit the settings at the top of the script (see below).
2. Source the script.
   In RStudio the script sets the working directory to its own folder.
   From the command line, start R in :file:`message_ix_buildings/sturm/`.
3. Results are written to :file:`output/` (see :doc:`output`).

Each scenario runs inside ``tryCatch``: if one scenario fails, the runner prints the error and continues with the next.

Settings in the residential runner
----------------------------------

:file:`run_STURM_resid_main_2026.R`:

.. list-table::
   :header-rows: 1
   :widths: 25 20 55

   * - Setting
     - Default
     - Values and notes
   * - ``scenarios``
     - ``c("CP_tp", "SSP2")``
     - Any column of the selected input list: SSPs (``SSP1``–``SSP5``, ``SSP1_LED``, ``SSP2_LED``) or circular-economy scenarios (``R``, ``N_r``, ``N_tp``, ``S_r``, ``S_tp``, ``C_r``, ``C_tp``, ``A_r``, ``A_tp``, ``E_r``, ``E_tp``, ``CP_r``, ``CP_tp``).
       The runner stops if a scenario is not a column of the list.
   * - ``region_run_mode``
     - ``"global"``
     - ``"global"`` (all 61 regions), ``"eu27"`` (EU-27 countries) or ``"custom"``.
   * - ``custom_region_bld``
     - ``"C-WEU-AUT"``
     - Regions (``region_bld`` codes) to run when ``region_run_mode = "custom"``.
   * - ``custom_input_scope``
     - ``"eu27"``
     - Input list to use in custom mode: ``"global"`` or ``"eu27"``.
   * - ``vacant_mode_selected``
     - ``"none"``
     - ``"none"`` or ``"vacant"``.
       Vacancy is only allowed with ``region_run_mode = "eu27"``, and the list must contain the vacancy inputs (see :doc:`input`).
   * - ``report_type_selected``
     - ``c("STURM", "MESSAGE")``
     - Report families; see :doc:`output`.
   * - ``report_var_selected``
     - ``c("energy", "material")``
     - Report contents: ``"energy"``, ``"material"``, ``"vintage"``.
   * - ``years_to_run``
     - ``c(2020, 2025, 2030)``
     - Model years; the first is the base year.
       Full horizon: ``c(seq(2020, 2060, 5), seq(2070, 2100, 10))``.

Energy prices are read from :file:`data/input_prices_R12.csv` (MESSAGE output format) and passed to :func:`run_scenario`.

Exogenous residential runner
----------------------------

:file:`run_STURM_resid_exo_main_2026.R` uses exogenous shares and renovation rates instead of the choice models, with inputs from :file:`data/input_csv_SSP_2023_resid_ex/`.
Settings: ``scenarios`` (default ``IND_SSP2``, ``IND_SSP1``, ``IND_SSP3``), ``countries_to_run`` (``"all"``, ISO3 codes, or ``region_bld`` codes), ``geo_level_report_selected``, ``report_type_selected``, ``report_var_selected``, ``years_to_run`` (full horizon ``seq(2020, 2050, 5)``).
Vacancy is not available.

Commercial runner
-----------------

:file:`run_STURM_comm_main_2026.R` has the same structure as the residential runner, with default ``scenarios = c("SSP1", "E_tp")``.
It passes no energy prices, and vacancy must be ``"none"``.

The ``run_scenario()`` function
===============================

.. function:: run_scenario(run, sector, path_in, path_inputs, path_rcode, path_out, prices, file_inputs, geo_level, geo_level_aggr, geo_levels, geo_level_report, region_select, yrs, input_mode, mod_arch, mod_new, mod_ren, mod_vacant, report_type, report_var)

   Run one STURM scenario for one sector.
   Defined in :file:`model/F10_scenario_runs_MESSAGE_2100.R`.
   No argument has a default value.

   :param run: Scenario name. Selects the column of the input list and is written to the ``scenario`` column and the file names of the reports.
   :param sector: ``"resid"`` or ``"comm"``.
   :param path_in: Folder containing the input list.
   :param path_inputs: Folder containing the input data CSV files.
   :param path_rcode: Folder containing the model R scripts (:file:`model/`).
   :param path_out: Folder where reports are written.
   :param prices: Energy prices from MESSAGE: a data frame with columns ``node``, ``commodity``, ``year``, ``lvl``, or ``NULL``.
      If ``NULL``, the ``price_en`` input is used.
      Prices are only used by the endogenous residential choice models.
   :param file_inputs: File name of the input list.
   :param geo_level: Region column used for the calculation, e.g. ``"region_bld"``.
   :param geo_level_aggr: Aggregate region column, e.g. ``"region_gea"``.
   :param geo_levels: Region columns kept in the results, e.g. ``c("region_bld", "region_gea")``.
   :param geo_level_report: Region level of the reports.
      If equal to ``geo_level``, reports are written without aggregation.
   :param region_select: ``NULL`` (all regions) or ``list(<column>, <values>)`` to run a subset of regions.
   :param yrs: Vector of model years.
      ``yrs[1]`` is the base year; the time step is the difference between consecutive years.
   :param input_mode: ``"csv"`` (read the input list and CSV files) or ``"rdata"`` (load :file:`<path_in>/data_input_<run>_<sector>.RData`).
      All runners use ``"csv"``.
   :param mod_arch: ``"stock"`` (stock split by material and housing type with ``shr_mat`` and ``shr_arch``) or ``"new"``.
      All runners use ``"stock"``.
   :param mod_new: New construction choice, residential only: ``"endogenous"`` (life-cycle-cost choice model) or ``"exogenous"`` (input shares).
   :param mod_ren: Renovation and fuel switching, residential only: ``"endogenous"`` or ``"exogenous"``.
   :param mod_vacant: ``"none"`` or ``"vacant"`` (track vacant dwellings and their re-occupation).
   :param report_type: One or more of ``"STURM"``, ``"MESSAGE"``, ``"IRP"``, ``"NGFS"``, ``"NAVIGATE"``.
   :param report_var: One or more of ``"energy"``, ``"material"``, ``"vintage"``.
   :returns: The MESSAGE report as a data frame if ``"MESSAGE"`` is in ``report_type``, otherwise ``NULL``.
      The other reports are written to ``path_out`` as CSV files.

The commercial sector always uses exogenous shares and renovation rates; ``mod_new`` and ``mod_ren`` have no effect there.

Endogenous and exogenous mode
-----------------------------

In **endogenous** mode (residential only), shares of energy efficiency levels and heating fuels in new construction, and renovation and fuel-switch decisions, come from discrete choice models based on life-cycle costs (see :doc:`model`).
They need cost, discount rate, lifetime and heterogeneity inputs.

In **exogenous** mode, these shares and the renovation rate are inputs (``shr_eneff_new``, ``shr_fuel_heat_new``, ``shr_eneff_ren``, ``shr_fuel_heat_ren``, ``shr_fuel_heat_sw``, ``rate_ren``).
Stock dynamics and energy demand are computed in the same way in both modes.
