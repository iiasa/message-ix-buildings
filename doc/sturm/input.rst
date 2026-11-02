Input data
**********

.. contents::
   :local:
   :depth: 2

Input lists
===========

STURM reads its inputs through an **input list**: a CSV file in :file:`message_ix_buildings/sturm/data/` that maps each model parameter to an input data file, for each scenario.

.. code-block:: text

   name_parameter,SSP2,SSP1,SSP3,...,R,N_tp,...
   geo_data,regions_R61,,,...,regions_R61,,...
   pop,R61_pop_ssp2_2026-01-29,R61_pop_ssp1_2026-01-29,...

- The first column, ``name_parameter``, gives the parameter name used in the code.
- Every other column is a scenario.
  A cell gives the name of a CSV file in the input data folder, **without** the :file:`.csv` extension.
- An empty cell means "same as the reference scenario".
  The reference scenario is the **second column** of the list (``SSP2`` in the global lists, ``R`` in the CircEUlar lists).
- Every file named in the list must exist; there are no optional files.
  Parameters that the selected mode does not use can be listed but are ignored.

When a scenario runs, the loader (:file:`model/F01_inputs_all.R`, ``fun_inputs_csv()``) reads each file and stores it under its parameter name.
In each file, the column named ``value`` is renamed to the parameter name.

Available lists:

.. list-table::
   :header-rows: 1
   :widths: 35 15 25 25

   * - Input list
     - Sector
     - Data folder
     - Scenarios
   * - :file:`input_list_resid_2026_SSP_CE.csv`
     - resid
     - :file:`input_csv_SSP_2023_resid/`
     - SSP1–5, SSP1_LED, SSP2_LED, R and circular-economy scenarios
   * - :file:`input_list_resid_CircEUlar_2026.csv`
     - resid (EU-27)
     - :file:`input_csv_SSP_2023_resid/`
     - R and circular-economy scenarios; includes vacancy inputs
   * - :file:`input_list_resid_2026_exogenous.csv`
     - resid (exogenous)
     - :file:`input_csv_SSP_2023_resid_ex/`
     - IND_SSP1–3
   * - :file:`input_list_comm_2026_SSP_CE.csv`
     - comm
     - :file:`input_csv_SSP_2023_comm/`
     - SSP1–5, SSP1_LED, SSP2_LED, LED, R and circular-economy scenarios
   * - :file:`input_list_comm_CircEUlar_2026.csv`
     - comm (EU-27)
     - :file:`input_csv_SSP_2023_comm/`
     - R, circular-economy and SSP scenarios

To add a scenario, add a column to the list and fill only the cells that differ from the reference scenario.

Input data files
================

Input data files are CSV files in long format: one column per dimension and a ``value`` column.
Files are joined to the model data by their column names, so the set of dimension columns of a file can vary.
For example, the demolition parameters (``prob_dem``) can be given by ``region_gea`` or by ``region_bld``, with or without ``year``.
The dimensions below are those of the files used in the reference scenarios; dimension names are explained in :ref:`sturm-dimensions`.

Column "Used in": **all** = always required; **resid** / **comm** = one sector only; **endo** = residential endogenous mode only; **exo** = exogenous mode (residential exogenous, and always for commercial); **vacant** = vacancy mode only.

Definitions and code lists
--------------------------

.. list-table::
   :header-rows: 1
   :widths: 22 38 10 30

   * - Parameter
     - Dimensions
     - Used in
     - Content
   * - ``geo_data``
     - ``region_bld``, ``region_gea``, ``R11``, ``R12``
     - all
     - Model regions and their mapping to aggregate regions.
   * - ``clim_zones``
     - ``region_bld``, ``urt``, ``clim``
     - all
     - Climatic zones present in each region.
   * - ``ct_bld``
     - ``region_gea``, ``urt``, ``arch``, ``mat``
     - all
     - Housing types present by region and location.
   * - ``ct_bld_age``
     - ``bld_age_id``, ``bld_age_name``, ``mat``, ``year_i``, ``year_f``
     - all
     - Construction periods (first and last year).
   * - ``ct_eneff``
     - ``eneff``, ``mat``, ``bld_age``
     - all
     - Energy efficiency cohorts and their construction period.
   * - ``ct_fuel_comb``
     - ``mat``, ``fuel_heat``, ``fuel_cool``, ``mod_decision``
     - all
     - Allowed heating/cooling fuel combinations; ``mod_decision = 0`` excludes a type from the choice models (informal housing).
   * - ``ct_inc_cl``
     - ``urt``, ``inc_cl``
     - all
     - Income classes. The number of classes divides the population (equal shares).
   * - ``ct_tenr``
     - ``mat``, ``tenr``
     - endo
     - Tenure types (owner, renter).
   * - ``ct_ren_eneff``
     - ``mat``, ``eneff_i``, ``eneff_f``
     - endo
     - Allowed renovation transitions between energy efficiency cohorts.
   * - ``ct_ren_fuel_heat``
     - ``mat``, ``fuel_heat_i``, ``fuel_heat_f``, ``value``
     - endo
     - Allowed heating-fuel switches at renovation (``value = 1``).
   * - ``ct_fuel_excl_new``, ``ct_fuel_excl_ren``, ``ct_fuel_excl_reg``
     - see files
     - endo
     - Fuel options excluded in new construction, in renovation, and by region.
   * - ``ct_fuel_dhw``
     - ``mat``, ``fuel_heat``, ``fuel_cool``, ``res_solar``, ``mod_decision``
     - —
     - Listed but not used by the current code.

Demographics and floor space
----------------------------

.. list-table::
   :header-rows: 1
   :widths: 22 38 10 30

   * - Parameter
     - Dimensions
     - Used in
     - Content and unit
   * - ``pop``
     - ``region_bld``, ``year``
     - all
     - Population [million].
   * - ``pop_urt``
     - ``region_bld``, ``year``, ``urt``
     - all
     - Share of population in urban and rural areas [–].
   * - ``pop_clim``
     - ``region_bld``, ``urt``, ``clim``
     - all
     - Share of population by climatic zone [–].
   * - ``hh_size``
     - ``region_bld``, ``urt``, ``year``
     - resid
     - Household size [persons per household].
   * - ``floor_cap``
     - resid: ``region_bld``, ``urt``, ``arch``, ``mat``, ``year``; comm: ``region_gea``, ``year``
     - all
     - Floor space per capita [m²/person].
   * - ``hh_tenure``
     - ``region_gea``, ``urt``, ``mat``, ``tenr``, ``year``
     - endo
     - Share of households by tenure [–].

Building stock
--------------

.. list-table::
   :header-rows: 1
   :widths: 22 38 10 30

   * - Parameter
     - Dimensions
     - Used in
     - Content and unit
   * - ``shr_mat``
     - resid: ``region_bld``, ``clim``, ``urt``, ``inc_cl``, ``mat``, ``year``; comm: ``urt``, ``mat``, ``year``
     - all
     - Share of permanent (``perm``) and informal (``sub``) buildings [–].
   * - ``shr_arch``
     - ``region_gea``, ``urt``, ``mat``, ``arch``, ``year`` (EU-27: by ``region_bld``, ``clim``)
     - all
     - Share of housing types within ``mat`` [–].
   * - ``stock_arch_base``
     - ``region_bld``, ``region_gea``, ``urt``, ``clim``, ``mat``, ``arch``, ``yr_con``, ``bld_age``
     - all
     - Base-year distribution of the stock by construction year [share].
   * - ``prob_dem``
     - ``region_gea``, ``mat``, ``arch``, ``bld_age``, ``parameter``; optionally ``region_bld``, ``year``
     - all
     - Weibull survival parameters: rows with ``parameter = shape`` and ``parameter = scale`` [years].
   * - ``shr_distr_heat``
     - ``region_gea``, ``urt``
     - all
     - Share of buildings on district heating [–].
   * - ``shr_fuel_heat_base``
     - ``region_gea``, ``urt``, ``fuel_heat``
     - all
     - Base-year shares of heating fuels in buildings not on district heating [–].

Energy demand
-------------

.. list-table::
   :header-rows: 1
   :widths: 22 38 10 30

   * - Parameter
     - Dimensions
     - Used in
     - Content and unit
   * - ``en_int_heat``
     - ``region_bld``, ``clim``, ``urt``, ``arch``, ``eneff``
     - all
     - Space heating useful energy intensity [kWh/m²/yr].
   * - ``en_int_cool``
     - ``region_bld``, ``clim``, ``urt``, ``arch``, ``eneff``
     - all
     - Space cooling useful energy intensity [kWh/m²/yr].
   * - ``days_cool``
     - ``region_bld``, ``clim``, ``urt``, ``arch``, ``eneff``
     - all
     - Number of cooling days per year (used for fans).
   * - ``shr_need_heat``
     - ``region_bld``, ``clim``, ``urt``, ``arch``, ``eneff``
     - all
     - Share of buildings that need space heating [–]; the rest is reported with ``fuel_heat = v_no_heat``.
   * - ``shr_need_cool``
     - ``region_bld``, ``clim``, ``urt``, ``arch``, ``eneff``
     - —
     - Listed but not used by the current code.
   * - ``en_sav_ren``
     - ``eneff``, ``year``
     - all
     - Energy savings of renovated cohorts relative to the original cohort [–].
   * - ``eff_heat``
     - ``eneff``, ``region_gea``, ``fuel_heat``, ``year``
     - all
     - Efficiency of heating systems [–].
   * - ``eff_cool``
     - ``region_gea``, ``fuel_cool``, ``year``
     - all
     - Efficiency of cooling systems [–].
   * - ``eff_hotwater``
     - ``eneff``, ``region_gea``, ``fuel_heat``, ``year``
     - all
     - Efficiency of hot water systems [–].
   * - ``shr_floor_heat``, ``shr_floor_cool``
     - ``region_gea`` (optionally ``year``)
     - all
     - Share of floor area heated / cooled [–].
   * - ``hours_heat``
     - ``clim``, ``region_gea``, ``year``
     - all
     - Daily hours of heating operation [h].
   * - ``hours_cool``, ``hours_fans``
     - ``region_gea``
     - all
     - Daily hours of cooling and fan operation [h].
   * - ``power_fans``
     - ``region_gea``
     - all
     - Power of a fan [W].
   * - ``shr_acc_cool``
     - ``region_bld``, ``clim``, ``urt``, ``year`` (resid also ``inc_cl``)
     - all
     - Share of households (resid) or floor area (comm) with access to air conditioning [–].
   * - ``en_int_hotwater``
     - ``region_gea``, ``year``
     - all
     - Hot water useful energy demand: resid [GJ/person/yr, multiplied by household size]; comm [kWh/m²/yr].
   * - ``en_int_others``
     - ``region_gea``, ``year``
     - comm
     - Final energy intensity of other uses [kWh/m²/yr].

.. todo:: Confirm the unit of ``en_int_hotwater`` (residential) and the source of the heating and cooling intensities (CHILLED runs).

Materials
---------

.. list-table::
   :header-rows: 1
   :widths: 22 38 10 30

   * - Parameter
     - Dimensions
     - Used in
     - Content and unit
   * - ``mat_int``
     - ``region_gea``, ``urt``, ``arch``, ``material``, ``year`` (RASMI-based files: ``region_bld``, ``region_gea``, ``urt``, ``arch``, ``material``)
     - all
     - Material intensity of construction [kg/m²].
   * - ``shr_mat_eol``
     - ``region_gea``, ``material``, ``year``, ``eol_treat``
     - all
     - Shares of end-of-life treatments of demolition waste (``reuse``, ``recycling``, ``downcycling``, ``others``) [–].

Decisions: endogenous mode
--------------------------

Residential runs with ``mod_new = "endogenous"`` and ``mod_ren = "endogenous"``.

.. list-table::
   :header-rows: 1
   :widths: 30 40 30

   * - Parameter
     - Dimensions
     - Content
   * - ``cost_invest_new_shell``
     - ``region_gea``, ``eneff``, ``year``
     - Investment cost of the building shell, new construction [per m²].
   * - ``cost_invest_new_heat``
     - ``region_gea``, ``eneff``, ``fuel_heat``, ``year``
     - Investment cost of the heating system, new construction [per dwelling].
   * - ``cost_intang_new_shell``, ``cost_intang_new_heat``
     - as investment costs
     - Intangible costs, new construction.
   * - ``cost_invest_ren_shell``
     - ``region_gea``, ``eneff``, ``eneff_f``, ``year``
     - Investment cost of shell renovation [per m²].
   * - ``cost_invest_ren_heat``
     - ``region_gea``, ``fuel_heat_f``, ``year``
     - Investment cost of a new heating system at renovation [per dwelling].
   * - ``cost_intang_ren_shell``
     - ``region_gea``, ``eneff``, ``mat``, ``eneff_f``, ``year``
     - Intangible cost of renovation.
   * - ``cost_intang_ren_heat``
     - ``mat``, ``fuel_heat_i``, ``fuel_heat_f``
     - Intangible cost of a heating-fuel switch.
   * - ``discount_new``
     - ``region_gea``, ``urt``, ``arch``
     - Discount rate, new construction [–].
   * - ``discount_ren``
     - ``region_gea``, ``urt``, ``arch``, ``tenr``
     - Discount rate, renovation, by tenure [–].
   * - ``lifetime_new``, ``lifetime_ren``
     - ``region_gea``
     - Time horizon of the life-cycle cost [years].
   * - ``heterog_new``, ``heterog_ren``
     - ``region_gea``
     - Heterogeneity parameter of the choice model [–].
   * - ``rate_ren_low``, ``rate_ren_high``
     - ``region_gea``, ``year``
     - Lower and upper bounds of the renovation rate [1/yr].
   * - ``price_en``
     - ``region_gea``, ``fuel``, ``year``
     - Energy prices, used only if ``run_scenario()`` gets ``prices = NULL``.

.. todo:: State the currency and price year of costs and energy prices.

Decisions: exogenous mode
-------------------------

Residential runs with ``mod_new = "exogenous"`` and ``mod_ren = "exogenous"``, and all commercial runs.

.. list-table::
   :header-rows: 1
   :widths: 30 40 30

   * - Parameter
     - Dimensions
     - Content
   * - ``shr_eneff_new``
     - ``region_gea``, ``mat``, ``eneff``, ``year``
     - Shares of energy efficiency cohorts in new construction [–].
   * - ``shr_fuel_heat_new``
     - ``mat``, ``eneff``, ``fuel_heat``, ``fuel_cool``, ``year``
     - Shares of heating fuels in new construction [–].
   * - ``shr_eneff_ren``
     - ``region_gea``, ``mat``, ``eneff_i``, ``eneff_f``, ``year``
     - Shares of renovation options [–].
   * - ``shr_fuel_heat_ren``
     - ``mat``, ``eneff_i``, ``eneff_f``, ``fuel_heat_f``, ``year``
     - Shares of heating fuels after renovation [–].
   * - ``shr_fuel_heat_sw``
     - ``mat``, ``fuel_heat``, ``fuel_heat_f``, ``year``
     - Shares of target fuels in fuel switching without renovation [–].
   * - ``rate_ren``
     - ``region_gea``, ``year`` (residential exogenous: also ``mat``)
     - Renovation rate [1/yr].

Fuel switching and vacancy
--------------------------

.. list-table::
   :header-rows: 1
   :widths: 22 38 10 30

   * - Parameter
     - Dimensions
     - Used in
     - Content and unit
   * - ``rate_switch_fuel_heat``
     - ``region_gea``, ``fuel_heat``, ``bld_age_min``, ``year``
     - all
     - Rate of heating-fuel switching without renovation [1/yr], for buildings older than ``bld_age_min`` years.
   * - ``stock_vacant_base``
     - ``year``, ``region_bld``, ``clim``, ``arch``, ``urt``, ``region_gea``
     - vacant
     - Vacant dwellings in the base year [dwellings].
   * - ``shr_vacant_base_arch``
     - ``region_bld``, ``clim``, ``urt``, ``mat``, ``arch``, ``year``
     - vacant
     - Share of vacant dwellings by housing type [–].
   * - ``shr_vacant_base_period``
     - ``region_bld``, ``region_gea``, ``urt``, ``clim``, ``mat``, ``arch``, ``yr_con``, ``bld_age``
     - vacant
     - Share of vacant dwellings by construction period [–].
   * - ``rate_vacant_occ``
     - ``region_gea``, ``year``
     - vacant
     - Re-occupation rate of vacant dwellings [1/yr].

Energy prices from MESSAGE
==========================

The residential runners read energy prices from :file:`data/input_prices_R12.csv`, a MESSAGE output with columns ``node``, ``commodity``, ``level``, ``year``, ``time``, ``lvl``, ``mrg``.
:func:`run_scenario` converts ``lvl`` with the factor 31.71, maps the MESSAGE commodities to STURM fuels (``biomass`` → ``biomass_solid``, ``electr`` → ``electricity``, ``lightoil`` → ``oil``, ``coal``, ``gas``; ``d_heat`` is not used) and joins the prices to the regions through the ``R11`` column of ``geo_data``.

.. todo:: Document the unit of ``lvl`` and of the converted price.

.. _sturm-dimensions:

Dimensions
==========

.. list-table::
   :header-rows: 1
   :widths: 15 25 60

   * - Name
     - Meaning
     - Values
   * - ``region_bld``
     - Model region
     - 61 regions in :file:`regions_R61.csv`: 31 European countries (``C-WEU-*``, ``C-EEU-*``: EU-27, CHE, GBR, ISL, NOR) and 30 ``R32*`` regions.
   * - ``region_gea``
     - Aggregate region
     - 11 regions: AFR, CPA, EEU, FSU, LAC, MEA, NAM, PAO, PAS, SAS, WEU.
   * - ``R11``, ``R12``
     - MESSAGE regions
     - Mapping to the MESSAGEix-GLOBIOM R11 and R12 regions.
   * - ``urt``
     - Location
     - ``urb`` (urban), ``rur`` (rural).
   * - ``clim``
     - Climatic zone
     - 20 zones, from ``Zone_0A Extremely hot Humid`` to ``Zone_8A Subarctic/arctic Humid``.
   * - ``inc_cl``
     - Income class
     - Residential: ``q1``, ``q2``, ``q3`` (terciles); commercial: ``ns``.
   * - ``tenr``
     - Tenure
     - ``own``, ``rent``.
   * - ``mat``
     - Building category
     - ``perm`` (permanent), ``sub`` (informal).
   * - ``arch``
     - Building type
     - Residential: ``sfh`` (single-family), ``mfh`` (multi-family), ``inf`` (informal, urban only); commercial: ``comm``.
   * - ``bld_age``
     - Construction period
     - ``p1`` (before 1945), ``p2`` (1946–1990), ``p3`` (1991–2015), ``p5`` (2016–2100), ``ns`` (informal).
   * - ``yr_con``
     - Construction year
     - Base stock: 1945 and 1960–2020 in five-year steps; new buildings: the model year.
   * - ``eneff``
     - Energy efficiency cohort
     - Existing: ``s1``, ``s2``, ``s3`` (periods p1–p3); new: ``s51_std``, ``s52_low``; renovated: ``sr11_std``, ``sr12_low``, ``sr21_std``, ``sr22_low``, ``sr31_std``, ``sr32_low``; informal: ``ns``.
   * - ``fuel_heat``
     - Heating fuel
     - ``biomass_solid``, ``coal``, ``district_heat``, ``electricity``, ``gas``, ``oil``; in results also ``v_no_heat`` (no heating need).
   * - ``fuel_cool``
     - Cooling fuel
     - ``electricity``.
   * - ``material``
     - Construction material
     - As in the ``mat_int`` file (``aluminum``, ``brick`` (RASMI files only), ``concrete``, ``copper``, ``glass``, ``steel``, ``wood``); ``cement`` is derived in the results.
   * - ``eol_treat``
     - End-of-life treatment
     - ``reuse``, ``recycling``, ``downcycling``, ``others``.
