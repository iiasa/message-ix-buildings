Model description
*****************

.. contents::
   :local:
   :depth: 2

STURM combines a stock turnover model based on dynamic material flow analysis (MFA) with discrete choice models for energy efficiency and heating-fuel decisions (Mastrucci et al. 2021).
The stock model is stock-driven: the required stock follows from population, household size, floor space per capita and the shares of building types.
Construction and renovation decisions are activity-driven: they are set by choice models (endogenous mode) or by exogenous shares and rates (exogenous mode).
This page describes the method as implemented in the R code; references to scripts are to :file:`message_ix_buildings/sturm/model/`.

Structure of a run
==================

:func:`run_scenario` (:file:`F10_scenario_runs_MESSAGE_2100.R`) runs these steps:

1. **Load inputs** (:file:`F01_inputs_all.R`, ``fun_inputs_csv()``) and build the set of building cases: every combination of region, location, income class, climatic zone, building type, energy efficiency cohort and fuel (``fun_build_data_model()``).
2. **Initialise the stock** in the base year ``yrs[1]`` (:file:`F02_init_stock_dyn_fut.R`, ``fun_stock_init_fut()``): aggregate stock for all years, base-year stock by construction year, energy efficiency cohort and fuel, and base-year energy demand.
3. **For each later model year**:

   a. Energy intensities and, for endogenous residential runs, operating costs (:file:`F03_energy_demand.R`).
   b. Shares of new construction by energy efficiency cohort and heating fuel (:file:`F04_constr_decision.R`).
   c. Renovation rates, renovation options and fuel switching (:file:`F05_renov_switch_decision.R`).
   d. Stock update: demolitions, new construction, vacancy, renovation, fuel switching; energy demand and material flows (:file:`F06_stock_dyn_complete_rev.R`, ``fun_stock_dyn()``).

4. **Write reports** (:file:`R00_report_basic.R` to :file:`R05_report_NAVIGATE.R`, see :doc:`output`).

Helper functions, including the life-cycle cost and market share formulas, are in :file:`B00_functions.R`.

Representation of the building stock
====================================

The stock is described by these dimensions (values in :ref:`sturm-dimensions`):

- **Geography**: model region (``region_bld``; 61 regions in the current set-up, country-level in Europe), aggregate region (``region_gea``), urban or rural location (``urt``), climatic zone (``clim``).
- **Households** (residential): income class (``inc_cl``, terciles) and tenure (``tenr``, owner or renter; used in the renovation decision).
- **Buildings**: category (``mat``: permanent or informal), type (``arch``: single-family, multi-family, informal; ``comm`` for commercial), construction year (``yr_con``) and period (``bld_age``).
- **Energy performance**: energy efficiency cohort (``eneff``) and heating fuel (``fuel_heat``).

Energy efficiency cohorts distinguish existing buildings by construction period (``s1``, ``s2``, ``s3``), new buildings at a standard (``s51_std``) or advanced (``s52_low``) level, and buildings renovated to a standard or advanced level (``sr<period>1_std``, ``sr<period>2_low``).
"Standard" represents current practice and "advanced" an improved performance (Mastrucci et al. 2021).

The stock unit is the dwelling (household) for residential buildings and the square metre of floor area for commercial buildings.

Stock dynamics
==============

Required stock
--------------

The population of each region is split by location and climatic zone:

.. math::

   P_{r,u,c,t} = P_{r,t} \cdot s^{urt}_{r,u,t} \cdot s^{clim}_{r,u,c}

**Residential.** The number of dwellings is the population divided by household size, split equally among income classes, and distributed over building categories and types with the shares ``shr_mat`` and ``shr_arch``:

.. math::

   N_{t} = \frac{P_{t}}{n_{inc} \cdot hh_{t}} \cdot s^{mat}_{t} \cdot s^{arch}_{t}

Floor area is the number of dwellings times household size times floor space per capita (``floor_cap``, by building type).

**Commercial.** The stock is the floor area: population times commercial floor space per capita.

The change in the required stock between two model years is :math:`\Delta N_t = N_t - N_{t-1}`.

Base-year stock
---------------

The base-year stock is distributed by construction year with the shares ``stock_arch_base``.
Construction periods p1–p3 correspond to the existing cohorts ``s1``–``s3``.
Buildings on district heating follow ``shr_distr_heat``; the other buildings are split by heating fuel with ``shr_fuel_heat_base``.
Because the vintage distribution is not known for many regions, these shares were derived by running the stock turnover model over the past and calibrating it to available data (Mastrucci et al. 2021).

Demolition
----------

Demolitions follow Weibull survival curves by region, building type and construction period (``prob_dem``, parameters ``shape`` and ``scale``).
For buildings built in year :math:`y`, the share demolished between :math:`t-1` and :math:`t` is

.. math::

   p^{dem} = F(t - y) - F(t - 1 - y), \qquad F(a) = 1 - e^{-(a / \lambda)^k}

where :math:`k` is the shape and :math:`\lambda` the scale parameter, and :math:`t - 1` stands for the previous model year.

New construction and empty buildings
------------------------------------

New construction replaces demolished buildings and covers the increase in required stock:

.. math::

   N^{new}_t = \max(\Delta N_t + N^{dem}_t, 0)

If the required stock decreases by more than the demolitions, the difference becomes empty buildings, taken out of the occupied stock in proportion to the surviving cohorts:
:math:`N^{empty}_t = \max(-\Delta N_t - N^{dem}_t, 0)`.

New buildings get the current model year as construction year and are assigned to energy efficiency cohorts and fuels by the construction decision (below).
New buildings on district heating (share ``shr_distr_heat``) are assigned to the standard new cohort.

Vacancy
-------

With ``mod_vacant = "vacant"``, empty buildings are added to a stock of vacant dwellings, which starts from the base-year vacant stock (``stock_vacant_base``, distributed by type and period).
Each year a share ``rate_vacant_occ`` of vacant dwellings is re-occupied.
Re-occupied dwellings return to the occupied stock and replace new construction; they cannot exceed new construction in that period.

Decisions on new construction and renovation
============================================

Endogenous mode
---------------

In residential endogenous runs, households choose among energy efficiency and heating-fuel options by comparing life-cycle costs (Giraudet et al. 2012; Mastrucci et al. 2021).
For each option :math:`j`:

.. math::

   LCC_j = C^{inv}_j + C^{op}_j \cdot \frac{1 - (1 + r)^{-L}}{r} + C^{int}_j

- :math:`C^{inv}`: investment cost per dwelling: shell cost per m² times dwelling floor area, plus the heating system cost.
- :math:`C^{op}`: annual operating cost per dwelling: final energy demand times energy price, summed over fuels.
- :math:`C^{int}`: intangible cost, representing non-monetary barriers, built in the same way as the investment cost.
- :math:`r`: discount rate (``discount_new``, ``discount_ren``), which differs by region, building type and, for renovation, tenure.
- :math:`L`: time horizon (``lifetime_new``, ``lifetime_ren``).

The market share of each option follows

.. math::

   MS_j = \frac{LCC_j^{-\nu}}{\sum_k LCC_k^{-\nu}}

with heterogeneity parameter :math:`\nu` (``heterog_new``, ``heterog_ren``).

**New construction** (``fun_ms_new()``): options are combinations of the new energy efficiency cohorts and heating fuels, minus excluded combinations (``ct_fuel_excl_new``, ``ct_fuel_excl_reg``).

**Renovation** (``fun_ms_ren_sw()``): for each existing cohort and fuel, options are the allowed transitions to renovated cohorts (``ct_ren_eneff``) combined with the allowed fuel switches (``ct_ren_fuel_heat``), plus the option of no change.
Shares are computed separately for owners and renters and weighted by ``hh_tenure``.
The renovation rate is the share of households not choosing the "no change" option, per year of the time step, bounded by ``rate_ren_low`` and ``rate_ren_high``.
Renovation thus responds to costs, energy prices and policies.

Renovation applies to the stock that is not demolished or emptied in the period:
:math:`N^{ren} = (N - N^{dem} - N^{empty}) \cdot rate^{ren} \cdot \Delta t \cdot MS^{ren}`.

Exogenous mode
--------------

In residential exogenous runs and in all commercial runs, the shares of energy efficiency cohorts and fuels in new construction (``shr_eneff_new``, ``shr_fuel_heat_new``), the renovation options (``shr_eneff_ren``, ``shr_fuel_heat_ren``), fuel switching (``shr_fuel_heat_sw``) and the renovation rate (``rate_ren``) are inputs.

Fuel switching without renovation
---------------------------------

Buildings older than ``bld_age_min`` years that are not renovated can switch heating fuel at the rate ``rate_switch_fuel_heat``, with target fuels from the fuel-switch shares.

Informal housing
----------------

Informal (slum) housing (``mat = "sub"``, ``arch = "inf"``) is represented in urban areas of regions in the Global South.
It has its own survival curves, a single energy efficiency cohort (``ns``) and fuel, and is excluded from the choice models (``mod_decision = 0``).
Its share in the stock follows ``shr_mat``, so scenarios can represent improved access to decent housing.

Energy demand
=============

Space heating and cooling intensities per unit floor area (``en_int_heat``, ``en_int_cool``) are inputs by region, climatic zone, location, building type and energy efficiency cohort.
They come from the CHILLED model, which uses variable degree days computed on a 0.5° grid for a set of building archetypes (Mastrucci et al. 2021).
STURM converts them to final energy:

.. math::

   e^{heat} = \frac{i^{heat} \cdot (1 - sav^{ren})}{\eta^{heat}} \cdot s^{floor}_{heat} \cdot \frac{h^{heat}}{24}

.. math::

   e^{AC} = \frac{i^{cool} \cdot (1 - sav^{ren})}{\eta^{cool}} \cdot s^{acc}_{cool} \cdot s^{floor}_{cool} \cdot \frac{h^{cool}}{24}

where :math:`sav^{ren}` is the saving of renovated cohorts (``en_sav_ren``), :math:`\eta` the system efficiency (``eff_heat``, ``eff_cool``), :math:`s^{floor}` the heated or cooled share of floor area, :math:`h` the daily operating hours and :math:`s^{acc}_{cool}` the share with access to air conditioning.
Fans add electricity demand from the number of cooling days, operating hours and fan power.
All intensities are in kWh/m²/yr.

Only the share ``shr_need_heat`` of buildings needs space heating; the rest is reported with ``fuel_heat = "v_no_heat"``.

**Hot water** uses the space-heating fuel and the efficiency ``eff_hotwater``.
Residential demand is per person, multiplied by household size; commercial demand is per m².

**Other uses** are included for commercial buildings only (``en_int_others``).
Cooking and appliances are not modelled.

Final energy of the stock is the intensity times the floor area of each cohort and fuel.
Cooling and other uses are electricity.

Materials
=========

Material stocks and flows are computed from material intensities (``mat_int``, kg/m²) by region, building type and material (Mastrucci et al. 2024, 2026):

- **stock** = floor area of the stock × intensity;
- **demand** (inflow) = floor area of new construction × intensity;
- **scrap** (outflow) = floor area demolished × intensity.

Flows are divided by the length of the time step to give annual values.
Changes in construction practice, such as wood-based construction, are represented with different intensity files for new buildings.
Informal buildings are excluded from residential material accounting, given the uncertainty on their material composition (Mastrucci et al. 2024).

End-of-life treatment
---------------------

Demolition material is allocated to end-of-life treatments with the shares ``shr_mat_eol``:

- **reuse** and **recycling** are limited by the material demand of the same period, and reduce primary material demand: :math:`primary = \max(demand - reuse - recycling, 0)`;
- other treatments (e.g. **downcycling**) are scrap times their share;
- the remainder is reported as other treatment.

Cement is derived as 15 % of the concrete mass.

Residential and commercial buildings
====================================

.. list-table::
   :header-rows: 1
   :widths: 30 35 35

   * -
     - Residential (``resid``)
     - Commercial (``comm``)
   * - Stock unit
     - Dwelling
     - m² of floor area
   * - Floor area
     - Dwellings × household size × floor space per capita
     - Population × floor space per capita
   * - Income classes and tenure
     - Yes
     - No
   * - Building types
     - Single-family, multi-family, informal
     - One type (``comm``)
   * - New construction and renovation decisions
     - Endogenous or exogenous
     - Exogenous
   * - End uses
     - Space heating, cooling, hot water
     - Space heating, cooling, hot water, other uses

Link with MESSAGEix-GLOBIOM
===========================

STURM is soft-linked with MESSAGEix-GLOBIOM:

- **From MESSAGE**: energy prices by region and fuel, used for operating costs in the residential choice models.
- **To MESSAGE**: final energy demand by sector, end use and fuel, and material demand and demolition flows (see :doc:`output`).

Emission factors from MESSAGE scenarios can be applied to STURM's energy results to compute operational emissions; embodied emissions have been computed with prospective life-cycle assessment in applications (Mastrucci et al. 2024, 2026).

References
==========

- Giraudet, L.-G., Guivarch, C., Quirion, P. (2012). Exploring the potential for energy conservation in French households through hybrid modeling. *Energy Economics* 34(2), 426–445. https://doi.org/10.1016/j.eneco.2011.07.010
- Mastrucci et al. (2021), (2024), (2026): see :doc:`index`.
