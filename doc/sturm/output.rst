Output
******

.. contents::
   :local:
   :depth: 2

Two arguments of :func:`run_scenario` control the output:

- ``report_type`` selects the report families: ``"STURM"`` (the standard STURM tables), ``"MESSAGE"`` (input for MESSAGEix-GLOBIOM), and the project templates ``"IRP"``, ``"NGFS"``, ``"NAVIGATE"``.
- ``report_var`` selects the content: ``"energy"``, ``"material"``, ``"vintage"``.

The 2026 runners use ``report_type = c("STURM", "MESSAGE")`` and ``report_var = c("energy", "material")`` by default.
Reports are produced by the scripts :file:`model/R00_report_basic.R` (STURM) to :file:`model/R05_report_NAVIGATE.R`.

STURM reports
=============

:file:`model/R00_report_basic.R` writes one CSV file per selected ``report_var``:

.. code-block:: text

   report_STURM_<scenario>_<sector>_<geo_level_report>_<report_var>.csv

The runners then rename the files to add the region selection, e.g. :file:`report_STURM_SSP2_resid_region_bld_energy_global.csv` (suffix ``global``, ``EU27`` or ``custom_<ISO3 codes>``, plus ``_v`` when vacancy is on).
The exogenous runner writes :file:`report_STURM_<scenario>_resid_<geo_level_report>_exogenous_<report_var>.csv` to :file:`output/resid_exogenous_<regions>/<scenario>/`.

If ``geo_level_report`` equals ``geo_level`` (as in all runners), the tables keep every dimension.
Otherwise they are aggregated to ``geo_level_report`` and the ``clim`` and ``inc_cl`` columns are dropped.

Energy (``energy``)
-------------------

Dimensions: ``region_bld``, ``region_gea``, ``urt``, ``clim``, ``inc_cl``, ``arch``, ``mat``, ``eneff``, ``fuel_heat``, ``fuel_cool``, ``scenario``, ``year``.
Includes the base year.

.. list-table::
   :header-rows: 1
   :widths: 25 20 55

   * - Column
     - Unit
     - Content
   * - ``stock_M``
     - million dwellings (resid), million m² (comm)
     - Building stock. For commercial buildings, the stock is counted in floor area, so ``stock_M`` equals ``floor_Mm2``.
   * - ``floor_Mm2``
     - million m²
     - Floor area.
   * - ``heat_TJ``
     - TJ/yr
     - Final energy for space heating.
   * - ``cool_TJ``
     - TJ/yr
     - Final energy for space cooling (``cool_ac_TJ`` + ``cool_fans_TJ``).
   * - ``cool_ac_TJ``
     - TJ/yr
     - Final energy for air conditioning.
   * - ``cool_fans_TJ``
     - TJ/yr
     - Final energy for fans.
   * - ``hotwater_TJ``
     - TJ/yr
     - Final energy for hot water.
   * - ``other_uses_TJ``
     - TJ/yr
     - Other uses: commercial only; always 0 for residential.

Buildings without space heating need are reported with ``fuel_heat = "v_no_heat"`` and zero heating and hot water energy.

Material (``material``)
-----------------------

Dimensions: ``region_bld``, ``region_gea``, ``urt``, ``clim``, ``inc_cl``, ``arch``, ``mat``, ``eneff``, ``material``, ``scenario``, ``year``.
Starts from the second model year (no base-year row).

.. list-table::
   :header-rows: 1
   :widths: 25 20 55

   * - Column
     - Unit
     - Content
   * - ``floor_tot_Mm2``
     - million m²
     - Floor area of the stock.
   * - ``floor_new_Mm2``
     - million m²/yr
     - Floor area of new construction.
   * - ``floor_dem_Mm2``
     - million m²/yr
     - Floor area demolished.
   * - ``mat_stock_Mt``
     - Mt
     - Material stock.
   * - ``mat_demand_Mt``
     - Mt/yr
     - Material demand for new construction (inflow).
   * - ``mat_scrap_Mt``
     - Mt/yr
     - Material from demolition (outflow).
   * - ``mat_int``
     - kg/m²
     - Average material intensity of the stock (only in non-aggregated reports).
   * - ``mat_reuse_Mt``, ``mat_recycling_Mt``
     - Mt/yr
     - Demolition material reused or recycled into new construction, limited by material demand.
   * - ``mat_<treatment>_Mt``
     - Mt/yr
     - Other end-of-life treatments in ``shr_mat_eol``, e.g. ``mat_downcycling_Mt``.
   * - ``mat_other_treat_Mt``
     - Mt/yr
     - Remaining demolition material.
   * - ``mat_primary_Mt``
     - Mt/yr
     - Primary material demand: demand minus reuse and recycling.

Material flows per year are flows over the time step divided by its length.
Informal buildings (``mat = "sub"``) are not included in residential material results.
A ``cement`` row is derived as 15 % of ``concrete``.

Vintage (``vintage``)
---------------------

Number of dwellings (``n_units``) by ``year``, construction year ``yr_con``, ``urt``, ``arch``, ``mat`` and ``eneff``.

.. note:: In this version the vintage file contains the material table instead of the vintage table.
   This is fixed in a later version.

Vacant dwellings
----------------

Written when ``mod_vacant = "vacant"`` (file suffix ``_vacant``): ``stock_vacant_M`` (vacant dwellings) and ``stock_reocc_M`` (dwellings re-occupied in the time step) [million dwellings], by ``urt``, ``arch``, ``mat``, ``eneff`` and region.

MESSAGE report
==============

:file:`model/R01_report_MESSAGE.R` returns a table in the format of MESSAGE parameters, with columns ``node``, ``commodity``, ``level``, ``year``, ``time``, ``value``, ``unit``.
The runners write it to :file:`report_MESSAGE_<sector>_<regions>_<scenario>.csv` (exogenous runner: :file:`report_MESSAGE_<scenario>_resid_<geo_level_report>_exogenous.csv`), after removing the ``*_v_no_heat`` commodities.

.. list-table::
   :header-rows: 1
   :widths: 40 15 15 30

   * - ``commodity``
     - ``level``
     - ``unit``
     - Content
   * - ``<sector>_<end_use>_<fuel>``
     - ``final``
     - GWa
     - Final energy by end use (``heat``, ``cool``, ``hotwater``, ``other_uses``) and fuel.
       Fuels use MESSAGE names: ``biomass``, ``coal``, ``d_heat``, ``electr``, ``gas``, ``lightoil``.
       Cooling and other uses are electricity.
   * - ``<sector>_mat_demand_<material>``
     - ``demand``
     - Mt/y
     - Material demand.
   * - ``<sector>_mat_scrap_<material>``
     - ``demand``
     - Mt/y
     - Demolition material.
   * - ``<sector>_floor_construction``, ``<sector>_floor_demolition``
     - ``demand``
     - Mm2/y
     - Floor area built and demolished.
   * - ``<sector>_mat_int_demand_<material>``, ``<sector>_mat_int_scrap_<material>``
     - ``demand``
     - Mt/Mm2
     - Material intensity of construction and demolition.

The ``node`` is ``<geo_level_report>_<region>``.
Material rows include permanent buildings only.

Project templates
=================

These reports are in IAMC format (columns ``Model``, ``Scenario``, ``Region``, ``Variable``, ``Unit`` and one column per year) and were developed for specific projects.
They are not produced by default.

.. list-table::
   :header-rows: 1
   :widths: 15 30 55

   * - ``report_type``
     - Script
     - Content
   * - ``IRP``
     - :file:`R02_report_IRP.R`
     - International Resource Panel: floor area stock, additions and retirements; final energy by end use; material demand, scrap and stock.
   * - ``NGFS``
     - :file:`R03_report_NGFS.R`
     - NGFS: floor space and final energy by end use and carrier.
   * - ``NAVIGATE``
     - :file:`R05_report_NAVIGATE.R`
     - NAVIGATE: floor space, renovation rate, final energy by end use and carrier, materials.

File names: :file:`report_<type>_<scenario>_<sector>_<geo_level_report>.csv`.
