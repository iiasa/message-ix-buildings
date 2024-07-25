.. currentmodule:: message_ix_buildings.chilled

CHILLED (:mod:`.chilled`)
*************************

:mod:`.chilled` (Cooling and HeatIng gLobaL Energy Demand) is a model that assesses the impacts of climate change (temperature change).

Methods
=======


Data
====

Input data
----------


Parameters
----------

In this module, the "version" refers to the version of the model, not the version of the data. For example, the "ALPS2023" version refers to the set of scenario inputs/parameters for the "ALPS2023" run.
When specifying a version, CHILLED will look in the `/data/chilled/version` directory that the version name exists and that the following files exist as well:

- `arch_input_reg.xlsx` (or `arch_input.xlsx`)
- `arch_regions.xlsx`
- `par_var.csv`
- `runs.csv`

Usage
=====

The preprocessing (preparing MESSAGE region rasters and country codes) happens without specifying GCM or RCP scenario. Therefore, it can be run first on its own using the following command (from within :mod:`chilled.run`):

.. code-block:: bash

   python -m preprocess.py -version "version_name"

If the `-version` command is not provided, then the default version is "ALPS2023".

The main model can be run using the following command:

.. code-block:: bash

   python -m main.py -version "version_name" -gcm "gcm_name" -rcp "rcp_name"

If the `-version` command is not provided, then the default version is "ALPS2023". 
If the `-gcm` and `-rcp` commands are not provided, then the default GCM is "GFDL-ESM2M" and the default RCP is "baseline".


Code reference
==============

.. automodule:: message_ix_buildings.chilled
   :members:

.. currentmodule:: message_ix_buildings.chilled.preprocess.message_raster

Create raster of MESSAGE regions (:mod:`~.chilled.preprocess.message_raster`)
-----------------------------------------------------------------------------

.. automodule:: message_ix_buildings.chilled.preprocess.message_raster
   :members:

   .. autosummary::

      create_message_raster

