STURM
*****

STURM (Stock TURnover Model of global buildings) is one model in the :doc:`MESSAGEix-Buildings <../index>` framework.
It projects the evolution of the residential and commercial building stock, the associated construction, demolition and renovation activities, energy efficiency and heating-fuel choices, final energy demand, and material flows, under alternative socio-economic, technology and policy scenarios.
STURM is written in R.

STURM combines a stock turnover model based on dynamic material flow analysis with discrete choice models for energy efficiency decisions in new construction and renovation (Mastrucci et al. 2021).
It represents the building stock with high granularity: regions and countries, urban and rural areas, climatic zones, income classes, tenure, housing types, energy efficiency cohorts and heating fuels.
STURM is soft-linked with the integrated assessment model MESSAGEix-GLOBIOM: it can read energy prices from MESSAGE and reports final energy and material demand in a format MESSAGE can use.

.. contents::
   :local:
   :depth: 1

Overview
========

At each model time step, STURM:

1. Computes the required building stock from population, urbanisation, household size and floor space per capita.
2. Computes demolitions with survival curves by building type and vintage.
3. Computes the new construction needed to replace demolished buildings and meet the change in demand.
4. Assigns new buildings to energy efficiency levels and heating fuels, and renovates part of the existing stock, either with cost-based choice models (endogenous mode) or with exogenous shares and rates (exogenous mode).
5. Updates the stock by vintage, energy efficiency level and fuel.
6. Computes final energy demand for space heating, space cooling and hot water (and, for commercial buildings, other uses), and material stocks and flows from material intensities.
7. Writes reports.

The model covers residential (``resid``) and commercial (``comm``) buildings.
The current global set-up has 61 regions (country-level for Europe), with five-year steps to 2060 and ten-year steps to 2100.

Documentation
=============

.. toctree::
   :maxdepth: 2

   usage
   input
   model
   output
   whatsnew

Code location
=============

All STURM files are in :file:`message_ix_buildings/sturm/` in the `message-ix-buildings repository <https://github.com/iiasa/message-ix-buildings>`_:

- :file:`model/` — R code of the model (scripts ``B00``, ``F01``–``F10``, report scripts ``R00``–``R05``).
- :file:`data/` — input lists and input data (CSV).
- :file:`run_STURM_*.R` — runner scripts.
- :file:`output/` — created by the runners for results (not tracked in git).

References
==========

Please cite the following when using STURM:

- Mastrucci, A., van Ruijven, B., Byers, E., Poblete-Cazenave, M., Pachauri, S. (2021).
  Global scenarios of residential heating and cooling energy demand and CO\ :sub:`2` emissions.
  *Climatic Change* 168, 14.
  https://doi.org/10.1007/s10584-021-03229-3
  — model description and residential implementation.

Applications with material flows and the commercial sector:

- Mastrucci, A., Guo, F., Zhong, X., Maczek, F., van Ruijven, B. (2024).
  Circular strategies for building sector decarbonization in China: A scenario analysis.
  *Journal of Industrial Ecology* 28, 1089–1102.
  https://doi.org/10.1111/jiec.13523
- Mastrucci, A., Steinberger-Maierhofer, D., Zhong, X., Alaux, N., Creutzig, F., Maczek, F., Quade, M., Röck, M., Sheng, D., Gidden, M.J., van Ruijven, B. (2026).
  Carbon storage and mitigation potential of structural wood and biochar-based concrete in global urban building scenarios.
  *Environmental Research Letters* 21, 184015.
  https://doi.org/10.1088/1748-9326/ae9f73
