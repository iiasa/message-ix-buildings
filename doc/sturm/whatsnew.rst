What's new
**********

Changes to STURM that affect inputs, settings or results.
Add an entry here in every pull request that changes the model.

Next release
============

- New documentation: :doc:`usage`, :doc:`input`, :doc:`model`, :doc:`output`.
- End-of-life treatment of demolition materials (reuse, recycling, downcycling, other) for residential and commercial buildings, with new material report columns (:pull:`42`).
- Vacant dwellings and their re-occupation (``mod_vacant = "vacant"``), currently for EU-27 residential runs (:pull:`42`).
- New runner scripts :file:`run_STURM_resid_main_2026.R`, :file:`run_STURM_resid_exo_main_2026.R` and :file:`run_STURM_comm_main_2026.R`, replacing the 2023 runners (:pull:`42`).
- Commercial base stock given as shares by construction period.
- Urban/rural and climate-zone population shares read as separate inputs (``pop_urt``, ``pop_clim``).
