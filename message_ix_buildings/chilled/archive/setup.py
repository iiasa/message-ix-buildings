import os


def create_dummy_folders(dle_path, version_name, gcm, rcp_scenario):
    """Create dummy folders for outputs"""
    output_path = os.path.join(dle_path, f"output_data_{version_name}")

    if not os.path.exists(os.path.join(output_path, "output")):
        os.makedirs(os.path.join(output_path, "output"))

    if not os.path.exists(
        os.path.join(output_path, gcm, rcp_scenario, "2_VDD_ene_calcs")
    ):
        os.makedirs(os.path.join(output_path, gcm, rcp_scenario, "2_VDD_ene_calcs"))

    if not os.path.exists(
        os.path.join(output_path, gcm, rcp_scenario, "3_floorarea_country_data")
    ):
        os.makedirs(
            os.path.join(output_path, gcm, rcp_scenario, "3_floorarea_country_data")
        )

    if not os.path.exists(os.path.join(output_path, gcm, rcp_scenario, "4_final_maps")):
        os.makedirs(os.path.join(output_path, gcm, rcp_scenario, "4_final_maps"))

    if not os.path.exists(os.path.join(output_path, gcm, rcp_scenario, "5_ISO_tables")):
        os.makedirs(os.path.join(output_path, gcm, rcp_scenario, "5_ISO_tables"))

    if not os.path.exists(os.path.join(output_path, gcm, rcp_scenario, "6_graphs")):
        os.makedirs(os.path.join(output_path, gcm, rcp_scenario, "6_graphs"))

    if not os.path.exists(os.path.join(output_path, gcm, rcp_scenario, "6_maps")):
        os.makedirs(os.path.join(output_path, gcm, rcp_scenario, "6_maps"))

    if not os.path.exists(os.path.join(output_path, gcm, rcp_scenario, "7_emulator")):
        os.makedirs(os.path.join(output_path, gcm, rcp_scenario, "7_emulator"))

    if not os.path.exists(os.path.join(output_path, gcm, "output_emulator")):
        os.makedirs(os.path.join(output_path, gcm, "output_emulator"))


def run_setup(cfg):
    # Run create_dummy_folders
    create_dummy_folders(cfg.dle_path, cfg.vstr, cfg.gcm, cfg.rcp)
