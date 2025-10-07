By: Measrainsey Meng
Date: 23 January 2025

# What's new?

## Where the code is housed

CHILLED has now been migrated to https://github.com/iiasa/message-ix-buildings. But we keep https://github.com/iiasa/cc_ene_dem to reference back to.

## Switching from scripting to functions as much as possible

To refrain from having multiple scripts that are broken into components where one script is run at a time to set parameters, I would recommend moving to a function-based codebase as much as possible, where functions are called upon and input parameters are specified. This hopefully allows for easier tracking of where things are coming and going. I tried to implement this where I could but admittedly there's still a lot of work to be done.

## New input files format + "version"s

First, I think moving from Excel .xlsx files to CSV files would be beneficial as it would allow us to better track changes and see what inputs and parameters have changed.

Additionally, instead of having input files in the `input_data` in our IIASA shared drive where we have different .xlsx files for each version with versioning numbers and names such as "v12", "v19", etc, I would recommend moving to a system where each "version" of our run is treated as its own set of input files. So for example, `ALPS2023` has is its own "version" with specific input files.

Each "version" will have its own folder in the `/data/chilled/version` directory in `message-ix-buildings`, and each "version" will have the following files (with the same names):

- arch_input_reg.csv
- arch_regions.csv
- par_var.csv
- runs.csv

So going from this:
![[Screenshot 2025-01-23 at 10.21.28 AM 1.png]]

To this:

![[Screenshot 2025-01-23 at 10.22.09 AM.png]]


## Introduction of the 
Config class

The Config object/class creates certain default settings for running CHILLED, removing the need (where possible) for running lines of code that sets parameters. This hopefully reduces inconsistencies in parameters.

### User settings

User-specific settings (paths mainly) are now specified as dictionaries (currently in .gitignore, so may need to share file internally). You can specify the user in the `Config()` object using `Config(user="MEAS")` for example. This should automatically update the corresponding paths for the user. This is helpful (necessary) because we have not only different paths but different path styles even because of different operating systems (Windows, Mac, Linux).

### Defaults and options for version, GCM, RCP, etc

The Config object already has some defaults selected for running a CHILLED scenario, but if you specify the selected version, GCM, RCP, etc., it will override the default. 

# Install message-ix-buildings

CHILLED is now housed in `message-ix-buildings`: 
https://github.com/iiasa/message-ix-buildings

After cloning the repository, in the root directory, run `pip install` in your virtual environment:

```
pip install --editable .
```

# How to run the model generally (up to the emulation point)

## Run preprocessing

## Run main CHILLED climate analysis (at the GCM-RCP level)

For the main functions, from creating VDD rasters to creating the ISO tables, these are all done per each GCM-RCP combination. 

The main steps you would need to do are:

1. Create a config object with your desired settings
2. Pass that config object through the following functions:
	1. `create_climate_variables_maps(Config, starttime)`
	2. `aggregate_urban_rural_files(Config)`
	3. `make_vdd_total_maps(Config)`
	4. `process_construction_shares(Config)`
	5. `process_floor_area_maps(Config)`
	6. `process_country_maps(Config)`
	7. `process_final_maps(Config)`
	8. `process_iso_tables(Config)`

## Run aggregation for all GCM-RCPs available for emulation/regression

After you've created the ISO tables for all the GCM-RCPs you're interested in, we can prep all the results for the emulation steps.

The general steps involved are:
1. Create a config object again with your desired settings.
2. Pass the config object through the following functions:
	1. `calculate_cumulative_carbon_emissions(Config)`
	2. `aggregate_ISO_tables_to_regions(Config)`
	3. `create_prereg_data(Config)`

## Run regression/emulation

The regression component is its own standalone function that has not been migrated to the Config structure yet or updated for heating files (as it currently references cooling variables/filenames). But the function can be called upon in `from message_ix_buildings.chilled.functions.regression import apply_regression`. The `apply_regression()` takes in the path where the pre-regression file created from `create_prereg_data()` is stored.

# How I run the workflow currently (on the UniCC cluster)

I have created some general wrapper Python scripts that takes inputs for the files and then passes those as parameters for the Config, then runs the related functions.

## Run preprocessing

In the `message_ix_buildings/chilled/` folder, run `run_preprocess.py`, with an argument for `--version` or `-version`. This is not dependent on GCM or RCP. But it does need to be run first. So, something like:

```bash
python run_preprocess.py --version "ALPS2024"
```

This will create the following files:

## Run main climate processing (the CHILLED component)

I have a file that runs the functions to create rasters and ISO tables for each GCM-RCP combination, with default Config settings. You can run the file via command line as such (from the `/message_ix_buildings/chilled/` folder):

```bash
python run_main.py -version "ALPS2024" -gcm "MRI-ESM2-0" -rcp "ssp370"
```


You can either modify the Config object directly in this part:

```python
def create_config(parsed_arguments):

cfg = Config(
	vstr=parsed_arguments.version,
	gcm=parsed_arguments.gcm,
	rcp=parsed_arguments.rcp,
)

return cfg
```

Or create a new file with slightly modified Config settings. For example, I have another file that is called `run_heat.py` which is basically the same but with cooling set to 0 and heating set to 1 in the Config object.

## Run aggregation (prepping for emulation)

Similarly, I also have a file for running the aggregation steps, when all GCM-RCP combinations have completed. This file is called `run_agg.py` and is run in a very similar fashion, except now you just need to specify the version:

```bash
python run_agg.py -version "ALPS2024"
```

## Run emulation

The emulation part is run similarly to the aggregation part and can only be run after the aggregation part above is complete.

```bash
python run_emulator.py -version "ALPS2024"
```

## Setting up job scripts for the SLURM workload manager

The nice thing about running jobs on a cluster is that you can send out multiple jobs/runs at once and wait for them to run. The downside though is that you could end up waiting in a queue for some time if other users are using the resources.

I have started some documentation on how to get started on the UniCC server: https://iiasa-energy-program-message-ix--279.com.readthedocs.build/projects/models/en/279/distrib/unicc.html
This is more specific to MESSAGEix but could be helpful. There are also lots of resources/documentation online for learning the SLURM system, for example here: https://slurm.schedmd.com/sbatch.html

Either way, I have a number of job scripts that I use to send out multiple CHILLED runs at once.

The preprocessing script can be run from the command line/terminal because it's not very memory or resource-intensive.

The `run_main.py` or `run_heat.py`, however, benefits from being run on a machine with more computing power. Here is a job script I have that sends off 4 RCPs for 1 GCM at once:

```bash
#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --mem=250G
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=36
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=mengm@iiasa.ac.at
#SBATCH -o /home/mengm/out/chilled/alps24_heat_%J.out
#SBATCH -e /home/mengm/err/chilled/alps24_heat_%J.err

source /opt/apps/lmod/8.7/init/bash
module load Python/3.11.5-GCCcore-13.2.0

echo "Activating environment..."
source /home/mengm/env/chilled/bin/activate

echo "Running python script..."
python /home/mengm/repo/message-ix-buildings/message_ix_buildings/chilled/run_heat.py -version "ALPS2024_heat" -gcm "UKESM1-0-LL" -rcp "baseline" &
python /home/mengm/repo/message-ix-buildings/message_ix_buildings/chilled/run_heat.py -version "ALPS2024_heat" -gcm "UKESM1-0-LL" -rcp "ssp126"  &
python /home/mengm/repo/message-ix-buildings/message_ix_buildings/chilled/run_heat.py -version "ALPS2024_heat" -gcm "UKESM1-0-LL" -rcp "ssp370"  &
python /home/mengm/repo/message-ix-buildings/message_ix_buildings/chilled/run_heat.py -version "ALPS2024_heat" -gcm "UKESM1-0-LL" -rcp "ssp585"  &
wait
```



# Needed work for code development

1. Move over regression file into Config-dependent function and add heating modifications
2. Implement a workflow-based system (in line with what Paul and others do for MESSAGEix). Input -> Output based, can be configurable to lots of runs, etc.
3. When reading in solar irradiation, force code to close dataset. 
4. Modify/update process_construction_shares() for building inputs
5. document where functions sit


