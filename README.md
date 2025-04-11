
# Message-ix-Buildings

[![DOI](https://zenodo.org/badge/812692384.svg)](https://doi.org/10.5281/zenodo.13919539)


## Disclaimer

**This repository is a fork of the [Message-ix Buildings repository](https://github.com/iiasa/message-ix-buildings). It contains the EU Implementation of the model used in the study _Meeting climate targets with realistic demand-side policies in the residential sector_ (currently under revision). The code in this repository will eventually be merged into the main repository.**

The following documentation provides a clear guide to running the code and generating the primary results. Please note that it does not cover all functionalities of the code in exhaustive detail.

---

## Overview

This repository hosts the **EU Implementation of Message-ix Buildings**, a bottom-up modeling framework for the European residential sector. The framework is both technologically explicit and behaviorally rich, relying on up-to-date data on building stock, heating system replacement dynamics, and energy renovations. It is designed to assess realistic mitigation policies in the residential sector.

The code is written in the **R** programming language.


---

## Installation

To get started with the repository, follow the steps below (should take less than 15 minutes to be able to install). Please, note that the model has been tested with R version 4.3.0.

### Step 1: Clone the Repository

Run the following command in your terminal to clone the repository to your local machine:

```bash
git clone https://github.com/lucas-vivier/message-ix-buildings-eu.git
```

Enter the repository directory:

```bash
cd message-ix-buildings-eu
```

### Step 2: Activate the EU Implementation Branch

Make sure to switch to the `eu_implementation_yssp` branch by running the following in your terminal:

```bash
git checkout eu_implementation_yssp
```

### Step 3: Install Required Libraries

To ensure all necessary dependencies are installed, run the following commands in your R environment:

```r
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("parallel")
install.packages("argparse")
install.packages("sf") # used for postprocessing spatial data
```

### Step 4: Launch the Model

By default, the configuration file `STURM_data/input_list_resid_EU.csv` is used to run the model. It contains the path to necessary inputs to launch the model.

To launch the Message-ix Buildings model, go to the `message-ix-buildings` directory and run the `main.R` script. The command line argument `-s` specifies the scenario file to be used. By default, the script will only run the "EU" scenario (see below for more details).

With the terminal:
```bash
Rscript main.R -s "all_scenarios.csv"
```

In your R environment:
```r
source("message-ix-buildings/main.R", encoding = "UTF-8")
```

Normal launch of one policy scenario between 2015 to 2050 with a 5 year time step should take less than one minute.

---

## Inputs

The model comes with a set of input data stored in the `STURM_data/input_csv/input_resid` directory, subdivided into different categories.

### Configuration File

A configuration file (`.csv` file) must be specified to run the model. This file contains all the path to the input data to be used. By default, the file `STURM_data/input_list_resid_EU.csv` contains the necessary inputs to launch the model and can be used as a template. The file includes:
- `name_parameter`: Defines the name of the variation within the model.
- `name_file`: Refers to the file to be used.
- `category`: Specifies the sub-folder within `STURM_data/input_csv/input_resid`.

The file is hardcoded into the `main.R` script for now, and variation of the configuration file is done with the scenarios file.

### Scenario Files

To run multiple scenarios, the model uses a scenario file (e.g., `scenarios_EU.csv`). This file defines the scenarios and allows for changing specific variables. By default, the model uses the `scenarios_EU.csv` file, but you can define your own scenario file and launch with the command line argument `-s`.

By default, the script will only run the "EU" scenario (see below for more details), but you can direcly modify the `runs` variable in `main.R`, which a vector that defines the scenarios to be executed. Another way is to use the command line argument `-a TRUE` to run all scenarios in the file.

Multiple scenarios are run in parallel by default.

---

## Running the Script

To run the script, use the following command format, specifying the appropriate parameters:

### Parameters

- `-c` or `--cores`: Specifies the number of CPU cores to use for parallel processing. If not provided, the script will use the system's default.
- `-s` or `--scenarios_file`: Specifies the path or name of the scenario file. If not provided, no specific scenarios will be executed.
- `-a` or `--all_scenarios`: A flag to run all scenarios in the provided file. The default is `FALSE`. Set this flag to `TRUE` to run all scenarios.

### Example Command

By default only the scenario called "EU" is run. 
```bash
Rscript main.R -s "scenarios_EU.csv"
```

To run all scenarios in the `scenarios_EU.csv` file, use the following command:

```bash
Rscript main.R -c 4 -s "scenarios_EU.csv" -a TRUE
```

---

## Outputs

The output files are stored in the `STURM_output/results` directory.

Several R scripts and notebooks are available to analyze the results and generate figures. Key scripts include:

### 1. `post_traitment.R`

This script generates a summary table comparing different scenarios and performs cost-benefit analysis. It processes the simulation results stored in `STURM_output/results/XX`, where `XX` is the folder containing the simulation results. The counterfactual scenario is specified using the `ref` variable, and the output results are saved in the `figures` folder. The output includes country-level and EU-level summary files.

This script enables to capture the main results of the model, without downloading the entire output (when the model is run on a remote server). 

**Parameters:**
- `-p` or `--path`: Specifies the directory in which to search for results and where to save the Figures.
- `-c` or `--counterfactual`: Specifies the name of the counterfactual scenario for comparison. If not provided, the first scenario is used as default.
- `-n` or `--names_scenarios`: Specifies the name of the scenario file.
- `-f` or `--figures`: A flag to display the figures (default is `TRUE`).

It combines key results and cost-benefits results within `results.csv` and `results_countries.csv` files that are used as input by some of other parsing output scripts.

```bash
Rscript STURM_output/post_traitment.R -p "2025-02-24_142018" -c "EU" -n "STURM_data/all_scenarios.csv" -f TRUE
```

### 2. `many_scenarios.R`

The script generates figures that are useful for comparing multiple scenarios. It uses the `results.csv` file generated by `post_traitment.R` and stores the output in the `figures` folder

### 2. `make_figures_scenarios.R`

This script generates comparison figures between different scenarios. The `run` variable specifies the directory where input files are located, and the `ref` variable defines the counterfactual scenario. Results are saved in the `STRUM_output` directory.

### 3. `make_figures_standalone.R`

Generates figures for a single scenario (specified in the script, default is `S1`). The script looks for the results in `STURM_output/results/report_agg_S1.csv`.

### 4. `make_policy_standalone.R`

This script generates specific policy assessments, such as calculating the marginal impact of each policy. It uses the `result.csv` file generated by `post_traitment.R` and stores the output in the `standalone` folder.


## System Specifications

The model was run and tested using the following system specifications:

- **Platform**: aarch64-apple-darwin20
- **Architecture**: aarch64
- **Operating System**: darwin20
- **System**: aarch64, darwin20
- **R Version**: R version 4.3.0 (2023-04-21) - "Already Tomorrow"
- **R SVN Revision**: 84292
- **R Language**: R 4.3.0 (Major: 4, Minor: 3.0)

These specifications represent the environment under which the model was successfully executed. It is recommended to ensure compatibility with these or similar system configurations when running the code. If your environment is not compatible with these specifications, please contact the author for assistance.


## Reproducing Outputs for _Meeting climate target with realistic demand-side policies in the residential sector_

First start by running the counterfactual scenario, it will default launch the "EU" scenario:
```bash
Rscript main.R -s "all_scenarios.csv"
```

Then, run for all policy scenarios use the following command:

```bash
Rscript main.R -s "all_scenarios.csv" -a TRUE
```
However, it'll take a long time to run all scenarios (more than a day). To run only a subset of scenarios, you can use the `scenarios_EU.csv` file. It'll take around 1 hour to run all scenarios.

```bash
Rscript main.R -s "scenarios_EU.csv" -a TRUE
```

Results will be stored in the `STURM_output/results/XX` folder, where `XX` is the scenario datetime.
