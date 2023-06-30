Notebook to run STURM Ouput.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(stringr)

print(paste("Working directory is:", getwd()))
#> [1] "Working directory is: /Users/lucas/PycharmProjects/message-ix-buildings/STURM_output"

# Loding figures setttings and functions
source("STURM_output/plot_settings.R")
#> Warning in file(filename, "r", encoding = encoding): cannot open file
#> 'STURM_output/plot_settings.R': No such file or directory
#> Error in file(filename, "r", encoding = encoding): cannot open the connection
```

``` r
file <- "report_STURM_EU_implementation_resid_region_bld_energy.csv"
file <- paste("STURM_output", file, sep = "/")

if (!file.exists(file)) {
  print("Check file name or directory!")
}
#> [1] "Check file name or directory!"

save_dir <- paste("STURM_output", "figures", sep = "/")
print(dir.exists(save_dir))
#> [1] FALSE
```

``` r
# Read output files
data <- read.csv(file)
#> Warning in file(file, "rt"): cannot open file
#> 'STURM_output/report_STURM_EU_implementation_resid_region_bld_energy.csv': No
#> such file or directory
#> Error in file(file, "rt"): cannot open the connection
print(summary(data))
#> Error in object[[i]]: object of type 'closure' is not subsettable
```

## Data formatting

``` r
data <- data %>%
    mutate(heat_TWh = heat_TJ / 3.6 / 10^3) %>%
    mutate(heat_hh_kWh = heat_TWh * 10**3 / stock_M)
#> Error in UseMethod("mutate"): no applicable method for 'mutate' applied to an object of class "function"
```

## Figures

``` r
plot_variable_evolution(data,
                        x_column = "year",
                        y_column = "heat_TWh",
                        group_column = "region_bld",
                        subplot = TRUE,
                        save_path = paste(save_dir, "heat_evolution.png", sep="/"))
#> Error in plot_variable_evolution(data, x_column = "year", y_column = "heat_TWh", : could not find function "plot_variable_evolution"
```

## Stock of households by space heating fuels

``` r
plot_stacked_area(data, "year", "stock_M", "fuel_heat",
                  y_label = "Stock households\nMillion buildings",
                  save_path = paste(save_dir, "stock_hh_fuel.png", sep="/"))
#> Error in plot_stacked_area(data, "year", "stock_M", "fuel_heat", y_label = "Stock households\nMillion buildings", : could not find function "plot_stacked_area"
```

## Final energy consumption for space heating in the Residential sector (TWh)

``` r
plot_stacked_area(data, "year", "heat_TWh", "fuel_heat",
                  save_path = "figures/heating_fuel.png",
                  y_label = "Final energy consumption for space heating \nResidential sector\nTWh",
                  font_size = 11)
#> Error in plot_stacked_area(data, "year", "heat_TWh", "fuel_heat", save_path = "figures/heating_fuel.png", : could not find function "plot_stacked_area"
```

<sup>Created on 2023-06-30 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>
