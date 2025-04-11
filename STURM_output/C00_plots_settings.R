
# Energy efficiency cohorts
rename_eneff <- c("adv" = "Renovated advanced",
  "avg" = "Average",
  "std" = "Renovated standard",
  "p1" = "Existing <1945",
  "p2" = "Existing 1946-1990",
  "p3" = "Existing 1991-2015",
  "p5" = "New standard"
)

order_eneff <- c("Existing <1945", "Existing 1946-1990", "Existing 1991-2015",
  "Renovated standard", "Renovated advanced", "New standard")


colors_efficiency <- c("Slum" = "grey30",
                "Existing <1945" = "coral4",
                "Existing 1946-1990" = "coral3",
                "Existing 1991-2015" = "coral2",
                "Average" = "grey30",
                "Renovated standard" = "khaki1",
                "Renovated advanced" = "goldenrod1",
                "New standard" = "palegreen3",
                "New advanced" = "palegreen4")

# Insulation levels

rename_insulation <- c(
  "0-0.5" = "0-0.5 kWh/(m2.K)",
  "0.5-1" = "0.5-1 kWh/(m2.K)",
  "1-1.5" = "1-1.5 kWh/(m2.K)",
  "1.5-2" = "1.5-2 kWh/(m2.K)",
  ">2" = "> 2 kWh/(m2.K)",
  "no_heating" = "No heating"
)

order_insulation <- c("No heating", "> 2 kWh/(m2.K)",
  "1.5-2 kWh/(m2.K)", "1-1.5 kWh/(m2.K)",
 "0.5-1 kWh/(m2.K)",  "0-0.5 kWh/(m2.K)")

colors_insulation <- c(
  "No heating" = "black",
  "> 2 kWh/(m2.K)" = "grey30",
  "1.5-2 kWh/(m2.K)" = "coral4",
  "1-1.5 kWh/(m2.K)" = "coral3",
  "0.5-1 kWh/(m2.K)" = "goldenrod1",
  "0-0.5 kWh/(m2.K)" = "darkgreen"
)

# Primary consumption level

rename_primary <- c(
  "0-50" = "Advanced performance",
  "50-100" = "Basic performance",
  "100-150" = "Bad performance",
  "150-200" = "Poor performance",
  ">200" = "No performance",
  "no_consumption" = "No consumption"
)

order_primary <- c("No consumption", "No performance", "Poor performance", "Bad performance",
  "Basic performance", "Advanced performance")

colors_primary <- c(
  "No consumption" = "black",
  "No performance" = "grey30",
  "Poor performance" = "coral4",
  "Bad performance" = "coral3",
  "Basic performance" = "goldenrod1",
  "Advanced performance" = "palegreen3"
)


# Heating fuels
rename_fuels <- c("biomass_solid" = "Biomass",
            "coal" = "Coal",
            "district_heat" = "District heating",
            "electricity" = "Direct electric",
            "heat_pump" = "Heat pump",
            "gas" = "Gas",
            "oil" = "Oil")

order_fuels <- c("Coal", "Oil", "Gas", "District heating",
  "Direct electric", "Heat pump", "Biomass")

colors_fuels <- c("No Heating" = "grey92",
  "Direct electric" = "slateblue",
  "Heat pump" = "slateblue4",
  "Gas" = "skyblue2",
  "District heating" = "lightgoldenrod3",
  "Oil" = "navajowhite4",
  "Coal" = "grey5",
  "Biomass" = "palegreen4")


rename_hh <- c("q1" = "Income 1st tertile",
            "q2" = "Income 2nd tertile",
            "q3" = "Income 3rd tertile")

order_hh <- c("q1", "q2", "q3")

rename_cost <- c(to_pay_renovation_cumsum = "Renovation",
            to_pay_heater_cumsum = "Heating",
            cost_energy = "Energy",
            total_cost_hh = "Total")

colors_cost <- c("Renovation" = "#ee6a6b",
            "Heating" = "#62c5c0",
            "Energy" = "#00589d",
            "Total" = "grey30")

order_cost <- c("Energy", "Renovation", "Heating", "Total")

# Countries

rename_countries <- c(
  "C-EEU-BGR" = "Bulgaria",
  "C-EEU-CZE" = "Czech Republic",
  "C-EEU-EST" = "Estonia",
  "C-EEU-HUN" = "Hungary",
  "C-EEU-LTU" = "Lithuania",
  "C-EEU-LVA" = "Latvia",
  "C-EEU-POL" = "Poland",
  "C-EEU-ROU" = "Romania",
  "C-EEU-SVK" = "Slovakia",
  "C-EEU-SVN" = "Slovenia",
  "C-WEU-AUT" = "Austria",
  "C-WEU-BEL" = "Belgium",
  "C-WEU-CYP" = "Cyprus",
  "C-WEU-HRV" = "Croatia",
  "C-WEU-MLT" = "Malta",
  "C-WEU-DEU" = "Germany",
  "C-WEU-DNK" = "Denmark",
  "C-WEU-ESP" = "Spain",
  "C-WEU-FIN" = "Finland",
  "C-WEU-FRA" = "France",
  "C-WEU-GBR" = "United Kingdom",
  "C-WEU-GRC" = "Greece",
  "C-WEU-IRL" = "Ireland",
  "C-WEU-ITA" = "Italy",
  "C-WEU-LUX" = "Luxembourg",
  "C-WEU-NLD" = "Netherlands",
  "C-WEU-PRT" = "Portugal",
  "C-WEU-SWE" = "Sweden",
  "EU" = "EU"
)

rename_countries_code <- c(
  "C-EEU-BGR" = "BGR",
  "C-EEU-CZE" = "CZE",
  "C-EEU-EST" = "EST",
  "C-EEU-HUN" = "HUN",
  "C-EEU-LTU" = "LTU",
  "C-EEU-LVA" = "LVA",
  "C-EEU-POL" = "POL",
  "C-EEU-ROU" = "ROU",
  "C-EEU-SVK" = "SVK",
  "C-EEU-SVN" = "SVN",
  "C-WEU-AUT" = "AUT",
  "C-WEU-BEL" = "BEL",
  "C-WEU-HRV" = "HRV",
  "C-WEU-CYP" = "CYP",
  "C-WEU-MLT" = "MLT",
  "C-WEU-DEU" = "DEU",
  "C-WEU-DNK" = "DNK",
  "C-WEU-ESP" = "ESP",
  "C-WEU-FIN" = "FIN",
  "C-WEU-FRA" = "FRA",
  "C-WEU-GBR" = "GBR",
  "C-WEU-GRC" = "GRC",
  "C-WEU-IRL" = "IRL",
  "C-WEU-ITA" = "ITA",
  "C-WEU-LUX" = "LUX",
  "C-WEU-NLD" = "NLD",
  "C-WEU-PRT" = "PRT",
  "C-WEU-SWE" = "SWE",
  "EU" = "EU"
)


colors_countries <- c(
  "Bulgaria" = "green",
  "Czech Republic" = "blue",
  "Croatia" = "red",
  "Estonia" = "#4891D9",   # Light blue
  "Hungary" = "red",
  "Lithuania" = "#FFC300",   # Yellow
  "Latvia" = "#AA0000",   # Maroon
  "Poland" = "#DC143C",   # Crimson
  "Romania" = "#002B5C",   # Dark blue
  "Slovakia" = "#0B6E4F",   # Dark green
  "Slovenia" = "#FFD700",   # Gold
  "Austria" = "#ED2939",   # Red
  "Belgium" = "#000000",   # Black
  "Cyprus" = "#FFD700",   # Gold
  "Germany" = "#000000",   # Black
  "Denmark" = "#C60C30",   # Red
  "Spain" = "#C60C30",   # Red
  "Finland" = "#003580",   # Dark blue
  "France" = "#0055A4",   # Blue
  "United Kingdom" = "#00247D",   # Dark blue
  "Greece" = "#0D5EAF",   # Blue
  "Ireland" = "#169B62",   # Green
  "Italy" = "#009246",   # Green
  "Luxembourg" = "#00A1DE",   # Light blue
  "Netherlands" = "#21468B",   # Dark blue
  "Portugal" = "#FF0000",   # Red
  "Sweden" = "#005CBF"    # Blue
)
order_countries <- c(
  "EU",
  "Austria",
  "Belgium",
  "Bulgaria",
  "Cyprus",
  "Croatia",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "United Kingdom"
)
