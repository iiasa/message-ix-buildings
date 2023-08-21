
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


# Heating fuels
rename_fuels <- c("biomass_solid" = "Biomass",
            "coal" = "Coal",
            "district_heat" = "District heating",
            "electricity" = "Direct electric",
            "heat_pump" = "Heat pump",
            "gas" = "Gas",
            "oil" = "Oil")

order_fuels <- c("Coal", "Oil", "Gas", "District heating", "Direct electric", "Heat pump", "Biomass")

colors_fuels <- c("No Heating" = "grey92",
  "Direct electric" = "slateblue",
  "Heat pump" = "slateblue4",
  "Gas" = "skyblue2",
  "District heating" = "lightgoldenrod3",
  "Oil" = "navajowhite4",
  "Coal" = "grey5",
  "Biomass" = "palegreen4")

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

colors_countries <- c(
  "Bulgaria" = "green",
  "Czech Republic" = "blue",
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
