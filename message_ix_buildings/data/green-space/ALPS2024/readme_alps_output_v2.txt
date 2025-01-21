
####
## Street green space, urban temperature, and energy consumption 
## Contribution to ALPS adaptation project
## Giacomo Falchetta, Steffen Lohrey
## 17 January 2025

# Output files:

* outer.csv: the share of pixels covered by each LCZs by city for a pool of 142 cities, including the XY coordinates of the centroid of the city and Koppen-Geiger climate zone of belonging. 

Legend of LCZs: 
	LCZ 1		|	Compact highrise	
	LCZ 2		|	Compact midrise		
	LCZ 3		|	Compact lowrise	
	LCZ 4		|	Open highrise		
	LCZ 5		|	Open midrise		
	LCZ 6		|	Open lowrise		
	LCZ 7		|	Lightweight low-rise	
	LCZ 8		|	Large lowrise	
	LCZ 9		|	Sparsely built		
	LCZ 10		|	Heavy Industry		
	LCZ 11 (A)	|	Dense trees		
	LCZ 12 (B)	|	Scattered trees		
	LCZ 13 (C)	|	Bush, scrub		
	LCZ 14 (D)	|	Low plants		
	LCZ 15 (E)	|	Bare rock or paved	
	LCZ 16 (F)	|	Bare soil or sand	
	LCZ 17 (G)	|	Water	

Legend of columns:
# UC_NM_MN: name of the city
# x: longitude coordinate of the centroid of the city
# y: latitude coordinate of the centroid of the city
# lcz_frac_x: percentage of grid cells in city covered b local climate zone x
# CTR_MN_ISO: country of belonging of the city
# GRGN_L2: global region of belonging of the city

####
####
####

* outer_2.csv: the coefficients of the impact of GVI (our street green space metric) on temperature for each city, local climate zone (within the city) and month of the year (1 to 12). Each coefficient describes by how much (in percentage terms) would the average daily temperature in month m change if the street green space in the city (as measured by the SGS/GVI indicator) would increase by one unit. 

Legend of columns:
# UC_NM_MN: name of the city
# lcz: local climate zone
# month: month of the year
# coef: value of the coefficient of impact
# CTR_MN_ISO: country of belonging of the city
# GRGN_L2: global region of belonging of the city

Usage notes: for example a coefficient of 0.3 for Vienna in July in LCZ 3 means that if GVI increased from 15 to 16, in areas of Vienna covered by LCZ 3 the average daily temperature of July would drop by 0.3 %.

####
####
####

* outer:3.csv:  the historical values and future projections to 2050 (with a five-year time step) of city-level average GVI in the future for three/four scenarios: 

(i)  “out_b_mean_s” - this is the GVI value (the unit in which SGS is measured) for each LCZ in each city in each year of the historical period (2016-2023)
(ii) “GVI_ALPS” - For years <= 2023 it equals out_b_mean_s for years >= 2023 it equals (GVI_proja_upr + GVI_proj_lwr)/2
(iii) “GVI_proja_upr” - This is the upper boundary of the “a” scenario. It assumes a 25% growth of SGS starting from the year 2023 to 2050, starting from the 75% percentile of the observed SGS for the 2016 - 2023 period.
(iv) “GVI_proja_lwr” - This is the lower boundary of the “a” scenario. It assumes a 25% decrease of SGS starting from the year 2023 to 2050, starting from the 25% percentile of the observed SGS for the 2016 - 2023 period.
(v) “GVI_projb_upr” - This is the upper boundary of the “b” scenario. It assumes a 50% growth of SGS starting from the year 2023 to 2050, starting from the 75% percentile of the observed SGS for the 2016 - 2023 period.
(vi) “GVI_projb_lwr” - This is the lower boundary of the “b” scenario. It assumes a 50% decrease of SGS starting from the year 2023 to 2050, starting from the 25% percentile of the observed SGS for the 2016 - 2023 period.

Legend of columns:
# UC_NM_MN: name of the city
# scen_SGS: name of the scenario of SGS projection
# lcz: local climate zone
# year: projection year (2020-2050)
# SGS: value of the city-level SGS in year t
# CTR_MN_ISO: country of belonging of the city
# GRGN_L2: global region of belonging of the city

Usage notes: Note that to calculate the temperature reduction in a given scenario, one needs first to subtract the current value of GVI (e.g. GVI_2020_Vienna = 20) from future value of GVI (e.g. GVI_2050_33scen_Vienna = 25). The delta (=5) needs to be multiplied by the coefficients for the city of Vienna of outer_2 to obtain the monthly, LCZ-specific average daily temperature changes. For instance, if GVI evolves along with this scenario, in areas of Vienna covered by LCZ 3 the average daily temperature of July would drop by 1.5% compared to a scenario of costant (current) GVI.

####
####
####

* outer:4.csv:  output telling how much the current (2016-2023 average value in each LCZ of each city) is contributing to decreasing the temperature compared to a counterfactual where GVI is at 0.

Legend of columns:
# UC_NM_MN: name of the city
# lcz: local climate zone
# month: month of the year
# CTR_MN_ISO: country of belonging of the city
# GRGN_L2: global region of belonging of the city
# delta: % current difference in monthly, LCZ-specific average temperature compared to a counterfactual where GVI is at 0.

Usage notes: For example, if delta is -5 for Bruxelles in July, it means that the observed temperatures are 5% lower than in a counterfactual situation where there were not street trees. Hence, assuming T_july_burxelles = 25 deg C, then 25 * 0.05 = 1.25 deg C lower temperatures.


