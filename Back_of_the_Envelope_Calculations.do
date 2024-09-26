*** bieocnomic model back of the envelope calcuations *** 
*** Main results ***
*** Molly Doruska ***
*** Date last modified: September 26, 2024 ***


clear all
set more off

*** install graphing packages if not yet installed ***
*ssc install blindschemes, replace
*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
*ssc install palettes, replace
*ssc install schemepack, replace

*** set working directory ***
cd "~\BioeconomicModelResults" 

*** append all data files together to make combined graphs ***
*** uses files after main results do file *** 
*** so run after main results do file *** 
use SLNoLaborLand25, clear 
append using SLNoLaborLand50
append using SLNoLaborLand75
append using SLNoLaborLand25_nh
append using SLNoLaborLand50_nh
append using SLNoLaborLand75_nh

*** calculate average household income difference at five years in *** 
keep if time == 5 

*** create variable of actual land value *** 
gen land_val = . 
replace land_val = 0.5 if land == 1 | land == 4 
replace land_val = 2 if land == 2 | land == 5 
replace land_val = 5.5 if land == 3 | land == 6 

*** calculate income difference for vegetation removal and no vegetation removal *** 
sort land_val land 
by land_val: gen obs = _n 
gen income_diff = med_income - med_income[_n-1] if obs == 2 
gen abs_income_diff = abs(income_diff)

*** calculate village level income difference for village size of 985 people and 10 people per household *** 

*** calculate number of households in each land grouping *** 
gen pop = . 
replace pop = 0.25*(985/10) if land_val == 0.5 
replace pop = 0.5*(985/10) if land_val == 2 
replace pop = 0.25*(985/10) if land_val == 5.5 

*** caluclate income gain from vegetation removal for households *** 
gen tot_income_gain = abs_income_diff*1000
gen tot_income_gain_usd = tot_income_gain/574.446 
gen village_income_gain = pop*tot_income_gain 
gen village_income_gain_usd = pop*tot_income_gain_usd

*** total village income gain amount *** 
collapse (sum) inc_usd = village_income_gain_usd (sum) inc = village_income_gain
*** these variables are the village level income gained from vegetation removal *** 

*** now scale to all villages within West Africa *** 
*** 43,320 communities *** 
gen wa_inc_usd = inc_usd*43320
gen wa_inc_fcfa = inc*43320 

format wa_inc_usd %15.0gc 
format wa_inc_fcfa %15.0gc 



