*** bioeconomic model results graphs ***
*** Main results ***
*** Molly Doruska ***
*** Date last modified: November 10, 2023 ***

clear all
set more off

*** install graphing packages if not yet installed ***
*ssc install blindschemes, replace
*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
*ssc install palettes, replace
*ssc install schemepack, replace

*** set working directory ***
cd "~\BioeconomicModelResults" 

*** import 25th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand25_pu300t.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 1

*** save dataset for appending later *** 
save SLNoLaborLand25, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_pu300t.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** save dataset for appending later *** 
save SLNoLaborLand50, replace 

*** import 75th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand75_pu300t.csv, clear 

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 3

*** save dataset for appending later *** 
save SLNoLaborLand75, replace 

*** import 25th percentile Saint Louis data *** 
import delimited using SaintLouisLaborLand25_nhnolab_pu300.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 4

*** save dataset for appending later *** 
save SLNoLaborLand25_nh, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisLaborLand50_nhnolab_pu300.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 5

*** save dataset for appending later *** 
save SLNoLaborLand50_nh, replace 

*** import 75th percentile Saint Louis data *** 
import delimited using SaintLouisLaborLand75_nhnolab_pu300.csv, clear 

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
sort time
by time: keep if _n == 1

** add land endowment variable ***
gen land = 6

*** save dataset for appending later *** 
save SLNoLaborLand75_nh, replace 

*** append all data files together to make combined graphs ***
use SLNoLaborLand25, clear 
append using SLNoLaborLand50
append using SLNoLaborLand75
append using SLNoLaborLand25_nh
append using SLNoLaborLand50_nh
append using SLNoLaborLand75_nh

*** create combined results graphs ***
twoway (rarea lower_vegload upper_vegload time if land == 1, color(gs14%50)) (rarea lower_vegload upper_vegload time if land == 2, color(gs14%50)) (rarea lower_vegload upper_vegload time if land == 3, color(gs14%50)) (rarea lower_vegload upper_vegload time if land == 4, color(gs13%50)) (rarea lower_vegload upper_vegload time if land == 5, color(gs13%50)) (rarea lower_vegload upper_vegload time if land == 6, color(gs13%50)) (line med_vegload time if land == 1, lcolor(black)) (line med_vegload time if land == 2, lcolor(cyan)) (line med_vegload time if land == 3, lcolor(purple) lpattern(solid)) (line med_vegload time if land == 4, lcolor(black) lpattern(dash)) (line med_vegload time if land == 5, lcolor(cyan) lpattern(dash)) (line med_vegload time if land == 6, lcolor(purple) lpattern(dash)), xtitle("Year") ytitle("Vegetation Load (metric tons)") title("A", position(11)) xlabel(1(1)20) ylabel(0(3)30) legend(off) scheme(cleanplots) name(fig2_a, replace)

twoway (rarea lower_hhinfection upper_hhinfection time if land == 1, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if land == 2, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if land == 3, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if land == 4, color(gs13%50)) (rarea lower_hhinfection upper_hhinfection time if land == 5, color(gs13%50)) (rarea lower_hhinfection upper_hhinfection time if land == 6, color(gs13%50)) (line med_hhinfection time if land == 1, lcolor(black)) (line med_hhinfection time if land == 2, lcolor(cyan)) (line med_hhinfection time if land == 3, lcolor(purple) lpattern(solid)) (line med_hhinfection time if land == 4, lcolor(black) lpattern(dash)) (line med_hhinfection time if land == 5, lcolor(cyan) lpattern(dash)) (line med_hhinfection time if land == 6, lcolor(purple) lpattern(dash)), xtitle("Year") ytitle("Household Infection Rate (%)") title("B", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(off) scheme(cleanplots) name(fig2_b, replace)

twoway (rarea lower_laboravailabe upper_laboravailabe time if land == 1, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if land == 2, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if land == 3, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if land == 4, color(gs13%50)) (rarea lower_laboravailabe upper_laboravailabe time if land == 5, color(gs13%50)) (rarea lower_laboravailabe upper_laboravailabe time if land == 6, color(gs13%50)) (line med_laboravailabe time if land == 1, lcolor(black)) (line med_laboravailabe time if land == 2, lcolor(cyan)) (line med_laboravailabe time if land == 3, lcolor(purple) lpattern(soild)) (line med_laboravailabe time if land == 4, lcolor(black) lpattern(dash)) (line med_laboravailabe time if land == 5, lcolor(cyan) lpattern(dash)) (line med_laboravailabe time if land == 6, lcolor(purple) lpattern(dash)), xtitle("Year") ytitle("Labor Availability (Household Members)") title("C", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(fig2_c, replace)

twoway (rarea lower_fert upper_fert time if land == 1, color(gs14%50)) (rarea lower_fert upper_fert time if land == 2, color(gs14%50)) (rarea lower_fert upper_fert time if land == 3, color(gs14%50)) (rarea lower_fert upper_fert time if land == 4, color(gs13%50)) (rarea lower_fert upper_fert time if land == 5, color(gs13%50)) (rarea lower_fert upper_fert time if land == 6, color(gs13%50)) (line med_fert time if land == 1, lcolor(black)) (line med_fert time if land == 2, lcolor(cyan)) (line med_fert time if land == 3, lcolor(purple) lpattern(solid)) (line med_fert time if land == 4, lcolor(black) lpattern(dash)) (line med_fert time if land == 5, lcolor(cyan) lpattern(dash)) (line med_fert time if land == 6, lcolor(purple) lpattern(dash)), xtitle("Year") ytitle("Fertilizer (kg per hectare)") title("D", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(fig2_d, replace)

twoway (rarea lower_income upper_income time if land == 1, color(gs14%50)) (rarea lower_income upper_income time if land == 2, color(gs14%50)) (rarea lower_income upper_income time if land == 3, color(gs14%50)) (rarea lower_income upper_income time if land == 4, color(gs13%50)) (rarea lower_income upper_income time if land == 5, color(gs13%50)) (rarea lower_income upper_income time if land == 6, color(gs13%50)) (line med_income time if land == 1, lcolor(black)) (line med_income time if land == 2, lcolor(cyan)) (line med_income time if land == 3, lcolor(purple) lpattern(solid)) (line med_income time if land == 4, lcolor(black) lpattern(dash)) (line med_income time if land == 5, lcolor(cyan) lpattern(dash)) (line med_income time if land == 6, lcolor(purple) lpattern(dash)), xtitle("Year") ytitle("Income (1,000 FCFA)") title("E", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(order(7 "Land = 0.5 Hectares" 8 "Land = 2 Hectares" 9 "Land = 5.5 Hectares" 10 "No Vegetation Harvest")) scheme(cleanplots) name(fig2_e, replace)

grc1leg fig2_a fig2_b fig2_c fig2_d fig2_e, cols(2) legendfrom(fig2_e) position(5) ring(0) scheme(cleanplots) iscale(0.5) 
graph export Figure_1.svg, replace 

*** sensitivity analysis results graphs ***
*** fertilizer feedback ***

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_nofert_pu300.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** save dataset for appending later *** 
save SLNoLaborLand50_nofert, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_rho005.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add rho cateogry variable ***
gen rho = 2

*** save dataset for appending later *** 
save SLNoLaborLand50_rho005, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_rho02.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add rho variable ***
gen rho = 4

*** save dataset for appending later *** 
save SLNoLaborLand50_rho02, replace 

*** merge all fertilizer feedback results together ***
use SLNoLaborLand50, clear

*** add rho variable ***
gen rho = 3 

save SLNoLaborLand50_rho01, replace 

use SLNoLaborLand50_nofert, clear

*** add rho variable ***
gen rho = 1

append using SLNoLaborLand50_rho005 
append using SLNoLaborLand50_rho01
append using SLNoLaborLand50_rho02

*** create combined results graphs ***
twoway (rarea lower_vegload upper_vegload time if rho == 1, color(gs14%50)) (rarea lower_vegload upper_vegload time if rho == 2, color(gs14%50)) (rarea lower_vegload upper_vegload time if rho == 3, color(gs14%50)) (rarea lower_vegload upper_vegload time if rho == 4, color(gs14%50)) (line med_vegload time if rho == 1) (line med_vegload time if rho == 2) (line med_vegload time if rho == 3) (line med_vegload time if rho == 4), xtitle("Year") ytitle("Vegetation Load (metric tons)") title("A", position(11)) xlabel(1(1)20) ylabel(0(3)30) legend(off) scheme(cleanplots) name(fig4_a, replace)

twoway (rarea lower_hhinfection upper_hhinfection time if rho == 1, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if rho == 2, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if rho == 3, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if rho == 4, color(gs14%50)) (line med_hhinfection time if rho == 1)  (line med_hhinfection time if rho == 2)  (line med_hhinfection time if rho == 3) (line med_hhinfection time if rho == 4), xtitle("Year") ytitle("Household Infection Rate (%)") title("B", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(off) scheme(cleanplots) name(fig4_b, replace)

twoway (rarea lower_laboravailabe upper_laboravailabe time if rho == 1, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if rho == 2, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if rho == 3, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if rho == 4, color(gs14%50)) (line med_laboravailabe time if rho == 1) (line med_laboravailabe time if rho == 2)  (line med_laboravailabe time if rho == 3) (line med_laboravailabe time if rho == 4), xtitle("Year") ytitle("Labor Availability (Household Members)") title("C", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(fig4_c, replace )

twoway (rarea lower_fert upper_fert time if rho == 1, color(gs14%50)) (rarea lower_fert upper_fert time if rho == 2, color(gs14%50)) (rarea lower_fert upper_fert time if rho == 3, color(gs14%50)) (rarea lower_fert upper_fert time if rho == 4, color(gs14%50)) (line med_fert time if rho == 1)  (line med_fert time if rho == 2)  (line med_fert time if rho == 3) (line med_fert time if rho == 4), xtitle("Year") ytitle("Fertilizer (kg per hectare)")  title("D", position(11)) xlabel(1(1)20) ylabel(0(1)10) legend(off) scheme(cleanplots) name(fig4_d, replace)

twoway (rarea lower_income upper_income time if rho == 1, color(gs14%50)) (rarea lower_income upper_income time if rho == 2, color(gs14%50)) (rarea lower_income upper_income time if rho == 3, color(gs14%50)) (rarea lower_income upper_income time if rho == 4, color(gs14%50)) (line med_income time if rho == 1)  (line med_income time if rho == 2)  (line med_income time if rho == 3) (line med_income time if rho == 4), xtitle("Year") ytitle("Income (1,000 FCFA)") title("E", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(order(5 "Rho = 0.0" 6 "Rho = 0.005" 7 "Rho = 0.01" 8 "Rho = 0.02")) scheme(cleanplots) name(fig4_e, replace)

grc1leg fig4_a fig4_b fig4_c fig4_d fig4_e, cols(2) legendfrom(fig4_e) position(5) ring(0) scheme(cleanplots) iscale(0.5) 
graph export Figure_4.svg, replace 

*** vegetation growth rate ***
*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_r025.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add growth rate category variable ***
gen r = 1

*** save dataset for appending later *** 
save SLNoLaborLand50_r025, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_r075.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add growth rate catoegry variable ***
gen r = 3

*** save dataset for appending later *** 
save SLNoLaborLand50_r075, replace 

*** merge all fertilizer feedback results together ***
use SLNoLaborLand50, clear

*** add growth rate variable ***
gen r = 2 

append using SLNoLaborLand50_r025 
append using SLNoLaborLand50_r075

*** create combined results graphs ***
twoway (rarea lower_vegload upper_vegload time if r == 1, color(gs14%50)) (rarea lower_vegload upper_vegload time if r == 2, color(gs14%50)) (rarea lower_vegload upper_vegload time if r == 3, color(gs14%50)) (line med_vegload time if r == 1) (line med_vegload time if r == 2) (line med_vegload time if r == 3), xtitle("Year") ytitle("Vegetation Load (metric tons)") title("A", position(11)) xlabel(1(1)20) ylabel(0(3)30) legend(off) scheme(cleanplots) name(fig5_a, replace)

twoway (rarea lower_hhinfection upper_hhinfection time if r == 1, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if r == 2, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if r == 3, color(gs14%50)) (line med_hhinfection time if r == 1)  (line med_hhinfection time if r == 2)  (line med_hhinfection time if r == 3), xtitle("Year") ytitle("Household Infection Rate (%)") title("B", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(off) scheme(cleanplots) name(fig5_b, replace)

twoway (rarea lower_laboravailabe upper_laboravailabe time if r == 1, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if r == 2, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if r == 3, color(gs14%50)) (line med_laboravailabe time if r == 1) (line med_laboravailabe time if r == 2)  (line med_laboravailabe time if r == 3), xtitle("Time") ytitle("Labor Availability (Household Members)") title("C", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(fig5_c, replace )

twoway (rarea lower_fert upper_fert time if r == 1, color(gs14%50)) (rarea lower_fert upper_fert time if r == 2, color(gs14%50)) (rarea lower_fert upper_fert time if r == 3, color(gs14%50)) (line med_fert time if r == 1)  (line med_fert time if r == 2)  (line med_fert time if r == 3), xtitle("Year") ytitle("Fertilizer (kg per hectare)") title("D", position(11)) xlabel(1(1)20) ylabel(0(1)10) legend(off) scheme(cleanplots) name(fig5_d, replace) 

twoway (rarea lower_income upper_income time if r == 1, color(gs14%50)) (rarea lower_income upper_income time if r == 2, color(gs14%50)) (rarea lower_income upper_income time if r == 3, color(gs14%50)) (line med_income time if r == 1)  (line med_income time if r == 2)  (line med_income time if r == 3), xtitle("Year") ytitle("Income (1,000 FCFA)") title("E", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(order(4 "r = 0.025" 5 "r = 0.05" 6 "r = 0.075")) scheme(cleanplots) name(fig5_e, replace)

grc1leg fig5_a fig5_b fig5_c fig5_d fig5_e, cols(2) legendfrom(fig5_e) position(5) ring(0) scheme(cleanplots) iscale(0.5) 
graph export Figure_5.svg, replace 

*** recolonization rate ***
*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_n0005.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add recolonization rate variable ***
gen n0 = 1 

*** save dataset for appending later *** 
save SLNoLaborLand50_n0005, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_n002.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add recolonization rate variable ***
gen n0 = 3

*** save dataset for appending later *** 
save SLNoLaborLand50_n002, replace 

*** merge all fertilizer feedback results together ***
use SLNoLaborLand50, clear

*** add recolonization rate variable ***
gen n0 = 2 

append using SLNoLaborLand50_n0005 
append using SLNoLaborLand50_n002

*** create combined results graphs ***
twoway (rarea lower_vegload upper_vegload time if n0 == 1, color(gs14%50)) (rarea lower_vegload upper_vegload time if n0 == 2, color(gs14%50)) (rarea lower_vegload upper_vegload time if n0 == 3, color(gs14%50)) (line med_vegload time if n0 == 1) (line med_vegload time if n0 == 2) (line med_vegload time if n0 == 3), xtitle("Year") ytitle("Vegetation Load (metric tons)") title("A", position(11)) xlabel(1(1)20) ylabel(0(3)30) legend(off) scheme(cleanplots) name(figs1_a, replace )

twoway (rarea lower_hhinfection upper_hhinfection time if n0 == 1, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if n0 == 2, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if n0 == 3, color(gs14%50)) (line med_hhinfection time if n0 == 1)  (line med_hhinfection time if n0 == 2)  (line med_hhinfection time if n0 == 3), xtitle("Year") ytitle("Household Infection Rate (%)") title("B", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(off) scheme(cleanplots) name(figs1_b, replace)

twoway (rarea lower_laboravailabe upper_laboravailabe time if n0 == 1, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if n0 == 2, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if n0 == 3, color(gs14%50)) (line med_laboravailabe time if n0 == 1) (line med_laboravailabe time if n0 == 2)  (line med_laboravailabe time if n0 == 3), xtitle("Year") ytitle("Labor Availability (Household Members)") title("C", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(figs1_c, replace)

twoway (rarea lower_fert upper_fert time if n0 == 1, color(gs14%50)) (rarea lower_fert upper_fert time if n0 == 2, color(gs14%50)) (rarea lower_fert upper_fert time if n0 == 3, color(gs14%50)) (line med_fert time if n0 == 1)  (line med_fert time if n0 == 2)  (line med_fert time if n0 == 3), xtitle("Year") ytitle("Fertilizer (kg per hectare)") title("D", position(11)) xlabel(1(1)20) ylabel(0(1)10) legend(off) scheme(cleanplots) name(figs1_d, replace)

twoway (rarea lower_income upper_income time if n0 == 1, color(gs14%50)) (rarea lower_income upper_income time if n0 == 2, color(gs14%50)) (rarea lower_income upper_income time if n0 == 3, color(gs14%50)) (line med_income time if n0 == 1)  (line med_income time if n0 == 2) (line med_income time if n0 == 3), xtitle("Year") ytitle("Income (1,000 FCFA)") title("E", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(order(4 "n0 = 0.005" 5 "n0 = 0.01" 6 "n0 = 0.02")) scheme(cleanplots) name(figs1_e, replace)

grc1leg figs1_a figs1_b figs1_c figs1_d figs1_e, cols(2) legendfrom(figs1_e) position(5) ring(0) scheme(cleanplots) iscale(0.5) 
graph export Figure_S1.svg, replace 

*** fertilizer price ***
*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_pu200.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add fertilizer price variable ***
gen pu = 200

*** save dataset for appending later *** 
save SLNoLaborLand50_pu200, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_pu500.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add fertilizer price variable ***
gen pu = 500

*** save dataset for appending later *** 
save SLNoLaborLand50_pu500, replace 

*** merge all fertilizer feedback results together ***
use SLNoLaborLand50, clear

*** add fertilzier price variable ***
gen pu = 300 

append using SLNoLaborLand50_pu200 
append using SLNoLaborLand50_pu500

*** create combined results graphs ***
twoway (rarea lower_vegload upper_vegload time if pu == 200, color(gs14%50)) (rarea lower_vegload upper_vegload time if pu == 300, color(gs14%50)) (rarea lower_vegload upper_vegload time if pu == 500, color(gs14%50)) (line med_vegload time if pu == 200) (line med_vegload time if pu == 300) (line med_vegload time if pu == 500), xtitle("Year") ytitle("Vegetation Load (metric tons)") title("A", position(11)) xlabel(1(1)20) ylabel(0(3)30) legend(off) scheme(cleanplots) name(figs2_a, replace )

twoway (rarea lower_hhinfection upper_hhinfection time if pu == 200, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if pu == 300, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if pu == 500, color(gs14%50)) (line med_hhinfection time if pu == 200)  (line med_hhinfection time if pu == 300)  (line med_hhinfection time if pu == 500), xtitle("Year") ytitle("Household Infection Rate (%)") title("B", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(off) scheme(cleanplots) name(figs2_b, replace)

twoway (rarea lower_laboravailabe upper_laboravailabe time if pu == 200, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if pu == 300, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if pu == 500, color(gs14%50)) (line med_laboravailabe time if pu == 200) (line med_laboravailabe time if pu == 300)  (line med_laboravailabe time if pu == 500), xtitle("Year") ytitle("Labor Availability (Household Members)") title("C", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(figs2_c, replace)

twoway (rarea lower_fert upper_fert time if pu == 200, color(gs14%50)) (rarea lower_fert upper_fert time if pu == 300, color(gs14%50)) (rarea lower_fert upper_fert time if pu == 500, color(gs14%50)) (line med_fert time if pu == 200)  (line med_fert time if pu == 300)  (line med_fert time if pu == 500), xtitle("Year") ytitle("Fertilizer (kg per hectare)") title("D", position(11)) xlabel(1(1)20) ylabel(0(1)10) legend(off) scheme(cleanplots) name(figs2_d, replace)

twoway (rarea lower_income upper_income time if pu == 200, color(gs14%50)) (rarea lower_income upper_income time if pu == 300, color(gs14%50)) (rarea lower_income upper_income time if pu == 500, color(gs14%50)) (line med_income time if pu == 200)  (line med_income time if pu == 300)  (line med_income time if pu == 500), xtitle("Year") ytitle("Income (1,000 FCFA)") title("E", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(order(4 "pu = 200" 5 "pu = 300" 6 "pu = 500")) scheme(cleanplots) name(figs2_e, replace)

grc1leg figs2_a figs2_b figs2_c figs2_d figs2_e, cols(2) legendfrom(figs2_e) position(5) ring(0) scheme(cleanplots) iscale(0.5) 
graph export Figure_S2.svg, replace 

*** price of the household good ***
*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_p300.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add price of the household good variable ***
gen pg = 300 

*** save dataset for appending later *** 
save SLNoLaborLand50_pg300, replace 

*** import 50th percentile Saint Louis data *** 
import delimited using SaintLouisNoLaborLand50_p700.csv, clear

*** rename variables ***
rename v1 time
rename v2 vegload
rename v3 infected
rename v4 susceptible
rename v5 miracidia
rename v6 sussnails
rename v7 infsnails
rename v8 cercariae
rename v9 preinf
rename v10 presus
rename v11 foodconsumption
rename v12 hhgoodconsumption
rename v13 healthstatus
rename v14 foodlabor
rename v15 vegprod
rename v16 fert
rename v17 veglabor
rename v18 foodprod
rename v19 leisure
rename v20 hiredfarm
rename v21 hiredveg
rename v22 marketlabor
rename v23 foodlabortotal
rename v24 veglabortotal

*** drop first row of zeros ***
drop if time == 0

*** create income variable ***
gen income = 0.29*foodprod + 0*marketlabor 
gen hhinfection = (infected / (infected + susceptible))*100
gen villageinfection = preinf / (preinf+presus)

*** create labor share variable ***
gen labortotal = foodlabor + veglabor + leisure + marketlabor
gen foodshare = foodlabor / labortotal
gen vegshare = veglabor / labortotal
gen leisureshare = leisure / labortotal
gen marketshare = marketlabor / labortotal

*** create labor availability variable ***
gen laboravailabe = susceptible + 0.5*infected

*** create list of variables ***
local outcomes "vegload infected susceptible miracidia sussnails infsnails cercariae preinf presus foodconsumption hhgoodconsumption healthstatus foodlabor vegprod fert veglabor foodprod income hhinfection villageinfection leisure hiredfarm hiredveg marketlabor laboravailabe foodshare vegshare leisureshare marketshare"

*** create average, standard deviation, 5th and 95th percentile by time period ***
foreach i in `outcomes' {
    egen avg_`i' = mean(`i'), by(time)
	egen sd_`i' = sd(`i'), by(time)
	egen med_`i' = median(`i'), by(time)
	egen upper_`i' = pctile(`i'), p(95) by(time)
	egen lower_`i' = pctile(`i'), p(5) by(time)
}

*** drop additional repitions of summary variables ***
gen row = _n 
drop if row > 20

*** add land endowment variable ***
gen land = 2

*** add price of household good variable ***
gen pg = 700

*** save dataset for appending later *** 
save SLNoLaborLand50_pg700, replace 

*** merge all fertilizer feedback results together ***
use SLNoLaborLand50, clear

*** add price of household good variable ***
gen pg = 500 

append using SLNoLaborLand50_pg300 
append using SLNoLaborLand50_pg700

*** create combined results graphs ***
twoway (rarea lower_vegload upper_vegload time if pg == 300, color(gs14%50)) (rarea lower_vegload upper_vegload time if pg == 500, color(gs14%50)) (rarea lower_vegload upper_vegload time if pg == 700, color(gs14%50)) (line med_vegload time if pg == 300) (line med_vegload time if pg == 500) (line med_vegload time if pg == 700), xtitle("Year") ytitle("Vegetation Load (metric tons)") title("A", position(11)) xlabel(1(1)20) ylabel(0(3)30) legend(off) scheme(cleanplots) name(figs3_a, replace)

twoway (rarea lower_hhinfection upper_hhinfection time if pg == 300, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if pg == 500, color(gs14%50)) (rarea lower_hhinfection upper_hhinfection time if pg == 700, color(gs14%50)) (line med_hhinfection time if pg == 300)  (line med_hhinfection time if pg == 500)  (line med_hhinfection time if pg == 700), xtitle("Year") ytitle("Household Infection Rate (%)") title("B", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(off) scheme(cleanplots) name(figs3_b, replace)

twoway (rarea lower_laboravailabe upper_laboravailabe time if pg == 300, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if pg == 500, color(gs14%50)) (rarea lower_laboravailabe upper_laboravailabe time if pg == 700, color(gs14%50)) (line med_laboravailabe time if pg == 300) (line med_laboravailabe time if pg == 500)  (line med_laboravailabe time if pg == 700), xtitle("Year") ytitle("Labor Availability (Household Members)") title("C", position(11)) xlabel(1(1)20) ylabel(0(2)10) legend(off) scheme(cleanplots) name(figs3_c, replace)

twoway (rarea lower_fert upper_fert time if pg == 300, color(gs14%50)) (rarea lower_fert upper_fert time if pg == 500, color(gs14%50)) (rarea lower_fert upper_fert time if pg == 700, color(gs14%50)) (line med_fert time if pg == 300)  (line med_fert time if pg == 500)  (line med_fert time if pg == 700), xtitle("Year") ytitle("Fertilizer (kg per hectare)") title("D", position(11)) xlabel(1(1)20) ylabel(0(1)10) legend(off) scheme(cleanplots) name(figs3_d, replace) 

twoway (rarea lower_income upper_income time if pg == 300, color(gs14%50)) (rarea lower_income upper_income time if pg == 500, color(gs14%50)) (rarea lower_income upper_income time if pg == 700, color(gs14%50)) (line med_income time if pg == 300)  (line med_income time if pg == 500)  (line med_income time if pg == 700), xtitle("Year") ytitle("Income (1,000 FCFA)") title("E", position(11)) xlabel(1(1)20) ylabel(0(10)100) legend(order(4 "pg = 300" 5 "pg = 500" 6 "pg = 700")) scheme(cleanplots) name(figs3_e, replace)

grc1leg figs3_a figs3_b figs3_c figs3_d figs3_e, cols(2) legendfrom(figs3_e) position(5) ring(0) scheme(cleanplots) iscale(0.5) 
graph export Figure_S3.svg, replace 