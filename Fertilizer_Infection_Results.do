*** bioeconomic model results graphs ***
*** Infection-Fertilizer Results ***
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

*** import 10% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr01.csv, clear

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

*** add infection rate variable ***
gen start_infection = 1

*** save dataset for appending later *** 
save SLNoLaborLand50_pr01, replace 

*** import 20% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr02.csv, clear

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

*** add infection rate variable ***
gen start_infection = 2

*** save dataset for appending later *** 
save SLNoLaborLand50_pr02, replace 

*** import 30% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr03.csv, clear

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

*** add infection rate variable ***
gen start_infection = 3

*** save dataset for appending later *** 
save SLNoLaborLand50_pr03, replace 

*** import 40% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr04.csv, clear

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

*** add infection rate variable ***
gen start_infection = 4

*** save dataset for appending later *** 
save SLNoLaborLand50_pr04, replace 

*** import 50% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr05.csv, clear

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

*** add infection rate variable ***
gen start_infection = 5

*** save dataset for appending later *** 
save SLNoLaborLand50_pr05, replace 

*** import 60% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr06.csv, clear

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

*** add infection rate variable ***
gen start_infection = 6

*** save dataset for appending later *** 
save SLNoLaborLand50_pr06, replace 

*** import 70% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr07.csv, clear

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

*** add infection rate variable ***
gen start_infection = 7

*** save dataset for appending later *** 
save SLNoLaborLand50_pr07, replace 

*** import 80% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr08.csv, clear

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

*** add infection rate variable ***
gen start_infection = 8

*** save dataset for appending later *** 
save SLNoLaborLand50_pr08, replace 

*** import 90% infection rate data *** 
import delimited using SaintLouisNoLaborLand50_pr09.csv, clear

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

*** add infection rate variable ***
gen start_infection = 9

*** save dataset for appending later *** 
save SLNoLaborLand50_pr09, replace 

*** append all data files together to make combined graphs ***
use SLNoLaborLand50_pr01, clear 
append using SLNoLaborLand50_pr02
append using SLNoLaborLand50_pr03
append using SLNoLaborLand50_pr04
append using SLNoLaborLand50_pr05
append using SLNoLaborLand50_pr06
append using SLNoLaborLand50_pr07
append using SLNoLaborLand50_pr08
append using SLNoLaborLand50_pr09

*** make one-demensional graph of first time period average ***
keep if time == 1 

*** generate starting infection fraction ***
gen s_infect = start_infection/10 

*** plot median fertilizer use relative to starting infection rate ***
twoway (rarea lower_fert upper_fert s_infect, color(gs14%50)) (connected med_fert s_infect), scheme(cleanplots) xtitle("Starting Infection Probability") ytitle("Median First Period Fertilizer Use (kg per hectare)") xlabel(0(0.1)1, format(%9.1fc)) ylabel(2.0(0.5)4.0, format(%9.1fc)) legend(off)
graph export Fertilizer_Infection_First.svg, replace 

graph export "Figure_3.png", as(png) name("Graph") replace 
