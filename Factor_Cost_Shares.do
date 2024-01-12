*** Senegal LSMS factor cost share calculations ***
*** Molly Doruska ****
*** Date last modified: January 10, 2023 ***

clear all
set more off 

*** set working directory ***
cd "~\BioeconomicModelResults"

*** import LSMS agricultural data - parcelle level data ***
use "s16a_me_sen2018.dta", clear 

*** create land size variable for each parcelle *** 
gen parcellesize = s16aq09a 
replace parcellesize = s16aq09a/10000 if s16aq09b == 2
gen parcellesizem = s16aq47 

*** create labor variables for each parcelle ***
egen pfamilyprep = rowtotal(s16aq33b_1 s16aq33b_2 s16aq33b_3 s16aq33b_4 s16aq33b_5 s16aq33b_6 s16aq33b_7 s16aq33b_8 s16aq33b_9 s16aq33b_10 s16aq33b_11 s16aq33b_12 s16aq33b_13 s16aq33b_14 s16aq33b_15 s16aq33b_16 s16aq33b_17 s16aq33b_18 s16aq33b_19 s16aq33b_20 s16aq33b_21 s16aq33b_22 s16aq33b_23 s16aq33b_24 s16aq33b_25 s16aq33b_26 s16aq33b_27 s16aq33b_28 s16aq33b_29 s16aq33b_30 s16aq33b_31 s16aq33b_32 s16aq33b_33 s16aq33b_34 s16aq33b_35 s16aq33b_36 s16aq33b_37 s16aq33b_38 s16aq33b_39 s16aq33b_40 s16aq33b_41 s16aq33b_42 s16aq33b_43)
egen pfamilyweeding = rowtotal(s16aq35b_1 s16aq35b_2 s16aq35b_3 s16aq35b_4 s16aq35b_5 s16aq35b_6 s16aq35b_7 s16aq35b_8 s16aq35b_9 s16aq35b_10 s16aq35b_11 s16aq35b_12 s16aq35b_13 s16aq35b_14 s16aq35b_15 s16aq35b_16 s16aq35b_17 s16aq35b_18 s16aq35b_19 s16aq35b_20 s16aq35b_21 s16aq35b_22 s16aq35b_23 s16aq35b_24 s16aq35b_25 s16aq35b_26 s16aq35b_27 s16aq35b_28 s16aq35b_29 s16aq35b_30 s16aq35b_31 s16aq35b_32 s16aq35b_33 s16aq35b_34 s16aq35b_35 s16aq35b_36 s16aq35b_37 s16aq35b_38 s16aq35b_39 s16aq35b_40 s16aq35b_41 s16aq35b_42 s16aq35b_43)
egen pfamilyharvest = rowtotal(s16aq37b_1 s16aq37b_2 s16aq37b_3 s16aq37b_4 s16aq37b_5 s16aq37b_6 s16aq37b_7 s16aq37b_8 s16aq37b_9 s16aq37b_10 s16aq37b_11 s16aq37b_12 s16aq37b_13 s16aq37b_14 s16aq37b_15 s16aq37b_16 s16aq37b_17 s16aq37b_18 s16aq37b_19 s16aq37b_20 s16aq37b_21 s16aq37b_22 s16aq37b_23 s16aq37b_24 s16aq37b_25 s16aq37b_26 s16aq37b_27 s16aq37b_28 s16aq37b_29 s16aq37b_30 s16aq37b_31 s16aq37b_32 s16aq37b_33 s16aq37b_34 s16aq37b_35 s16aq37b_36 s16aq37b_37 s16aq37b_38 s16aq37b_39 s16aq37b_40 s16aq37b_41 s16aq37b_42)

egen poadultmaleprep = rowtotal(s16aq39b_1)
egen poadultfemaleprep = rowtotal(s16aq39b_2) 
egen pochildmaleprep = rowtotal(s16aq39b_3) 
egen pochildfemaleprep = rowtotal(s16aq39b_4)  
egen poadultmaleweeding = rowtotal(s16aq41b_1)
egen poadultfemaleweeding = rowtotal(s16aq41b_2) 
egen pochildmaleweeding = rowtotal(s16aq41b_3) 
egen pochildfemaleweeding = rowtotal(s16aq41b_4) 
egen poadultmaleharvest = rowtotal(s16aq43b_1)
egen poadultfemaleharvest = rowtotal(s16aq43b_2) 
egen pochildmaleharvest = rowtotal(s16aq43b_3) 
egen pochildfemaleharvest = rowtotal(s16aq43b_4) 

gen ptotalfamilylabor = pfamilyprep + pfamilyweeding + pfamilyharvest
gen poprep = poadultmaleprep + poadultfemaleprep + pochildmaleprep + pochildfemaleprep
gen poweeding = poadultmaleweeding + poadultfemaleweeding + pochildmaleweeding + pochildfemaleweeding
gen poharvest = poadultmaleharvest + poadultfemaleharvest + pochildmaleharvest + pochildfemaleharvest
gen ptotalolabor = poprep + poweeding + poharvest

gen ptotallabor = ptotalfamilylabor + ptotalolabor
 
*** create fertilizer variable for each parcelle ***
gen pfertuse = s16aq27
replace pfertuse = 0 if pfertuse == 2 

gen pureause = s16aq29a1
replace pureause = s16aq29a1*1000 if s16aq29a2 == 2 
replace pureause = s16aq29a1*50 if s16aq29a2 == 3 
replace pureause = . if s16aq29a2 == 4 

gen pphosphateuse = s16aq29b1
replace pphosphateuse = s16aq29b1*1000 if s16aq29b2 == 2 
replace pphosphateuse = s16aq29b1*50 if s16aq29b2 == 3 
replace pphosphateuse = . if s16aq29b2 == 4 

gen pnpkuse = s16aq29c1
replace pnpkuse = s16aq29c1*1000 if s16aq29c2 == 2 
replace pnpkuse = s16aq29c1*50 if s16aq29c2 == 3 
replace pnpkuse = . if s16aq29c2 == 4 

gen pdapuse = s16aq29d1
replace pdapuse = s16aq29d1*1000 if s16aq29d2 == 2 
replace pdapuse = s16aq29d1*50 if s16aq29d2 == 3 
replace pdapuse = . if s16aq29d2 == 4 

egen pqfertuse = rowtotal(pureause pphosphateuse pnpkuse pdapuse) 
 
*** create compost variable for each parcelle ***
gen panimalcompost = (s16aq24a > 0)

*** aggregate up to household use levels ***
egen hhland = total(parcellesize), by(vague grappe menage)
egen hhlandm = total(parcellesizem), by(vague grappe menage)

egen hhfamilylabor = total(ptotalfamilylabor), by(vague grappe menage)
egen hholabor = total(ptotalolabor), by(vague grappe menage)
egen hhlabor = total(ptotallabor), by(vague grappe menage)

egen hhureause = total(pureause), by(vague grappe menage)
egen hhphosphateuse = total(pphosphateuse), by(vague grappe menage)
egen hhnpkuse = total(pnpkuse), by(vague grappe menage)
egen hhdapuse = total(pdapuse), by(vague grappe menage)

egen hhfertuse = max(pfertuse), by(vague grappe menage)
egen hhqfertuse = total(pqfertuse), by(vague grappe menage)
replace hhqfertuse = . if hhfertuse == . 

egen hhanimalcompost = max(panimalcompost), by(vague grappe menage)

*** calculate wage rates by group and task ***
gen adultmaleprepwage = s16aq39c_1 / s16aq39b_1
gen adultfemaleprepwage = s16aq39c_2 / s16aq39b_2 
gen childmaleprepwage = s16aq39c_3 / s16aq39b_3
gen childfemaleprepwage = s16aq39c_4 / s16aq39b_4 
gen adultmaleweedingwage = s16aq41c_1 / s16aq41b_1
gen adultfemaleweedingwage = s16aq41c_2 / s16aq41b_2 
gen childmaleweedingwage = s16aq41c_3 / s16aq41b_3 
gen childfemaleweedingwage = s16aq41c_4 / s16aq41b_4 
gen adultmaleharvestwage = s16aq43c_1 / s16aq43b_1
gen adultfemaleharvestwage = s16aq43c_2 / s16aq43b_2 
gen childmaleharvestwage = s16aq43c_3 / s16aq43b_3 
gen childfemaleharvestwage = s16aq43c_4 / s16aq43b_4

*** adult male harvest is modal type of labor ***
*** focus on adult male harvest wage for valuing labor in cost shares ***
gen malewage = s16aq43c_1 / (s16aq43b_1*s16aq43a_1) if s16aq43c_1 > 0

*** collapse into household level data for key inputs ***
collapse hhland hhlandm hhfamilylabor hholabor hhlabor hhfertuse hhqfertuse hhureause hhphosphateuse hhnpkuse hhdapuse hhanimalcompost malewage, by(vague grappe menage)

*** output household level dataset for merging with other price and use data ***
save phhfactors, replace  

*** import input use data *** 
use "s16b_me_sen2018.dta", clear 


*** animal compost use amount *** 
gen animalcompost = .
replace animalcompost = s16bq03a if s16bq01 == 1
replace animalcompost = s16bq03a/1000 if s16bq03b == 1 & s16bq01 == 1
replace animalcompost = s16bq03a*1000 if s16bq03b == 3 & s16bq01 == 1
replace animalcompost = s16bq03a*50 if s16bq03b == 5 & s16bq01 == 1
replace animalcompost = s16bq03a*50 if s16bq03b == 6 & s16bq01 == 1

*** animal compost bought quantity *** 
gen animalcompostbought = . 
replace animalcompostbought = s16bq09a if s16bq01 == 1
replace animalcompostbought = s16bq09a/1000 if s16bq09b == 1 & s16bq01 == 1
replace animalcompostbought = s16bq09a*1000 if s16bq09b == 3 & s16bq01 == 1
replace animalcompostbought = s16bq09a*50 if s16bq09b == 5 & s16bq01 == 1
replace animalcompostbought = s16bq09a*50 if s16bq09b == 6 & s16bq01 == 1

*** animal compost cost per kg *** 
gen animalcompostprice = s16bq09c / animalcompostbought if s16bq01 == 1

*** urea use amount *** 
gen urea = .
replace urea = s16bq03a if s16bq01 == 3
replace urea = s16bq03a/1000 if s16bq03b == 1 & s16bq01 == 3
replace urea = s16bq03a*1000 if s16bq03b == 3 & s16bq01 == 3
replace urea = s16bq03a*50 if s16bq03b == 5 & s16bq01 == 3
replace urea = s16bq03a*50 if s16bq03b == 6 & s16bq01 == 3

*** urea bought quantity *** 
gen ureabought = . 
replace ureabought = s16bq09a if s16bq01 == 3
replace ureabought = s16bq09a/1000 if s16bq09b == 1 & s16bq01 == 3
replace ureabought = s16bq09a*1000 if s16bq09b == 3 & s16bq01 == 3
replace ureabought = s16bq09a*50 if s16bq09b == 5 & s16bq01 == 3
replace ureabought = s16bq09a*50 if s16bq09b == 6 & s16bq01 == 3

*** urea cost per kg ***
gen ureaprice = s16bq09c / ureabought if s16bq01 == 3

*** phosphate use amount *** 
gen phosphate = .
replace phosphate = s16bq03a if s16bq01 == 4
replace phosphate = s16bq03a/1000 if s16bq03b == 1 & s16bq01 == 4
replace phosphate = s16bq03a*1000 if s16bq03b == 3 & s16bq01 == 4
replace phosphate = s16bq03a*50 if s16bq03b == 5 & s16bq01 == 4
replace phosphate = s16bq03a*50 if s16bq03b == 6 & s16bq01 == 4

*** phosphate bought quantity *** 
gen phosphatebought = . 
replace phosphatebought = s16bq09a if s16bq01 == 4
replace phosphatebought = s16bq09a/1000 if s16bq09b == 1 & s16bq01 == 4
replace phosphatebought = s16bq09a*1000 if s16bq09b == 3 & s16bq01 == 4
replace phosphatebought = s16bq09a*50 if s16bq09b == 5 & s16bq01 == 4
replace phosphatebought = s16bq09a*50 if s16bq09b == 6 & s16bq01 == 4

*** phosphate cost per kg ***
gen phosphateprice = s16bq09c / phosphatebought if s16bq01 == 4

*** NPK use amount *** 
gen npk = .
replace npk = s16bq03a if s16bq01 == 5
replace npk = s16bq03a/1000 if s16bq03b == 1 & s16bq01 == 5
replace npk = s16bq03a*1000 if s16bq03b == 3 & s16bq01 == 5
replace npk = s16bq03a*50 if s16bq03b == 5 & s16bq01 == 5
replace npk = s16bq03a*50 if s16bq03b == 6 & s16bq01 == 5

*** NPK bought quantity *** 
gen npkbought = . 
replace npkbought = s16bq09a if s16bq01 == 5
replace npkbought = s16bq09a/1000 if s16bq09b == 1 & s16bq01 == 5
replace npkbought = s16bq09a*1000 if s16bq09b == 3 & s16bq01 == 5
replace npkbought = s16bq09a*50 if s16bq09b == 5 & s16bq01 == 5
replace npkbought = s16bq09a*50 if s16bq09b == 6 & s16bq01 == 5

*** NPK cost per kg ***
gen npkprice = s16bq09c / npkbought if s16bq01 == 5

*** DAP use amount *** 
gen dap = .
replace dap = s16bq03a if s16bq01 == 6
replace dap = s16bq03a/1000 if s16bq03b == 1 & s16bq01 == 6
replace dap = s16bq03a*1000 if s16bq03b == 3 & s16bq01 == 6
replace dap = s16bq03a*50 if s16bq03b == 5 & s16bq01 == 6
replace dap = s16bq03a*50 if s16bq03b == 6 & s16bq01 == 6

*** DAP bought quantity *** 
gen dapbought = . 
replace dapbought = s16bq09a if s16bq01 == 6
replace dapbought = s16bq09a/1000 if s16bq09b == 1 & s16bq01 == 6
replace dapbought = s16bq09a*1000 if s16bq09b == 3 & s16bq01 == 6
replace dapbought = s16bq09a*50 if s16bq09b == 5 & s16bq01 == 6
replace dapbought = s16bq09a*50 if s16bq09b == 6 & s16bq01 == 6

*** DAP cost per kg ***
gen dapprice = s16bq09c / dapbought if s16bq01 == 6

*** look at mean and median prices ***
summarize animalcompostprice ureaprice phosphateprice npkprice dapprice, detail

*** create household level data set ***
collapse animalcompost urea phosphate npk dap animalcompostprice ureaprice phosphateprice npkprice dapprice, by(vague grappe menage)

*** save household level use data *** 
save hhinputs, replace 

*** import data set with regions ***
use "s00_me_sen2018.dta", clear 

*** keep wave, grappe, household, region and department  *** 
rename s00q01 region 
rename s00q02 department 

keep vague grappe menage region department 

*** save geography data ***
save hhregion, replace 

*** import data with household weights ***
use "ehcvm_ponderations_sen2018.dta", clear

*** save weights *** 
save hhweight, replace 
 
*** merge together datasets ***

*** import aggregated plot level data ***
use phhfactors, clear 

*** merge with household level input use data ***
merge 1:1 vague grappe menage using hhinputs 

drop _merge

*** merge with region data *** 
merge 1:1 vague grappe menage using hhregion

drop _merge 

*** merge with household weights *** 
merge m:1 grappe using hhweight

drop _merge 

*** generate cost of land, labor *** 
gen landcost = hhland*120000
egen laborprice = median(malewage), by(region)
gen laborcost = laborprice*hhlabor

*** calculate median fertilizer costs by region ***
egen rureacost = median(ureaprice) if ureaprice > 0, by(region)
egen rphosphatecost = median(phosphateprice) if ureaprice > 0, by(region)
egen rnpkcost = median(npkprice) if ureaprice > 0, by(region)
egen rdapcost = median(dapprice) if ureaprice > 0, by(region)

*** generate cost of fertilizer ***
gen ureacost = urea*rureacost 
gen phosphatecost = phosphate*rphosphatecost 
gen npkcost = npk*rnpkcost
gen dapcost = dap*rdapcost
egen fertcost = rowtotal(ureacost phosphatecost npkcost dapcost)

*** calculate mean animal compost cost by region ***
egen ranimalcompostcost = median(animalcompostprice) if animalcompostprice > 0, by(region)

*** generate cost of animal compost ***
gen compostcost = animalcompost*ranimalcompostcost

*** total input cost ***
egen totalinputcost = rowtotal(landcost laborcost fertcost compostcost)

*** calculate factor cost shares ***
gen landshare = landcost / totalinputcost
gen laborshare = laborcost / totalinputcost
gen fertshare = fertcost / totalinputcost
gen compostshare = compostcost / totalinputcost

*** summarize factor cost shares ***
estpost summarize landshare laborshare fertshare compostshare 
esttab using Table_S4.rtf, cells("count mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") noobs replace
