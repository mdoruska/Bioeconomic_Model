*** Schisto Project - LSMS summary statistics ***
*** Code Owner: Molly Doruska ***
*** Date Modified: October 19, 2022 ***
*** Required Files: 2018 LSMS survey dta files ***
*** Required Package: estout ***
*** Outputs: dta files for data, summary statistics tables ***

clear all
set more off

** set working directory ***
cd "~\BioeconomicModelResults"

*** get basic summary statistics for female, literacy schooling ***
*** household size, labor days in the household, land holdings ***
*** percentage of rice farmers, irrigation and fertilizer use ***
*** general labor market conditions for household members ***

*** import basic demographic data from LSMS ***
use "s01_me_sen2018.dta", clear 

*** rename variables ***
rename s01q01 female 
rename s01q03a birthday
rename s01q03b birthmonth 
rename s01q03c birthyear 
rename s01q04a agenobirthyear 
rename s01q07 married 

*** recode female to correct indicator variable ***
replace female = 0 if female == 1
replace female = 1 if female == 2 

*** recode married indicator ***
replace married = 0 if married == 1 
replace married = 1 if married == 2 
replace married = 1 if married == 3 
replace married = 0 if married > 3 

*** calculate household size ***
egen hhsize = max(s01q00a), by(vague grappe menage)

*** keep only househld heads ***
keep if s01q02 == 1 

*** keep key variables ***
keep vague grappe menage s01q00a female birthday birthmonth birthyear agenobirthyear married hhsize

*** save dta file for age calculation and merging ***
save headbasicdemographic, replace 

*** import education data from LSMS ***
use "s02_me_sen2018.dta", clear 

*** rename variables ***
rename s02q01__1 readfrench
rename s02q01__2 readlocallanguage
rename s02q02__1 writefrench
rename s02q02__2 writelocallanguage
rename s02q03 formalschool 

*** recode formal school ***
replace formalschool = 0 if formalschool == 2

*** keep key education variables ***
keep vague grappe menage s01q00a readfrench readlocallanguage writefrench writelocallanguage formalschool

*** save data file for merging ***
save education, replace 

*** import LSMS agricultural data - parcelle level data ***
use "s16a_me_sen2018.dta", clear 

*** create rice indicator at plot level ***
gen ricep = (s16aq08 == 3) 

*** create irrigation indicator at plot level *** 
*** river as primary water source is not included as irrigation ***
gen irrigated = (s16aq17 < 4)
gen riverp = (s16aq17 == 4)

*** create other crop indicators at plot level ***
gen milletp = (s16aq08 == 1)
gen sorghomp = (s16aq08 == 2)
gen maizep = (s16aq08 == 4)
gen foniop = (s16aq08 == 7)
gen cowpeap = (s16aq08 == 8)
gen peanutp = (s16aq08 == 10)
gen okrap = (s16aq08 == 11)
gen sorellep = (s16aq08 == 12)
gen sesamep = (s16aq08 == 13)
gen cassavap = (s16aq08 == 14)
gen sweetpotatop = (s16aq08 == 15)
gen potatop = (s16aq08 == 16)
gen sweetpepperp = (s16aq08 == 17)
gen mintp = (s16aq08 == 18)
gen spicypepperp = (s16aq08 == 24)
gen melonp = (s16aq08 == 25)
gen watermelonp = (s16aq08 == 26)
gen lettucep = (s16aq08 == 27)
gen cabbagep = (s16aq08 == 28)
gen tomatop = (s16aq08 == 29)
gen carrotp = (s16aq08 == 30)
gen eggplantp = (s16aq08 == 32)
gen onionp = (s16aq08 == 33)
gen cucumberp = (s16aq08 == 34)

*** create household level rice and irrigation measures ***
collapse (max) rice=ricep (max) irrigation=irrigated (max) river=riverp (max) millet=milletp (max) sorghom=sorghomp (max) maize=maizep (max) fonio=foniop (max) cowpea=cowpeap (max) peanut=peanutp (max) okra=okrap (max) sorelle=sorellep (max) cassava=cassavap (max) sweetpotato=sweetpotatop (max) potato=potatop (max) sweetpepper=sweetpepperp (max) mint=mintp (max) spicypepper=spicypepperp (max) melon=melonp (max) watermelon=watermelonp (max) lettuce=lettucep (max) cabbage=cabbagep (max) tomato=tomatop (max) carrot=carrotp (max) eggplant=eggplantp (max) onion=onionp (max) cucumber=cucumberp, by(vague grappe menage)

*** save data for merging with other characterisitcs ***
save riceirrigation, replace 

*** import LSMS labor use data *** 
use "s04_me_sen2018.dta", clear 

*** rename variables ***
rename s04q06 agworklastweek
rename s04q07 hhenterpriselastweek
rename s04q08 outsideworklastweek 
rename s04q09 internshiplastweek 

*** generate profession categories ***
gen agriculture = (s04q29b == 6)
gen unemployed = (s04q29b == 12)

*** calculate number of members working in agriculture and unemployed 
egen agmems = total(agriculture), by(vague grappe menage)
egen unemployedmems = total(unemployed), by(vague grappe menage)

*** collapse into household data ***
collapse (sum) agworklastweek (sum) hhenterpriselastweek (sum) outsideworklastweek (sum) internshiplastweek (max) agmems (max) unemployedmems, by(vague grappe menage)

*** save for merging with other characterisitcs ***
save laborcategories, replace 

*** import LSMS survey date data for age calculations ***
use "s00_me_sen2018.dta", clear 

*** create year of interview variables ***
gen year1 = substr(s00q23a,1,4)
gen year2 = substr(s00q24a,1,4)
gen year3 = substr(s00q25a,1,4)

*** check if any interviews split years *** 
gen yearsplit12 = (year1 != year2)
gen yearsplit23 = (year2 != year3)
gen yearsplit13 = (year1 != year3)

*** only 3 households different - go with first year ***
destring year1, gen(year)

*** rename region and department ***
rename s00q01 region
rename s00q02 department 

*** keep year, key location variables for merging ***
keep vague grappe menage region department year 

*** save data ***
save locationyear, replace 

*** merge data together for summary statistics ***
use headbasicdemographic, clear 

*** merge in survey location and date variables *** 
merge 1:1 vague grappe menage using locationyear 
drop _merge 

*** calculate age ***
gen age = year - birthyear 
replace age = agenobirthyear if birthyear == 9999

*** merge in education data *** 
merge 1:1 vague grappe menage s01q00a using education

*** drop non-household heads *** 
drop if _merge == 2 
drop _merge 

*** merge in rice and irrigation data *** 
merge 1:1 vague grappe menage using riceirrigation
drop _merge 

*** merge in labor use data *** 
merge 1:1 vague grappe menage using laborcategories
drop _merge 

*** merge in land holdings and agricultrual labor days data ***
merge 1:1 vague grappe menage using phhfactors 
drop _merge 

*** merge in fertilizer use data *** 
merge 1:1 vague grappe menage using hhinputs 
drop _merge 

*** merge in weights ***
merge m:1 grappe using hhweight 
drop _merge 

*** calculate percentage of members that work in sectors *** 
gen aglastweekp = agworklastweek / hhsize 
gen hhentlastweekp = hhenterpriselastweek / hhsize 
gen outsidelastweekp = outsideworklastweek / hhsize 
gen internlastweekp = internshiplastweek / hhsize 

gen primaryagp = agmems / hhsize 
gen unemployedp = unemployedmems / hhsize 

*** create indicator for hired labor ***
gen hirelabor = (hholabor > 0)
replace hirelabor = . if hholabor == . 

*** summary statistics for key categories *** 
estpost tabstat hhland if region == 4 | region == 8, statistics(p25 p50 p75)
esttab using Table_S1.rtf, cells("p25 p50 p75") noobs replace

estpost summarize female age married readfrench writefrench formalschool hhsize hhfamilylabor hirelabor hholabor rice millet cowpea peanut irrigation river hhfertuse if region == 4 | region == 8
esttab using Table_S2.rtf, cells("count mean(fmt(3)) sd(fmt(3)) min max") noobs replace 
