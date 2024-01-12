*** Senegal Expenditure Share Calculation ***
*** Molly Doruska ***
*** Date Last Modified: October 13, 2022 ***

clear all
set more off 

*** set working directory ***
cd "~\BioeconomicModelResults"

*** import data ***
use "ehcvm_conso_sen2018.dta", clear 

*** two waves of data - first work with wave 1 data *** 
drop if vague == 2 

*** create total expenditure variable ***
collapse (sum) depan, by(hhid)

*** rename variable *** 
rename depan totalexpend

*** export file *** 
save hhtotalexpend, replace 

*** import data ***
use "ehcvm_conso_sen2018.dta", clear 

*** two waves of data - first work with wave 1 data *** 
drop if vague == 2 

*** collapse data by hhid and category ***
collapse (sum) depan, by(hhid codpr)

*** reshape data from long to wide ***
reshape wide depan, i(hhid) j(codpr)

*** create expenditure amounts ***
egen foodexpend = rowtotal(depan1 depan2 depan3 depan4 depan5 depan6 depan7 depan8 depan9 depan10 depan11 depan12 depan13 depan14 depan15 depan17 depan17 depan18 depan19 depan20 depan21 depan22 depan23 depan24 depan25 depan26 depan27 depan28 depan29 depan30 depan31 depan32 depan33 depan35 depan36 depan37 depan38 depan39 depan40 depan41 depan42 depan43 depan44 depan45 depan46 depan47 depan48 depan49 depan50 depan52 depan53 depan54 depan55 depan56 depan57 depan58 depan59 depan60 depan61 depan62 depan63 depan64 depan65 depan66 depan67 depan68 depan69 depan70 depan71 depan72 depan73 depan74 depan75 depan76 depan77 depan78 depan79 depan80 depan81 depan82 depan83 depan84 depan85 depan86 depan87 depan88 depan89 depan90 depan91 depan92 depan93 depan94 depan95 depan96 depan97 depan98 depan99 depan100 depan102 depan104 depan105 depan106 depan107 depan108 depan109 depan110 depan111 depan113 depan114 depan115 depan116 depan117 depan118 depan119 depan120 depan121 depan122 depan123 depan124 depan125 depan126 depan127 depan128 depan129 depan130 depan131 depan132 depan133 depan134 depan135 depan136 depan139 depan140 depan151 depan152), missing 
egen hhgoodexpend = rowtotal(depan202 depan203 depan204 depan205 depan206 depan207 depan208 depan209 depan210 depan211 depan212 depan213 depan214 depan215 depan216 depan217 depan303 depan304 depan305 depan306 depan307 depan309 depan310 depan311 depan312 depan313 depan314 depan315 depan316 depan317 depan318 depan319 depan320 depan321 depan322 depan332 depan333 depan334 depan335 depan336 depan337 depan338 depan401 depan402 depan403 depan404 depan405 depan406 depan407 depan408 depan409 depan410 depan411 depan501 depan502 depan503 depan504 depan505 depan506 depan507 depan508 depan509 depan510 depan511 depan512 depan521 depan615 depan617 depan618 depan619 depan620 depan621 depan622 depan625 depan626 depan627 depan628 depan629 depan630 depan631 depan632 depan636 depan637 depan638 depan639 depan640 depan643 depan644 depan645 depan646 depan647 depan661 depan662 depan663 depan664 depan665 depan666 depan667 depan668 depan669 depan670 depan671 depan672 depan801 depan802 depan803 depan804 depan805 depan806 depan807 depan808 depan809 depan810 depan811 depan812 depan813 depan814 depan815 depan816 depan817 depan818 depan819 depan820 depan821 depan822 depan823 depan824 depan825 depan826 depan827 depan828 depan829 depan830 depan831 depan832 depan833 depan834 depan835 depan836 depan837 depan838 depan839 depan412 depan413 depan414), missing
egen medicalexpend = rowtotal(depan416 depan417 depan651 depan681 depan682 depan683 depan684 depan685 depan686 depan691 depan692), missing

*** merge in total expenditure data *** 
merge 1:1 hhid using hhtotalexpend 

drop _merge 

*** create expenditure shares ***
gen foodshare = foodexpend/totalexpend
gen hhgoodshare = hhgoodexpend/totalexpend
gen medicalshare = medicalexpend/totalexpend

*** summarize expenditure shares ***
summarize foodshare hhgoodshare medicalshare

*** save total data *** 
save hhtotalexpendall1, replace 

*** import data ***
use "ehcvm_conso_sen2018.dta", clear 

*** two waves of data - now work with wave 2 data *** 
drop if vague == 1 

*** create total expenditure variable ***
collapse (sum) depan, by(hhid)

*** rename variable *** 
rename depan totalexpend

*** export file *** 
save hhtotalexpend2, replace 

*** import data ***
use "ehcvm_conso_sen2018.dta", clear 

*** two waves of data - now work with wave 2 data *** 
drop if vague == 1 

*** collapse data by hhid and category ***
collapse (sum) depan, by(hhid codpr)

*** reshape data from long to wide ***
reshape wide depan, i(hhid) j(codpr)

*** create expenditure amounts ***
egen foodexpend = rowtotal(depan1 depan2 depan3 depan4 depan5 depan6 depan7 depan8 depan9 depan10 depan11 depan12 depan13 depan14 depan15 depan17 depan17 depan18 depan19 depan20 depan21 depan22 depan23 depan25 depan26 depan27 depan28 depan29 depan30 depan31 depan32 depan33 depan35 depan36 depan37 depan38 depan39 depan40 depan41 depan42 depan43 depan44 depan45 depan46 depan47 depan48 depan49 depan50 depan51 depan52 depan53 depan54 depan55 depan56 depan57 depan58 depan59 depan60 depan61 depan62 depan63 depan64 depan65 depan66 depan67 depan68 depan69 depan71 depan72 depan73 depan74 depan75 depan76 depan77 depan78 depan79 depan80 depan81 depan82 depan83 depan84 depan85 depan86 depan87 depan88 depan89 depan90 depan91 depan92 depan93 depan94 depan95 depan96 depan97 depan98 depan99 depan100 depan102 depan104 depan105 depan107 depan108 depan109 depan110 depan111 depan112 depan113 depan114 depan115 depan116 depan117 depan118 depan119 depan120 depan121 depan122 depan123 depan124 depan125 depan126 depan127 depan128 depan129 depan130 depan131 depan132 depan133 depan134 depan135 depan136 depan139 depan140 depan151 depan152), missing 
egen hhgoodexpend = rowtotal(depan202 depan203 depan204 depan205 depan206 depan207 depan208 depan209 depan210 depan211 depan212 depan213 depan214 depan215 depan216 depan217 depan303 depan304 depan305 depan306 depan307 depan309 depan310 depan311 depan312 depan313 depan314 depan315 depan316 depan317 depan318 depan319 depan320 depan321 depan322 depan332 depan333 depan334 depan335 depan336 depan337 depan338 depan401 depan402 depan403 depan404 depan405 depan406 depan407 depan408 depan409 depan410 depan411 depan501 depan502 depan503 depan504 depan505 depan506 depan507 depan508 depan509 depan510 depan511 depan512 depan521 depan615 depan617 depan618 depan619 depan620 depan621 depan622 depan625 depan626 depan627 depan628 depan629 depan630 depan631 depan632 depan636 depan637 depan638 depan639 depan640 depan643 depan644 depan645 depan646 depan647 depan661 depan662 depan663 depan664 depan665 depan666 depan667 depan668 depan669 depan670 depan671 depan672 depan801 depan802 depan803 depan804 depan805 depan806 depan807 depan808 depan809 depan810 depan811 depan812 depan813 depan814 depan815 depan816 depan817 depan818 depan819 depan820 depan821 depan822 depan823 depan824 depan825 depan826 depan827 depan828 depan829 depan830 depan831 depan833 depan834 depan835 depan836 depan837 depan838 depan839 depan412 depan413 depan414), missing
egen medicalexpend = rowtotal(depan416 depan417 depan651 depan681 depan682 depan683 depan684 depan685 depan686 depan691 depan692), missing

*** merge in total expenditure data *** 
merge 1:1 hhid using hhtotalexpend2 

drop _merge 

*** create expenditure shares ***
gen foodshare = foodexpend/totalexpend
gen hhgoodshare = hhgoodexpend/totalexpend
gen medicalshare = medicalexpend/totalexpend

*** summarize expenditure shares ***
summarize foodshare hhgoodshare medicalshare

*** save total data *** 
save hhtotalexpendall2, replace 

*** append data ***
append using hhtotalexpendall1 

*** summarize both waves of data *** 
estpost summarize foodshare hhgoodshare medicalshare
esttab using Table_S3.rtf, cells("count mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") noobs replace