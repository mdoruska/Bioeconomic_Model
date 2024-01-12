*** Vegetation production function estimate ***
*** Code Owner: Molly Doruska ***
*** Date Modified: January 12, 2024 ***
*** Required Files: VegRemovalData.xlsx from Rohr et al. (2023) replication pacakge ***
*** Outputs: vegetation production function estimate ***
*** Package required: estout ***

clear all
set more off

*** set working directory 
cd "~\BioeconomicModelResults"

*** import data ***
import excel VegRemovalData.xlsx, firstrow sheet("Table_vegetation")

*** drop extra rows ***
drop if Removalround == .

*** create log of person days ***
gen logpersondays = log(manhr)

*** estiamte production function ***
reg logkg logpersondays, robust 
eststo vegprod 

*** calculate scale parameter ***
scalar constant = exp(_b[_cons])
display constant

*** export regression results ***
esttab vegprod using Table_S8.rtf, replace se ar2 star(* 0.1 ** 0.05 *** 0.01)
