clear
set memory 400m
pause off
tempfile predictions

use "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL predictions all.dta", clear

****Make regional averages: population-weighted sum of olscount and simplecount
keep if prediction == 1 & year == 2005
pause
for X in varlist olscount simplecount: replace X = X * pop
collapse (sum) olscount simplecount pop, by( gbd_region regionid age mage threshold sex )
for X in varlist olscount simplecount: replace X = X / pop
pause
ren regionid region
merge region mage sex threshold using "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Emma final files\July09 Prep\hierpredictions2005.dta", sort 
pause
keep if _merge == 3 /*this drops unmatching threshold and age group*/

*save "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL predictions region.dta", replace

gen developed = 0
for X in numlist 1 6 10 16 8 9: replace developed = 1 if region == X
for X in varlist olscount simplecount media: replace X = X * pop
collapse (sum) olscount simplecount media pop, by(developed age mage threshold sex )
for X in varlist olscount simplecount media: replace X = X / pop 
label variable olscount "OLS"
label variable simplecount "Logistic"
label variable media "Hierarchical"
save `predictions', replace


/*The following studies use GBD age patterns: 2,4,5,8,15,21,38,40-43,47; in addition, 3,13,32,33 have good age pattern information */
use "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL studies.dta", clear
drop if a1 < 5
gen developed = 0
for X in numlist 1 6 10 16 8 9: replace developed = 1 if RID == X
gen keep = 0
for X in numlist 2 4 5 8 15 21 40 41 42 43 47: replace keep = 1 if uid == X
keep if keep == 1
replace prev = 0.001 if prev < 0.001
replace prev = 0.999 if prev > 0.999
gen logitprev = logit(prev)
replace prev = prev * 100
keep prev logitprev sex a1 mage t1 uid developed
ren t1 threshold
ren a1 age
reshape wide prev logitprev, i(  sex mage age threshold developed) j(uid)

label variable prev2 "Australia"
label variable prev4 "Brazil 03"
label variable prev5 "Brazil 06"
label variable prev8 "China"
label variable prev15 "India"
label variable prev21 "Madagascar"
label variable prev40 "USA 73"
label variable prev41 "USA 78"
label variable prev42 "USA 01"
label variable prev43 "USA 05"
label variable prev47 "Norway"

label variable logitprev2 "Australia"
label variable logitprev4 "Brazil 03"
label variable logitprev5 "Brazil 06"
label variable logitprev8 "China"
label variable logitprev15 "India"
label variable logitprev21 "Madagascar"
label variable logitprev40 "USA 73"
label variable logitprev41 "USA 78"
label variable logitprev42 "USA 01"
label variable logitprev43 "USA 05"
label variable logitprev47 "Norway"

merge developed age threshold sex using `predictions', sort


local dev = 0
 forvalues sex = 0/1 {
		foreach age in 35 65 {
			scatter  olscount simplecount media prev4 prev5 prev8 prev15 prev21 thresh if age == `age' & developed == `dev' & sex == `sex', c(l l l) ytitle("Prevalence of hearing loss")
			graph export "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Graphs\percentgraph_sex`sex'_dev`dev'_age`age'.emf", as(emf) replace
		}
}
 
local dev = 1
 forvalues sex = 0/1 {
		foreach age in 35 65 {
			scatter  olscount simplecount media prev2 prev40 prev41 prev42 prev43 prev47 thresh if age == `age' & developed == `dev' & sex == `sex', c(l l l) ytitle("Prevalence of hearing loss")
			graph export "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Graphs\percentgraph_sex`sex'_dev`dev'_age`age'.emf", as(emf) replace
		}
}

		foreach age in 15 35 65 85 {
			scatter logitprev4 logitprev5 logitprev8 logitprev15 logitprev21 logitprev2 logitprev40 logitprev41 logitprev42 logitprev43 logitprev47 thresh if age == `age', ytitle("Logit(prevalence of hearing loss)") 
			graph export "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Graphs\logitprevbythresh_age`age'.emf", as(emf) replace
		}


local dev = 0
 forvalues sex = 0/1 {
		foreach thresh in 35 50 {
			scatter  olscount simplecount media prev4 prev5 prev8 prev15 prev21 age if threshold == `thresh' & developed == `dev' & sex == `sex', c(l l l) ytitle("Prevalence of hearing loss")
			graph export "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Graphs\percentgraph_sex`sex'_dev`dev'_t`thresh'.emf", as(emf) replace
		}
}
 
local dev = 1
 forvalues sex = 0/1 {
		foreach thresh in 35 50 {
			scatter  olscount simplecount media prev2 prev40 prev41 prev42 prev43 prev47 age if threshold == `thresh' & developed == `dev' & sex == `sex', c(l l l) ytitle("Prevalence of hearing loss")
			graph export "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Graphs\percentgraph_sex`sex'_dev`dev'_t`thresh'.emf", as(emf) replace
		}
}

 forvalues sex = 0/1 {
		foreach thresh in 35 50 {
			scatter logitprev4 logitprev5 logitprev8 logitprev15 logitprev21 logitprev2 logitprev40 logitprev41 logitprev42 logitprev43 logitprev47 mage if mage < 45 & threshold == `thresh' & sex == `sex', ytitle("Logit(prevalence of hearing loss)") c(l l l l l l l l l l l)
			graph export "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Graphs\logitprevbyage_young_t`thresh'_sex`sex'.emf", as(emf) replace
		}
}		
		