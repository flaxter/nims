clear
set memory 400m
pause on
set more off


/*The following studies use GBD age patterns: 2,4,5,8,15,21,38,40-43,47; in addition, 3,13,32,33 have good age pattern information */
use "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL studies.dta", clear
*GDP
gen lgdp= ln(gdp)

*Keep only relevent data
drop if t1 <= 20
for X in numlist 6 16 17 22 28: drop if sex != 0.5 & uid == X

*Interaction terms
gen agelgdp= mage*lgdp
gen aget= mage*t1

*logit of prevalence
replace prev = 0.001 if prev < 0.001
replace prev = 0.999 if prev > 0.999
gen logitprev = logit(prev)

*Rename nicely
ren t1 threshold

*keep ones with full age information
gen keep = 0
for X in numlist 2 4 5 8 15 21 40 41 42 43 47 3 13 32 33: replace keep = 1 if uid == X  /*3 13 32 33*/
keep if keep == 1

*get the missing n's using Emma's code
bysort uid: egen lines = count(prev)
replace n = round(sample_size/lines) if n == .
replace count = round(sample_size/lines*prev) if count == . 
drop lines
merge record_id using "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\agesexsizes_Jul20dataset.dta", sort update
drop if _merge == 2

*create a weight
gen wght = 1/sqrt(n)
gen olswght = sqrt(n)
/*
***** dummy logit model
gen pred = 0
append using "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\age cutoff\quickpred.dta"
gen child = mage
replace child = 0 if mage > 40
*replace child = 1 if mage < 20
xi: glm count i.child mage agelgdp aget sex threshold lgdp [aweight=wght] if pred == 0, family(binomial n) 

/* This part just makes a graph of the prediction
replace n = 100 if pred == 1
predict simplecount
scatter simplecount mage if pred == 1
replace prev = simplecount / n if prev == .
replace logitprev = logit(prev)
scatter logitprev mage if pred == 1  */
drop if pred == 1  

**** using mkspline for age only -- minimum is 45
foreach a in 10 15 25 35 40 45 50 {
	mkspline age1_`a' `a' age2_`a' = mage
	glm count age1_`a' age2_`a' agelgdp aget sex threshold lgdp [aweight=wght], family(binomial n)
}


**** also for all age interactions for everything -- minimum is 45
foreach a in 15 25 40 45 50 {
	foreach var in 1 2 {
		gen agelgdp`var'_`a' = lgdp*age`var'_`a'
		gen aget`var'_`a' = threshold*age`var'_`a'
	}
	glm count age1_`a' age2_`a' agelgdp1_`a' aget1_`a' agelgdp2_`a' aget2_`a' sex threshold lgdp [aweight=wght], family(binomial n)
}

**** when the child correction is not allowed to be curved -- minimum is age 35
foreach a in 20 25 30 35 40 45 {
	gen dummy`a' = 0
	replace dummy`a' = 1 if mage <= `a'
	gen mage`a' = mage
	replace mage`a' = `a' if mage <= `a'
	glm count mage`a' dummy`a' sex threshold lgdp [aweight=wght], family(binomial n)
}
*/
*****checking for different thresholds
foreach thresh in 35 50 65 80 95 {
	foreach a in 35 40 45 50 55 60 {
		mkspline age1_`a' `a' age2_`a' = mage
		glm count age1_`a' age2_`a' agelgdp sex threshold lgdp [aweight=wght] if threshold == `thresh', family(binomial n)
	}
	drop age1* age2*
	pause
}

