/**Simple model in stata**/

clear
set memory 400m
set more off
pause on
tempfile temp temp1 predictvalues pop

use "X:\GBD2005\MODELLING\common datasets\Prediction values.dta", clear
drop _merge
save `predictvalues', replace

use "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL studies.dta", clear

*GDP
gen lgdp= ln(gdp)

*Keep only relevent data
drop if t1 < 20
for X in numlist 6 16 17 22 28: drop if sex != 0.5 & uid == X

*Interaction and higher order terms
gen agelgdp= mage*lgdp
gen aget= mage*t1

*Rename nicely
ren t1 threshold

*logit of prevalence
replace prev = 0.001 if prev < 0.001
replace prev = 0.999 if prev > 0.999
gen logitprev = logit(prev)

*get the missing n's using Emma's code
bysort uid: egen lines = count(prev)
replace n = round(sample_size/lines) if n == .
replace count = round(sample_size/lines*prev) if count == . 
drop lines
*merge record_id using "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\agesexsizes.dta", sort update

gen prediction = 0
save `temp', replace


********Model 1, two ways: 
/*
Logistic model with the following variables:
sex - categorical
age - continuous, midpt of age
fprev - percent of hearing loss
gdp - log of gdp
threshold - cumulative threshold

interaction terms:
agelgdp - age and gdp
aget - age and threshold

The equivalent of this model is 
blogit count n mage agelgdp aget sex threshold lgdp 
*/


glm count mage agelgdp aget sex threshold lgdp, family(binomial n) 
pause
append using `predictvalues'
replace n = 100 if prediction == 1
predict simplecount
***Diagnostics
scatter count simplecount if prediction == 0
pause
sum count simplecount if prediction == 0
scatter simplecount age if threshold == 50 & prediction == 1
pause

********Model 2: 
/*
OLS with the logit of prevalence as the Y variable
sex - categorical
age - continuous, midpt of age
fprev - percent of hearing loss
gdp - log of gdp
threshold - cumulative threshold

interaction terms:
agelgdp - age and gdp
aget - age and threshold
*/

regress logitprev mage agelgdp aget sex threshold lgdp if prediction == 0
predict olslogit
gen olscount = invlogit(olslogit)*n
***Diagnostics
scatter count olscount if prediction == 0
pause
sum count olscount if prediction == 0
scatter olscount age if threshold == 50 & prediction == 1
pause
save "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL predictions all.dta", replace

****Make regional averages: population-weighted sum of olscount and simplecount
keep if prediction == 1 & year == 2005

for X in varlist olscount simplecount: replace X = X * pop
collapse (sum) olscount simplecount pop, by( gbd_region regionid age mage threshold sex )
for X in varlist olscount simplecount: replace X = X / pop

ren regionid region
merge region age sex threshold using "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Emma final files\hierpredictions2005.dta", sort 
keep if _merge == 3 /*this drops unmatching threshold and Emma's 100-yr-old age group*/

scatter olscount simplecount median age if region == 9 & threshold == 50

save "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\HL predictions region.dta", replace

/*
count
local num = e(N)
drop in 1/`num'
*/

/*The following can be used to generate a file for predicting country values
*get population data
use "X:\GBD2005\MODELLING\common datasets\popseries_clean.dta", clear
recode age (25/30 = 25) (35/40 = 35) (45/50 = 45) (55/60 = 55) (65/70 = 65) (75/80 = 75) (85/100 = 85)
collapse (sum) pop, by(gbd_c gbd_r iso year sex age region)
ren gbd_c country
replace country = "USA" if country == "United States of America"
replace country = "DRC" if country == "Democratic Republic of the Congo"
replace country = "Tanzania" if country == "United Republic of Tanzania"
drop gbd_r country
drop if iso3 == ""
save `pop', replace

*Create the prediction dataset
use "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\June 2009 Analysis\IHME_gdp.dta", clear
keep  iso3 gbd_country gbd_region gdppc_1990 gdppc_2005
capture drop count
sort gbd_region
egen regionid = group(gbd_region)
sort gbd_country
gen count=_n
drop if count==177 | count==216  
drop count
reshape long gdppc_, i(iso3 gbd_country gbd_region regionid) j(year)
rename gbd_country country
rename gdppc_ gdp

replace country = "USA" if country == "United States of America"
replace country = "DRC" if country == "Democratic Republic of the Congo"
replace country = "Tanzania" if country == "United Republic of Tanzania"
for X in numlist 0 1 5 10 15 20 25 35 45 55 65 75 85: gen idX = 1
reshape long id, i(iso3 country gbd_region gdp year regionid) j(age)
ren id id0
gen id1 = id0
reshape long id, i(iso3 country gbd_region gdp year age regionid) j(sex)
drop if iso3 == ""
merge iso year sex age region using `pop', sort
keep if _merge == 3  /*note that this drops a lot of random countries*/

drop id
for X in numlist 20 35 50 65 80 95: gen idX = 1
reshape long id, i(iso3 country gbd_region gdp year age regionid sex pop) j(threshold)
gen mage = age + 5
replace mage = age + 0.5 if age == 0
replace mage = age + 2 if age == 1
replace mage = age + 2.5 if age >1 & age < 25

gen lgdp = ln(gdp)
gen agelgdp= mage*lgdp
gen aget= mage*threshold

drop if gdp == .

gen prediction = 1
save "X:\GBD2005\MODELLING\common datasets\Prediction values.dta", replace
*/