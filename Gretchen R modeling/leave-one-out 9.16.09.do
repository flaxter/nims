/**Simple model in stata**/

clear
set memory 400m
set more off
pause on
tempfile temp temp1 predictvalues pop
global dir = "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\"

use "$dir\HL studies.dta", clear

*GDP
gen lgdp= ln(gdp)
gen highinc = 0
replace highinc = 1 if gdp > 10000

*Keep only relevent data
drop if t1 < 25
for X in numlist 6 16 17 22 28: drop if sex != 0.5 & uid == X
for X in numlist 905 906 1101 1102: drop if record_id == X
drop if mage < 1

* Drop the ones with messed up pop data this needs to be fixed!!!
drop if uid == 34 | uid == 35

*Interaction and higher order terms
gen agelgdp= mage*lgdp
gen aget= mage*t1
gen age2 = mage*mage
gen th2 = t1*t1

*Rename nicely
ren t1 threshold

*create a weight
gen wght = 1/sqrt(n)
gen wght2 = 1/n

*get the missing n's using Emma's code
bysort uid: egen lines = count(prev)
replace n = round(sample_size/lines) if n == .
replace count = round(sample_size/lines*prev) if count == . 
drop lines
merge record_id using "$dir\agesexsizes_Sep15.dta", sort update
drop if _merge == 2
drop _merge

* do a spline and age dummies
mkspline age1_50 50 age2_50 = mage

gen childinc = age1_50*lgdp

* urban var
replace urban = ".5" if (urban != "1" & urban != "0")
destring urban, replace

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


forvalues x = 1/47 {
glm count age1_50 age2_50 aget sex threshold lgdp urban if uid != `x' [aweight=wght], family(binomial n) 
predict prevfull`x'
glm count age1_50 age2_50 aget sex threshold lgdp if uid != `x' [aweight=wght], family(binomial n) 
predict prevbare`x'
xtmelogit count age1_50 age2_50 aget sex threshold lgdp || region: if uid != `x', cov(uns) binomial(n)
predict hierbare`x'

foreach var in prevfull prevbare hierbare {
	if "`x'" == "1" {
			gen se`var' = (count-`var'`x')*(count-`var'`x')/(n*n) if uid == `x'
		} 
		else {
			replace se`var' = (count-`var'`x')*(count-`var'`x')/(n*n) if uid == `x'
		}
	drop `var'`x'
	}
}

foreach var in prevfull prevbare hierbare {
	sum se`var' 
	gen rmse`var' = sqrt(r(mean))
	sum se`var'  if mage > 70
	gen oldrmse`var' = sqrt(r(mean))
	sum se`var'  if mage < 50
	gen youngrmse`var' = sqrt(r(mean))
}

exit


****Try making a hierarchical model in stata

****Make regional averages: population-weighted sum of olscount and simplecount
keep if prediction == 1 & year == 2005

for X in varlist prevwght prevflat twokink agesqr incdummy nochina poorchild: replace X = X * pop
replace se = (prevflat - nochina)*(prevflat-nochina)
sum se if pred == 1 & thresh == 30 & sex == 1 & year == 2005
local rmse = sqrt(r(mean))
di "`rmse'" 


exit

collapse (sum) prevwght prevflat twokink agesqr incdummy nochina pop, by( gbd_region regionid age mage threshold sex )


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