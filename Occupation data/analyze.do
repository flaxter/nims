*First make a count var that is 1 . . .n # countries
*loop through it.  For each country id which one to pick (closest to the index year)
*id year picked
*save
*correlate!
*median year is 1997

clear
set more off
tempfile temp tai
pause on

use "D:\Files\GBD\Hearing loss\Occupation data\laborsta2C.dta"
keep if codesource == "B" | codesource == "BA" | codesource == "A"

egen index = group(country)
sum index
local max = r(max)
gen emp = .
gen year = .

	foreach x in 0 1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8 9 -9 10 -10 {
		local year = 1997 + `x'
		  replace year = `year' if emp==. & d`year' != . 
		  replace emp = d`year' if emp==. & d`year' != . 
	}

drop if sex == "Total men and women" & codecountry != "TW"	
save `temp', replace
keep if codecountry == "TW"
replace sex = "Women"
save `tai', replace
use `temp', clear
replace sex ="Men" if codecountry == "TW"
append using `tai'

*                          2513  "ISCO-1968"
 *                         5202  "ISCO-88"
  *                          89  "NSCO"
drop if classification == "NSCO"  /*??? do not know what this means*/

	*subclassification and codesubclassification are the key ones for occ cat
	gen noisy = 0
	replace noisy = 1 if substr(codesubclass,1,2) == "07" | substr(codesubclass,1,2) == "08" | substr(codesubclass,1,2) == "09"
	
	replace noisy = noisy*emp
	ren codecountry iso2
	joinby iso2 using "X:\GBD2005\BMI archive\Data files\Webtable\codelist2.dta"
	collapse (sum) noisy emp, by(regionname sex) /* This merges together different sources if they exist */
	gen percentnoisy = noisy/emp
	
	drop if percentnoisy == . /*this is clearly problematic*/
	
	joinby regionname using "X:\GBD2005\BMI archive\Data files\Webtable\codelist2.dta"
	replace sex = "female" if sex == "Women"
	replace sex = "male" if sex == "Men"
	ren percentnoisy noisyjob
	forvalues x = 1970/2010 {
		gen noisyjob`x' = noisyjob
	}
	drop noisy noisyjob emp ihme_195 gbd_list countryname country iso2 gbdname regionname	
	reshape long noisyjob, i(iso3 sex) j(year)
	order iso3 year sex noisyjob
	outsheet using "D:\Files\GBD\Hearing loss\Occupation data\percentnoisy.txt", comma replace
/* Get averages by GBD region then apply to countries! */