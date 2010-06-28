/* This merges together the two files for developing countries with HIV rates. "45q15 HIV free" contains HIV free data for 45q15 (i.e. with HIV and war as causes of death excluded). "45q15 high HIV ct" contains data for 45q15 with just war excluded--so HIV is not excluded */

pause on
insheet using "WHOMortCod NEW.csv", clear
rename iso iso3
rename country whoname
sort iso3
save "tmp/countrylist2007_sorted", replace


insheet using "45q15 high HIV ct.csv", comma clear
/*drop if country=="Afghanistan"*/
sort country iso3 year

save "tmp/hiv", replace

insheet using "45q15 HIV free.csv", comma clear
rename f f_hiv_free
rename m m_hiv_free


sort country iso3 year

/* Note that this would work:
merge using "tmp/hiv"
... but just to be safe, we might as well do this: */

merge (country iso3 year) using "tmp/hiv"

/* Note that _merge is 3 for all rows except Zimbabwe 2008 */

rename f f_with_hiv
rename m m_with_hiv

drop _merge

sort iso3
merge iso3 using "tmp/countrylist2007_sorted", keep(iso3 whoname whocode)

drop if _merge==2
drop _merge
drop country

sort whoname iso3 year

save "tmp/developing_countries", replace
