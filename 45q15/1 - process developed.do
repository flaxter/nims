/* Conservation notice:

45q15 with HIV covers developed countries where we do not need to exclude HIV as a cause of death. 

*/


clear
set mem 250m


insheet using "WHOMortCod NEW.csv", clear
rename iso iso3
rename country whoname
sort whocode
save "tmp/countrylist2007_sorted", replace

insheet using "45q15 with HIV.csv", comma clear
/* contains iso3 */
rename iso3 whocode
rename m m_with_hiv
rename f f_with_hiv
sort whocode
merge whocode using "tmp/countrylist2007_sorted", keep(iso3 whoname)

drop if _merge==2
drop _merge

drop country
sort whocode
save "tmp/developed_countries", replace
