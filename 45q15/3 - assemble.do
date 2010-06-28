/* Merge together developing, developed, and ihme data from 0, 1, 2 do files */

use "tmp/developed_countries", clear
append using "tmp/developing_countries"

sort iso3 year

merge iso3 year using "ihme45q15", keep(m_ihme f_ihme whoname)

drop _merge
sort iso3 year
order whoname whocode iso3 year m_ihme f_ihme m_hiv_free f_hiv_free m_with_hiv f_with_hiv

save "master45q15", replace

/*  for debugging: */
collapse (first) whoname, by(iso3)
count

use "master45q15", replace

