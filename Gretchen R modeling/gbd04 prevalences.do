/*This file gets ranks of cause, Deaths and DALYs*/
clear                      
set mem 600m               
set more off		    	    
pause on 
tempfile temp
local rgn = "gbd05"
global exdir ="X:\CRA\2004 stata files\"

use "X:\BOD\2004 GBD final datasets\DalynatorIIC\Input\YLD\alldevsyld2004allbases.dta" if cause == "W102", clear
keep whocode cause seq age sex prevc p2004 yld inc
sort whocode
joinby whocode using "$exdir\codelist.dta" 
sort whocode sex
ren prevc cases
ren p2004 pop
recode seq (5=1) (6=2) (7=3) (8=4)
collapse (sum) cases pop, by(cause seq age sex `rgn')
save `temp'
collapse (sum) cases pop, by(cause seq age sex)
gen `rgn' = "World"
append using `temp'
drop if `rgn' == "Not included"
gen cdr = 100 * cases/pop
drop cases pop
reshape wide cdr, i(cause seq `rgn' sex) j(age)
joinby cause seq using "X:\BOD\2004 GBD final datasets\DalynatorIIC\Input\Keys\causemap.dta"
drop gbd cause_num sub2_name-pyld_group2 cause 
ren cause2 cause
gen cdr99 = 0.088*cdr0+0.173*cdr5+0.246*cdr15+0.214*cdr30+0.160*cdr45+0.067*cdr60+0.037*cdr70+0.015*cdr80
reshape long cdr, i(seq seq_name sex `rgn') j(age)
replace age = -99 if age == 99
save "X:\GBD2005\DISABILITY ENVELOPES\Hearing loss\Gretchen R modeling\gbd04prev.dta", replace

