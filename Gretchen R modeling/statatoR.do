/** This gets the output of Maya's dofile and preps it to go into R **/

clear
set memory 400m
pause on
set more off
tempfile temp smallage bigage alldata data pop
global dir = "D:\Files\GBD\Hearing loss\"

/* Get rid of wide ages -- this drops 6 surveys, all in grey in the final data sources table */

use "$dir\HL studies.dta", clear

ren CID iso3

/* Generate the superregions variable */

gen superregionid = RID
recode superregionid (1 6 16 = 1) (8/9 = 2) (18/21 = 3) (4=5) (15=4) (3/5 17 = 6) (11/14 = 7) (10=8)

/* Drop the lower than 25 data not in ISO booths
In isobooth 2 38 40/43 47 50 54 55 13 32
Not in isobooth 4 5 8 15 21 10 24 31 49 51 */

for X in numlist 4 5 8 15 21 10 24 31 49 51: drop if t1 < 25 & uid == X
egen studies = group(uid)
sum studies

/* Drop the lower than 41 data in schools: this should eliminate 2 studies */

for X in numlist 1 9 10 19 24 29 31 34 35 37 46 53: drop if t1 < 41 & uid == X
drop studies
egen studies = group(uid)
sum studies

/* If we ever have "both by age" and "by sex" or "all ages" keep "by age"  : these are 6,16,17,22,28 */

gen agedif=a2-a1
gen wideage = 0
replace wideage = 1 if agedif>15 & a1 < 60

for X in numlist 17 28: drop if uid == X & wideage == 1
drop if uid == 6 & wideage == 1 & (t1 == 27 | t1 == 90) /* keep 50 wideage for validation*/
drop if uid == 16 & wideage == 1 & (t1 == 41 | t1 == 61) 
drop if uid == 22 & wideage == 1 & (t1 == 41 | t1 == 61) 
drop if uid == 28 & wideage == 1 & (t1 == 41 | t1 == 26) 
drop agedif 

pause

/*  Interpolate n's by age/sex */

* Sample size is for both sexes .  . . code is by sex
gen n_tot=sample_size

replace n_tot = n_tot/2 if sex2 != "B" 

* Deal with a special case by fudging the age range
replace a2 = 9.99 if uid == 17 & a1 == 5 & t1 == 61
replace a1 = 10 if uid == 17 & a1 == 7 & t1 == 61

save `data'

* Get population data

use "X:\GBD2005\MODELLING\common datasets\popseries.dta", clear

*some countries have population by 80+ rather than 80-84, 85-89, etc. 

replace pop_80to84 = pop_80plus / 3 if pop_80to84 == . & pop_80plus != .
replace pop_85to89 = pop_80plus / 3 if pop_85to89 == . & pop_80plus != .
replace pop_90plus = pop_80plus / 3 if pop_90plus == . & pop_80plus != .
replace pop_0to1 = pop_0to1 + pop_1to4

drop pop_80plus pop_1to4

* Deal with inconsistent sex
ren sex sex2
gen sex = 1
replace sex = 0 if sex2 == "Female"
replace sex = 0.5 if sex2 == "Total"
sort iso3 year sex

save `pop'

use `data', clear        

sort iso3 year sex

merge iso3 year sex using `pop'
drop if _merge == 2


count
local N = r(N)  /* Total number of observations */
local N10 = ceil(`N' / 10)

gen general_pop = .
replace t1 = 41 if uid == 28 & a1 < 15 & t1 == 31


foreach r of numlist 0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90 {

                rename pop_`r'~~~~ pop_`r'

}
ren pop_5to9 pop_5


foreach a of numlist 1 / `N10' {
                local x = (`a' - 1)*10 + 1
                local y = `a'*10
                foreach n of numlist `x' / `y' {
                                if n[`n'] == . & n_tot[`n'] != . {
                                                local rd5_agestart = round(a1[`n'], 5)
                                                local rd5_ageend = round(a2[`n'], 5) - 1
                                                if a1[`n'] > 80 {
                                                                local rd5_agestart = 80
                                                                local rd5_ageend = 99
                                                }
                                                if a1[`n'] == a2[`n'] {
                                                                local rd5_ageend = round(a2[`n'], 5)
                                                }
                                                local pop = 0
                                                foreach r of numlist 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80  {
                                                                if `rd5_agestart' <= `r' & `rd5_ageend' >= `r' {
                                                                 local pop = `pop' + pop_`r'[`n']
                                                                }
                                                }
                                                capture qui replace general_pop = `pop' in `n'
                                }
                }
}

 

egen tot_pop = total(general_pop) if n == . & n_tot != ., by(uid sex t1)
gen per_pop = general_pop / tot_pop
replace n = round(n_tot * per_pop) if n == . & n_tot != .
drop tot_pop per_pop general_pop n_tot pop_* _merge

replace t1 = 31 if uid == 28 & a1 < 15 & t1 == 41


* ***** Generate a count if it didn't exist, and make sure sum_count and n are rounded

replace sum_count = round(n*prev) if sum_count == .
replace sum_count = round(sum_count)
replace n = round(n)

* ***** Drop babies and make age splines

drop if mage < 1
mkspline child 15 adult 50 oldadult = mage
mkspline childplusadult 50 oldadult = mage

* ****  Generate file with all data

save "$dir\Gretchen R modeling\Allstudies.dta", replace

exit

/* *********** This part makes the file for dismod ************ */

gen GBD_Cause = "HL"
gen Parameter = "prevalence data"
ren prev Parameter_Value
ren year Year_Start
gen Year_End=Year_Start
ren a1 Age_Start
ren a2 Age_End
ren iso3 Country_ISO3_Code
ren gbd_region Region
gen Units = 1

keep  Country_ISO3_Code Year_Start sex sex2 Age_Start Age_End t1 Parameter_Value n  GBD_Cause Parameter Year_End Region Units

drop sex
ren sex2 sex
replace Age_End = round(Age_End)
replace sex = "All" if sex == "B"
replace Age_Start = 0 if Age_Start == .5
ren n effective_sample_size

replace t1 = t1/100

outsheet using "$dir\dismod\HLdismod.txt", replace