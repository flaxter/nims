/* Conservation notice:

The IHME data comes from :
Rajaratnam JK, Marcus JR, Levin-Rector A, Chalupka AN, Wang H, Dwyer L, Costa M, Lopez AD, Murray CJL. Worldwide mortality in men and women aged 15–59 years from 1970 to 2010: a systematic analysis. Lancet. 2010 Apr 30; 375:1704–20.

http://www.healthmetricsandevaluation.org/resources/news/2010/adult_mortality_trends_0410.html

The spreadsheet IHME provides contains no country codes, so we need to rename a bunch of countries to do string matching with whonames for countries. Two countries with no whocode need to be assigned an iso3 manually. 
*/

insheet using "WHOMortCod NEW.csv", clear
save "tmp/countrylist2007_sorted", replace

insheet using "adult_mortality_IHME_0410.csv", clear

gen match_country = country
replace match="Democratic Republic of the Congo" if country=="Congo, the Democratic Republic of the"

replace match="Iran (Islamic Republic of)" if country=="Iran, Islamic Republic of"
replace match="Democratic People's Republic of Korea" if country=="Korea, Democratic People's Republic of"
replace match="Republic of Korea" if country=="Korea, Republic of"
replace match="The former Yugoslav Republic of Macedonia" if country=="Macedonia, the Former Yugoslav Republic of"
replace match="Micronesia (Federated States of)" if country=="Micronesia, Federated States of"
replace match="Republic of Moldova" if country=="Moldova"

replace match="United Republic of Tanzania" if country=="Tanzania, United Republic of"
replace match="United States of America" if country=="United States"
replace match="United Kingdom of Great Britain and Northern Ireland" if country=="United Kingdom"
replace match="Cote d'Ivoire" if country=="Côte d'Ivoire"

rename country ihme_country
rename match_country country
joinby country using "tmp/countrylist2007_sorted", unmatched(master)
rename country whoname
rename ihme_country country
drop _merge
drop notincurrentuncodelist
sort country year

replace iso="TWN" if country=="Taiwan, Province of China"
replace iso="PSE" if country=="Occupied Palestinian Territory"

outsheet using "adult_mortality_IHME_0410 country codes.csv", replace comma

drop country
drop rank_male rank_female whoms
rename v45q15_female f_ihme
rename v45q15_male m_ihme
rename iso iso3
drop uncod
sort iso3 year
save "ihme45q15", replace

