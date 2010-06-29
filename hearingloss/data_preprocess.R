# gets the data into a form where we can run models
# load up useful packages (including interface to winbugs)
library("arm")
setwd( "/Users/emma/Documents/thesis_brainstorm/meetings_notes/who/newdata/")

# read in data
library(foreign) # so can read in stata files

agewts<-read.dta("agewts.dta") # global population age weights
surveydata<-read.dta("HLstudies11Apr10.dta");
samplesized<-read.table("hearinglosssamplesize.txt",header=TRUE)
agepopdata<-read.dta("popseries.dta");

# change all names to be consistent among files
ii<-which(agepopdata$gbd_country == "China, Hong Kong SAR" );
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Hong_Kong";
}
ii<-which(agepopdata$gbd_country == "Micronesia (Fed. States of)");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Micronesia (Federated States of)";
}
ii<-which(agepopdata$gbd_country == "Moldova");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Republic of Moldova";
}
ii<-which(agepopdata$gbd_country == "Saint Vincent and the Grenadines");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Saint Vincent";
}
ii<-which(agepopdata$gbd_country == "TFYR Macedonia");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"The Former Yugoslav Republic of Macedonia";
}

ii<-which(agepopdata$gbd_country == "Lao People's Democratic Republic");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Lao PDR";
}
ii<-which(agepopdata$gbd_country == "Dem. People's Republic of Korea");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"DPR Korea";
}
ii<-which(agepopdata$gbd_country == "Hong Kong");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Hong_Kong";
}
ii<-which(agepopdata$gbd_country == "Timor-Leste");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Timore_Leste";
}
ii<-which(agepopdata$gbd_country == "S\xe3o Tom\xe9 and Pr\xedncipe");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Sao Tome and Principe";
}
ii<-which(agepopdata$gbd_country == "New Zealand");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"New_Zealand";
}
ii<-which(agepopdata$gbd_country == "Serbia");
if(length(ii) > 0)
{
	agepopdata$gbd_country[ii]<-"Serbia and Montenegro";
}


write.dta(agepopdata,"popseries.dta");
countryregion<-read.dta("countryregion_data.dta")
ii<-which(countryregion$country == "Sri_Lanka");
countryregion$country[ii]<-"Sri Lanka";
countryregion$country[15]<-"DPR Korea";
countryregion$country[26]<-"Lao PDR";
countryregion$country[37]<-"Viet Nam";
write.dta(countryregion,"countryregion_data.dta")

inds<-grep("Venezuela",agepopdata$gbd_country);
if(length(inds)> 0)
{
	agepopdata$gbd_country[inds]<-"Venezuela";
}
inds<-grep("Brunei Darussalam",agepopdata$gbd_country);
if(length(inds)>0)
{
	agepopdata$gbd_country[inds]<-"Brunei";
	write.dta(agepopdata,"popseries.dta");
}

countrygdpyear<-read.dta("newcountry_GDPs.dta")
countrygdpyear$gbd_country[155]<-"DPR Korea";
countrygdpyear$gbd_country[105]<-"Lao PDR";
countrygdpyear$gbd_country[80]<-"Hong_Kong";
countrygdpyear$gbd_country[188]<-"Timore_Leste";
countrygdpyear$gbd_country[204]<-"Viet Nam";
countrygdpyear$gbd_country[145]<-"New_Zealand";
countrygdpyear$gbd_country[173]<-"Serbia and Montenegro";
countrygdpyear$gbd_country[174]<-"Sao Tome and Principe";
countrygdpyear$gbd_country[11]<-"Antigua and Barbuda";
ii<-which(countrygdpyear$gbd_country == "Saint_Kitts_and_Nevis")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"Saint Kitts and Nevis";
}

ii<-which(countrygdpyear$gbd_country == "French_Polynesia")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"French Polynesia";
}

ii<-which(countrygdpyear$gbd_country == "Micronesia (Fed. States of)");
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"Micronesia (Federated States of)";
}
ii<-which(countrygdpyear$gbd_country == "New_Caledonia")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"New Caledonia";
}
ii<-which(countrygdpyear$gbd_country == "Occupied_Palestinian_Territory")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"Occupied Palestinian Territory";
}
ii<-which(countrygdpyear$gbd_country == "Papua_New_Guinea")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"Papua New Guinea";
}
ii<-which(countrygdpyear$gbd_country == "Saint Vincent and the Grenadines")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"Saint Vincent";
}
ii<-which(countrygdpyear$gbd_country == "TFYR Macedonia")
if(length(ii) > 0)
{
	countrygdpyear$gbd_country[ii]<-"The Former Yugoslav Republic of Macedonia";
}

ii<-which(countrygdpyear$gbd_country == "Samoa")
if(length(ii) == 0)
{
	# add in GDP info for Samoa
ngdp<-length(countrygdpyear$gbd_country);
countrygdpyear[ngdp+1,1]<-"Samoa";
countrygdpyear[ngdp+1,2:17]<-c(
2954.572,
2856.585,
2779.337,
2847.671,
2675.121,
2833.901,
3026.719,
3042.851,
3129.439,
3157.674,
3363.603,
3576.952,
3678.055,
3761.241,
3849.032,
4016.758)
}
#countrygdpyear$gbd_country[]<-"";
write.dta(countrygdpyear,"newcountry_GDPs.dta");

#countrygdpyear<-read.table("countrygdbyear.txt",header=TRUE)
gbd2004<-read.table("gbd2004collapsed.txt",header=TRUE)

ii<-which(surveydata$country == "Viet_Nam")
surveydata$country[ii]<-"Viet Nam"
ii<-which(surveydata$country == "Tanzania")
surveydata$country[ii]<-"United Republic of Tanzania"
ii<-which(surveydata$country == "USA")
surveydata$country[ii]<-"United States of America"
ii<-which(surveydata$country == "DRC");
surveydata$country[ii]<-"Democratic Republic of the Congo"
ii<-which(surveydata$country == "Costa_Rica")
surveydata$country[ii]<-"Costa Rica";
ii<-which(surveydata$country == "Sierra_Leone")
surveydata$country[ii]<-"Sierra Leone"
ii<-which(surveydata$country == "Sri_Lanka")
surveydata$country[ii]<-"Sri Lanka"
ii<-which(surveydata$country == "South_Africa")
surveydata$country[ii]<-"South Africa"
ii<-which(surveydata$country == "United_Kingdom")
surveydata$country[ii]<-"United Kingdom"

# remove study 39 if present (same as 40)
inds<-which(surveydata$uid > 39 | surveydata$uid < 39)
surveydata<-surveydata[inds,];

# remove studies which have very broad age range data for 
# both sexes which is then repeated in narrower ages

rm_inds<-c(904:910,1101:1106);
inds<-which(surveydata$record_id %in% rm_inds);
if(length(inds) > 0)
{
oinds<-setdiff(1:length(surveydata$record_id),inds);
surveydata<-surveydata[oinds,];
write.dta(surveydata,'HLstudiesApr10_2010_rmdup.dta');
}

# remove school studies with thresholds <= 40dB
schoolid<-c(1,9,10,19,24,31,34,35,37,46,53);
inds<-which(surveydata$uid %in% schoolid & surveydata$t1 <= 40);
if(length(inds)>0)
{
	oinds<-setdiff(1:length(surveydata$country),inds)
	surveydata<-surveydata[oinds,];
	write.dta(surveydata,'HLstudiesApr10_2010_rmschool.dta')
}

# need to sort data by survey, sex, age, threshold
surveydata<-surveydata[order(surveydata$uid,surveydata$sex,surveydata$a1,
	surveydata$a2,surveydata$t1),]

# assign almost all variables in data set to shorter names for 
# ease of reference
N<-length(surveydata$uid)
agel<-surveydata$a1
ageh<-surveydata$a2
age<-surveydata$mage
year<-surveydata$year
thresh1<-surveydata$t1
thresh2<-surveydata$t2
country<-surveydata$country
corig<-surveydata$counts
survey<-surveydata$uid
gdp<-log(surveydata$gdp)
sex<-surveydata$sex
sexagen<-surveydata$n
# later separate into regions & also look at sex
p<-surveydata$prev
regionid<-surveydata$RID
female<-0*sex
female[which(sex == 1)]<-1

usurvey<-unique(survey)
for(i in 1:length(usurvey))
{
	inds<-which(survey == usurvey[i]);
	if(max(p[inds]) > 1)	
	{
		print(paste("Prev above 1: Survey ",usurvey[i]))	
	}
	
}

# all GBD2005 region names: later could read these in automatically
regionnames=c("Asia Pacific, High Income","Asia, Central",
	"Asia, East","Asia, South","Asia, Southeast",
	"Australasia","Caribbean","Europe, Central",
	"Europe, Eastern","Europe, Western",
	"Latin America, Andean","Latin America, Central",
	"Latin America, Southern","Latin America, Tropical",
	"North Africa, Middle East","North America, High Income",
	"Oceania","Sub-Saharan Africa, Central",
	"Sub-Saharan Africa, East",
	"Sub-Saharan Africa, Southern",
	"Sub-Saharan Africa, West")

# set region names/ids
region2super<-c(1,2,6,5,6,1,7,2,2,8,
	7,7,7,7,4,1,6,3,3,3,3)

# SS africa, High income countries, everyone else
super1<-c("Asia Pacific, High Income", 
		"North America, High Income",
        "Australasia");
super2<-c("Europe, Central",
        "Europe, Eastern",
        "Asia, Central");
super3<-c(   "Sub-Saharan Africa, East",
	"Sub-Saharan Africa, Southern",
        "Sub-Saharan Africa, West",
        "Sub-Saharan Africa, Central");
super4<-c(        "North Africa, Middle East",
        "North Africa/Middle East");        
super5<-c("Asia, South");
super6<-c("Asia, Southeast",
		"Asia, East",
        "Oceania");
super7<-c("Caribbean",
        "Latin America, Andean",
        "Latin America, Central",
        "Latin America, Tropical",
        "Latin America, Southern")
super8<-c("Europe, Western") 
superregions<-list(super1,super2,super3,super4,super5,super6,super7,
	super8);
origregionid<-regionid

regionsetC<-origregionid
regionsetC[which(surveydata$gbd_region %in% super1)]<-1
regionsetC[which(surveydata$gbd_region %in% super2)]<-2
regionsetC[which(surveydata$gbd_region %in% super3)]<-3
regionsetC[which(surveydata$gbd_region %in% super4)]<-4
regionsetC[which(surveydata$gbd_region %in% super5)]<-5
regionsetC[which(surveydata$gbd_region %in% super6)]<-6
regionsetC[which(surveydata$gbd_region %in% super7)]<-7
regionsetC[which(surveydata$gbd_region %in% super8)]<-8

regionid<-regionsetC;



######################################################################
# Take all data reported for both sexes and convert to male or female
# using country-specific sex ratios
# also get sample sizes for each age & sex group by using
# country, year specific age and sex population data
######################################################################
source("computesamplesize.R")

mfdata<-computesamplesize(survey,thresh1,thresh2,year,country,
        agepopdata, agel,ageh,gdp,regionid,age,sex,p)

newage<-mfdata[[1]]
newsex<-mfdata[[2]]
newregion<-mfdata[[3]]
newsurvey<-mfdata[[4]]
newthresh<-mfdata[[5]]
newcountsall<-mfdata[[6]]
newp<-mfdata[[7]]
newcountry<-mfdata[[8]]
newthresh2<-mfdata[[9]]
newagel<-mfdata[[10]]
newageh<-mfdata[[11]]
newyear<-mfdata[[12]]
newsexratio<-mfdata[[13]]
newposcounts<-mfdata[[14]]
newrecordid<-mfdata[[15]]
newsamplesize<-mfdata[[16]]
newgdp<-mfdata[[17]]

newwc<-unique(newcountry[which(newageh-newagel >= 15)])

# store imputed counts in stata format to write out
agesexall<-data.frame(age=newage,sex=newsex,region=newregion,
survey=newsurvey,thresh=newthresh,record_id=newrecordid,count=newposcounts,
n=newcountsall,p=newp,thresh2=newthresh2,agel=newagel,ageh=newageh,year=newyear,
sexratio=newsexratio)
write.dta(agesexall,file="agesexsizes_Sep15.dta")

# now use these for rest of analysis
age<-newage
regionid<-newregion
thresh1<-newthresh
countsn<-newcountsall
survey<-newsurvey
country<-newcountry
female<-0*newsex
female[which(newsex == 1)]<-1
agel<-newagel
ageh<-newageh
gdp<-newgdp
thresh2<-newthresh2
year<-newyear
p<-newp
sex<-newsex
sexratio<-newsexratio
poscounts<-newposcounts
recordid<-newrecordid
samplesize<-newsamplesize

# issue about age/sex
# for now remove all records from studies 6,16,17,22,26 with sex data
# and only keep information about age pattern
removesexids<-c(6,16,17,22,28)  # dataset Jul 16 (22 28), new  US new data it is 22 28
goodsurveys<-setdiff(1:max(surveydata$uid),removesexids)
# either one of special surveys or special survey data
matchind<-surveydata$uid%in%goodsurveys
newinds<-which(matchind == TRUE | (surveydata$sex == 0.5))
# only keep remaining data
age<-age[newinds]
regionid<-regionid[newinds]
thresh1<-thresh1[newinds]
countsn<-countsn[newinds]
survey<-survey[newinds]
sexratio<-sexratio[newinds]
female<-female[newinds]
agel<-agel[newinds]
ageh<-ageh[newinds]
thresh2<-thresh2[newinds]
year<-year[newinds]   
gdp<-gdp[newinds]
p<-p[newinds]   
sex<-sex[newinds] 
poscounts<-poscounts[newinds]
recordid<-newrecordid[newinds]
country<-country[newinds]
samplesize<-samplesize[newinds]

# We will want to predict at a set of fixed thresholds and 
# ages: for now predicting at start of age group, could
# choose middle
gbdthresh<-c(20,21,35,41,50,61,65,80) # threshold to predict at
gbdagegroups<-c(1,5,10,15,20,25,35,45,55,65,75,85,100)
# predict at midpoint of each age range
gbdagepredict<-(gbdagegroups[1:(length(gbdagegroups)-1)]+
	gbdagegroups[2:(length(gbdagegroups))])/2

Nr<-21

source("countrypopulationsperyear.R")
	
# for hearing loss only look at thresholds > 20: below this 
# is essentially normal hearing, and the age patterns are different
# look at all sexessstorerest
print("only look at data for thresholds >= 25 and age > 1")
sinds<-which( thresh1 >= 25 & age > 1 )

# change variables so only considering those which fulfill the 
# above condition
age<-age[sinds]
agel<-agel[sinds]
ageh<-ageh[sinds]
year<-year[sinds]
thresh1<-thresh1[sinds]
thresh2<-thresh2[sinds]
sex<-sex[sinds]
gdp<-gdp[sinds]
N<-length(age)
p<-p[sinds]
Ns<-length(unique(survey[sinds]))
countryorig<-country[sinds]
country<-country[sinds]
countsn<-countsn[sinds]
survey2country<-c(NA)
survey<-survey[sinds]
female<-female[sinds]
regionid<-regionid[sinds]
poscounts<-poscounts[sinds]
sexratio<-sexratio[sinds]
countrynames<-unique(country)
recordid<-recordid[sinds]
samplesize<-samplesize[sinds]

j<-1
countrynew<-c(NA)
i<-1
cunique<-unique(country) # store unique set of country ids
# number of countries
Nc<-length(unique(country))
N<-length(p) # number of samples

# stores sample hearing loss counts adjusted for sample 
# size. not exact because do rounding and because use 
# population age-sex numbers, not survey numbers
# (and some surveys may provide these numbers-- would be 
# worth looking)
allcounts = 0*matrix(1:2*length(age),length(age),2)#mat.or.vec(length(age),2)
allcounts[,1]<-poscounts
allcounts[,2]<-countsn-poscounts

Nr<-max(regionid) # largest region consider
# store original data here so can reload these
allallcounts<-allcounts
allage<-age
allthresh<-thresh1
allfemale<-female
allgdp<-gdp
allp<-p
allagel<-agel
allageh<-ageh
allwidth<-ageh-agel
allcountry<-country
allregionid<-regionid
allsurvey<-survey
allsamplesize<-samplesize
# set the number of parameters to sample when computing 
# confidence intervals over predicted prevalences
Nsamples<-100

# width of confidence intervals compute (lower/upper percentile)
ciint<-c(.025,.975)

countrynames<-unique(country);

ocounts<-allcounts
oage<-age
othresh<-thresh1
ofemale<-female
ogdp<-gdp
op<-p
oagel<-agel
oageh<-ageh
owidth<-ageh-agel
ocountry<-country
oregionid<-regionid
osurvey<-survey
owidth<-allwidth
osamplesize<-samplesize
osexratio<-sexratio
ot2<-othresh^2
oa2<-oage^2
oyear<-year

# remove narrow data when building model
width_threshold<-15;
inds<-which(allwidth< width_threshold);
ii<-inds
allcounts<-ocounts[ii,]
age<-age[ii]
thresh1<-thresh1[ii]
female<-female[ii]
gdp<-gdp[ii]
country<-country[ii]
p<-p[ii]
regionid<-regionid[ii]	
survey<-survey[ii]
sexratio<-sexratio[ii]	
year<-year[ii]
width<-allwidth[ii]
samplesize<-samplesize[ii]
t2<-thresh1[ii]^2
a2<-age[ii]^2

odata<-data.frame(ocountry,oage,othresh,osexratio,ogdp,op,oagel,oageh,
	osurvey,oregionid,owidth,osamplesize,ocounts1=ocounts[,1],ocounts2=ocounts[,2],oyear);
data<-data.frame(country,age,thresh1,sexratio,gdp,p,
	survey,regionid,samplesize,allcounts1=allcounts[,1],allcounts2=allcounts[,2],year);


save.image("data_wMadagascar.RData")

