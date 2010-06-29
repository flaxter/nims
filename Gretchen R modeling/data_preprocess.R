
# read in data
library(foreign) # so can read in stata files
library("arm")
#set the path where to look for things

surveydata<-read.dta("Allstudies.dta")
predictionvalues<-read.dta("Prediction values.dta")

# assign almost all variables in data set to shorter names for 
# ease of reference
agel<-surveydata$a1
ageh<-surveydata$a2
age<-surveydata$mage
year<-surveydata$year
thresh1<-surveydata$t1
thresh2<-surveydata$t2
country<-surveydata$iso3   # Note that this formerly held the country name
poscounts<-surveydata$sum_count # Note that this was formerly countsn but changed -- ask Emma
survey<-surveydata$uid
gdp<-log(surveydata$gdp)
sex<-surveydata$sex
sexagen<-surveydata$n
# later separate into regions & also look at sex
p<-surveydata$prev
regionid<-surveydata$RID
regionidregion<-surveydata$superregionid

# We will want to predict at a set of fixed thresholds and 
# ages: for now predicting at start of age group, could
# choose middle
gbdthresh<-c(20,21,35,41,50,61,65,80) # threshold to predict at
gbdagegroups<-c(1,5,10,15,20,25,35,45,55,65,75,85,100)
# predict at midpoint of each age range
gbdagepredict<-(gbdagegroups[1:(length(gbdagegroups)-1)]+
	gbdagegroups[2:(length(gbdagegroups))])/2

# Store number of regions
Nr<-21

#  I am not sure what the following does

j<-1
countrynew<-c(NA)
i<-1
cunique<-unique(country) # store unique set of country ids -- we won't use this!
countrynames<-unique(country);  # This command is repeated
# number of countries
Nc<-length(unique(country))
N<-length(p) # number of samples

# stores sample hearing loss counts adjusted for sample 
# size. not exact because do rounding and because use 
# population age-sex numbers, not survey numbers
# (and some surveys may provide these numbers-- would be 
# worth looking)
allcounts = 0*matrix(1:2*length(age),length(age),2)  # mat.or.vec(length(age),2)
allcounts[,1]<-poscounts  # number in age-sex group with HL
allcounts[,2]<-sexagen-poscounts  # number in age-sex group with good hearing 

Nr<-max(regionid) # largest region consider
# store original data here so can reload these
allallcounts<-allcounts
allage<-age
allthresh<-thresh1
allsex<-sex
allgdp<-gdp
allp<-p
allagel<-agel
allageh<-ageh
allcountry<-country
allregionid<-regionid
allsurvey<-survey
allsamplesize<-sexagen  # total number in teh age-sex group
# set the number of parameters to sample when computing 
# confidence intervals over predicted prevalences

Nsamples<-100

# width of confidence intervals compute (lower/upper percentile)
ciint<-c(.025,.975)
ocounts<-allcounts
oage<-age
othresh<-thresh1
osexratio<-sex
ogdp<-gdp
op<-p
oagel<-agel
oageh<-ageh
owidth<-ageh-agel
ocountry<-country
oregionid<-regionid
osurvey<-survey
osamplesize<-sexagen
ot2<-othresh^2
oa2<-oage^2
oyear<-year

# remove narrow data when building model

ii<-which(surveydata$wideage == 0)
allcounts<-ocounts[ii,]
age<-age[ii]
thresh1<-thresh1[ii]
sexratio<-sex[ii]
gdp<-gdp[ii]
country<-country[ii]
p<-p[ii]
regionid<-regionid[ii]	
survey<-survey[ii]
samplesize<-sexagen[ii]
year<-year[ii]
t2<-thresh1[ii]^2
a2<-age[ii]^2

#  This is all of the data.   Note that sexratio used to be in these and I took it out, also osamplesize

odata<-data.frame(ocountry,oage,othresh,osexratio,ogdp,op,oagel,oageh,
	osurvey,oregionid,owidth,osamplesize,ocounts1=ocounts[,1],ocounts2=ocounts[,2],oyear);
	
#  This is the model-building data.  Same, it used to have sexratio
	
data<-data.frame(country,age,thresh1,sexratio,gdp,p,
	survey,regionid,samplesize,allcounts1=allcounts[,1],allcounts2=allcounts[,2],year);


save.image("data_wMadagascar.RData")
