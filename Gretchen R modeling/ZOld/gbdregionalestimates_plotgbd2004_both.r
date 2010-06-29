######################################################
# Computes regional estimates of hearing loss
# This code uses GBD regions
# Modified June 23 1009 by Gretchen (Original by Emma B.)
######################################################
# load up useful packages (including interface to winbugs)
library("arm")
setwd( "X:/GBD2005/DISABILITY ENVELOPES/Hearing loss/Gretchen R modeling/")

# read in data
surveydata<-read.dta("HL studies.dta")
samplesized<-read.table("hearinglosssamplesize.txt",header=TRUE)  #ASK MAYA
agepopdata<-read.table("ihmepop80.txt",header=TRUE)
countryregion<-read.table("countryregion.txt",header=TRUE)
countrygdpyear<-read.table("countrygdbyear.txt",header=TRUE)
gbd2004<-read.table("gbd2004collapsed.txt",header=TRUE)

chosenyear<-2000 # predicting data at 2000
# assign almost all variables in data set to shorter names for ease of reference
N<-length(surveydata$UID)
# use midpoint for now
agel<-surveydata$a1
ageh<-surveydata$a2
age<-(surveydata$a1+surveydata$a2)/2
year<-surveydata$year
thresh1<-surveydata$t1
thresh2<-surveydata$t2
country<-surveydata$CID  #This is now ISO code
survey<-surveydata$uid
gdp<-log(surveydata$gdp)
sex<-surveydata$Sex
# later separate into regions & also look at sex
p<-surveydata$Prev
regionid<-surveydata$RID
female<-sex  #female counts both as male and should be replaced with sex

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
		
# all countries in hearing loss dataset: later could feed these in automatically	
origcountrynames=c("Angola","Australia","Brazil","China","CostaRica","Congo","Denmark",
"Finland","India","Indonesia","Italy","Kenya","Madagascar","Myanmar","Nepal",
"Nigeria","Oman","Pakistan","Sierra_Leone", "South_Africa","Sri_Lanka",
"Sweden","United_Republic_of_Tanzania","Thailand","United_Kingdom",
"United_States_of_America","Viet_Nam","Zimbabwe")
countrynames<-origcountrynames


######################################################################
# Take all data reported for both sexes and convert to male or female
# using country-specific sex ratios
# also get sample sizes for each age & sex group by using
# country, year specific age and sex population data
######################################################################
source("converttomalefemale.R")
mfdata<-converttomalefemale(survey,thresh1,thresh2,year,country,countrynames,
        samplesized,agepopdata,agel,ageh,gdp,regionid,age,sex,p)

age<-mfdata[[1]]
sex<-mfdata[[2]]
regionid<-mfdata[[3]]
survey<-mfdata[[4]]
thresh1<-mfdata[[5]]
countsn<-mfdata[[6]]
p<-mfdata[[7]]
country<-mfdata[[8]]
thresh2<-mfdata[[9]]
agel<-mfdata[[10]]
ageh<-mfdata[[11]]
year<-mfdata[[12]]

female<-0*newsex
female[which(newsex == 1)]<-1

# We will want to predict at a set of fixed thresholds and 
# ages: for now predicting at start of age group, could
# choose middle
gbdthresh<-c(21,35,41,50,61,65,80,81) # threshold to predict at
gbdagegroups<-c(5,10,15,20,25,35,45,55,65,75,85,100)
Nr<-21
# get counts per age/sex for each study, using country 
# age/sex distributions to normalize each study distribution
source("agesamplesize.r")
countsall<-agesamplesize(survey,year,country,countrynames,
	samplesized,agepopdata,agel,ageh)
source("countrypopulationsperyear.R")

choosenyear<-2000

# get age population for each country used
pop<-countrypopulationsperyear(country,countrynames,
	agepopdata,choosenyear,gbdagegroups)
femalepop<-pop[[1]]
malepop<-pop[[2]]

# change so all hearing loss done as cumulative prevalence 
source("agecumulative.R")
cump<-agecumulative(survey,year,thresh1,thresh2,agel,ageh,p)
p<-cump
countsn<-countsall


# for hearing loss only look at thresholds > 20: below this 
# is essentially normal hearing, and the age patterns are different
# also only look at male or female reports
sinds<-which( thresh1 > 20 & (sex == 1 | sex== 2))
# change variables so only considering those which fulfill the 
# above condition
age<-age[sinds]
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
countrynames<-origcountrynames


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
allcounts = mat.or.vec(length(age),2)
for(i in 1:length(p)) 
{ 
  allcounts[i,1]<-round(countsn[i]*p[i]/100)
  allcounts[i,2]<-countsn[i]-allcounts[i,1]
}

Nr<-max(regionid) # largest region consider

# store original data here so can reload these
allallcounts<-allcounts
allage<-age
allthresh<-thresh1
allfemale<-female
allgdp<-gdp
allp<-p
# set the number of parameters to sample when computing 
# confidence intervals over predicted prevalences
Nsamples<-100

# width of confidence intervals compute (lower/upper percentile)
ciint<-c(.025,.975)

#######################################################
#
# Hierarchical model with covariate of GDP
#
#######################################################
# logistic regression model ^M
# probability of hearing loss ^M
# = 1/(1 + exp(-( offset + b.age[region]age + b.agegdp*age*gdp + 
#                               b.sex*female +  b.thresh*threshold + 
#                               b.gdp*gdp )  ))
# say all countries within a region share one parameter for age
# but this is drawn from a hyper-parameter that is shared between regions 
# also include GDP*age to capture local variance within a region
# 
hier_gdptage<-lmer(allallcounts ~ allage + allgdp + allfemale + 
        allthresh + allage*allthresh + 
        allage*allgdp +  (1 + allage | regionid),
        family=binomial(link="logit"))

# number of regions in world
Nr<-length(unique(countryregion$regionid))
tmp<-1:length(gbdthresh)*2*Nr
# store weighted prevalances per [region,sex,thresh]
hierageweightedprev<-array(0*tmp,dim=c(Nr,2,length(gbdthresh)))
tmp<-1:length(gbdthresh)*2*Nr*length(gbdagegroups)


# all region 1 male all thresh all age prevalences
hierageprev[1,1, ,]
# region, sex, threshold, age estimate 
hierageprev<-array(0*tmp,
        dim=c(Nr,2,length(gbdthresh),length(gbdagegroups)))
runique<-sort(unique(regionid))

# initialize arrays to hold the regional estimates
tmp<-1:length(gbdthresh)*2*Nr
# holds the age weighted prevalences, one for each
# region, sex & threshold [region,sex,threshold]
hierageweightedprev<-array(0*tmp,dim=c(Nr,2,length(gbdthresh)))

tmp<-1:length(gbdthresh)*2*Nr*2
# array for the age weighted prevalences upper and lower
# confidence intervals for each region, sex & threshold	
hierciageweighted<-array(0*tmp,dim=c(Nr,2,length(gbdthresh),2))

# These are used to store confidence intervals per 
# region, sex, threshold, age group: cipersexaget 
hiersampledprevpersex<-array(0*tmp,
	dim=c(length(gbdthresh),length(gbdagegroups),Nsamples))
tmp<-1:Nr*2*2*length(gbdagegroups)*length(gbdthresh)
hiercipersexaget<-array(0*tmp,
	dim=c(Nr,2,length(gbdthresh),length(gbdagegroups),2))


for (rr in 1:Nr)
{
    # now need to do predictions for each sex,age, threshold & age weighted
    par(mfrow=c(1,1)) # make a new plot
    flagplot<-0			
    # for each country in region, predict		
    cinr<-which(countryregion$regionid == rr)
    # check if have any survey data for this region
    surveycountries<-intersect(countrynames,as.character(
    	countryregion$country[cinr]))
    # make sure these countries are still represented in dataset
    # (could be missing because only using data from a certain
    # threshold or higher)
    stillindata<-0
    cid<-NULL
    ij<-1
    while(ij <= length(surveycountries) & stillindata == 0)
    {
       cid<-which(countrynames == surveycountries[ij])
       if(length(country == cid) > 0)
       {
          stillindata<-1
       }
       ij<-ij+1
    }
    if(stillindata == 0)
    {
		surveycountries<-NULL
    }
    for(sexid in 0:1)
    {
    	# set up to plot for this region & sex
    	flagplot<-0
    	filestr=paste("gbdhier2004all",rr,"_s",sexid,".ps",sep="")
    	postscript(file=filestr,width=10,height=8)
	    # reset counts 
		hiersampledprevpersex[,,]<-0
		for(ti in 1:length(gbdthresh)) 
		{
			# used to look at parameters for this versus Bayesian
			hier_alphas<-c(NA)
			hier_betas<-c(NA)
			t<-gbdthresh[ti]
			agepoptotal<-rep(0,length(gbdagegroups))
			predicts<-rep(0,length(gbdagegroups))
			if(length(surveycountries) > 0) 
			{
				# this region has survey data and a particular set 
				# of coordinates in hierarchical model already
				# compute mean and estimates only over country variation
				# instead of also including parameter error
				predpercountry<-		
					array(rep(0,length(cinr)*length(gbdagegroups)),
					dim=c(length(gbdagegroups),length(cinr)))
				storepop<-array(rep(0,length(cinr)*length(gbdagegroups)),
					dim=c(length(gbdagegroups),length(cinr)))				
			}
			# for all countries in region
			for(cc in 1:length(cinr))
			{

				# get gdp for this country for chosen year
				rowind<-which(countrygdpyear$gbd_country == 
					as.character(countryregion$country[cinr[cc]]))
				# starts at 1990
				g<-log(countrygdpyear[rowind,chosenyear-1990+2])
				# expectation over all countries in region is 
				# weighted by population: get population for this country 
				# could cache this
				cpop<- countrypopulationsperyear(1,
				  as.character(countryregion$country[cinr[cc]]),
				  agepopdata,chosenyear,gbdagegroups)
						
				if(sexid == 0) # if male, stored in second index
				{
			  	  popd<-cpop[[2]]
				} else { # female
			   	  popd<-cpop[[1]]
				}	
				
            	if(sum(popd) > 0 & length(g) > 0) # otherwise estimate
			    {				
				  newagepoptotal<-agepoptotal+popd # total population			
				  # check if this is a region have data for 
				  # if so should use those parameters
				  
				  # see if ever had any survey data for this country
				  curids<-which(countrynames == countryregion						$country[cinr[cc]])
				  # check that if did have survey data, still do 
				  # (countrynames isn't modified by the subset 
				  # of surveys extracted that fulfill the threshold 
				  # requirements, but the country variable is only 
				  # for this subset, so check that there exist 
				  # observations in the data subset for this country)
				  if(length(surveycountries) > 0 & length(curids) > 0 & 					length(which(country == cid)) > 0)
				  {				
				      # get index into model coeficients
					# get which row in model this region is
				      ri<-which(sort(runique) == rr)
					# sample from all parameters
					param.offset<-coef(hier_gdptage)$regionid[ri,1]
					param.age<-coef(hier_gdptage)$regionid[ri,2]
					# use standard error for this particular region param
					samp.offset<-rnorm(Nsamples,param.offset, 
							se.ranef(hier_gdptage)$regionid[ri,1])
					samp.age<-rnorm(Nsamples,param.age, 
							se.ranef(hier_gdptage)$regionid[ri,2])
                   } else {
				     # region without any surveys, sample from parameters
				     # sample over parameters to get CI
 				     param.offset<-fixef(hier_gdptage)["(Intercept)"]
				     param.age<-fixef(hier_gdptage)["allage"]
				     # get stdev of parameters
				     sigmas<-sigma.hat(hier_gdptage)$sigma$regionid
				     # now samples from these parameters  
				     # new region: these are random but can use gdp as well
				     samp.age<-rnorm(Nsamples,param.age,sigmas[2])
				     samp.offset<-rnorm(Nsamples,param.offset,sigmas[1])
				 }
	     		# standard error for other params same for all regions
				param.gdp<-coef(hier_gdptage)$regionid[1,3]
				param.sex<-coef(hier_gdptage)$regionid[1,4]
				param.t<-coef(hier_gdptage)$regionid[1,5]
				param.agesex<-coef(hier_gdptage)$regionid[1,6]
				param.agegdp<-coef(hier_gdptage)$regionid[1,7]
				# sample parameters from normal distribution, 
				# using mean and std error for that parameter
				samp.gdp<-rnorm(Nsamples,param.gdp, 
						se.fixef(hier_gdptage)[3])
				samp.sex<-rnorm(Nsamples,param.sex, 
						se.fixef(hier_gdptage)[4])
				samp.t<-rnorm(Nsamples,param.t, 
						se.fixef(hier_gdptage)[5])
				samp.agesex<-rnorm(Nsamples,param.agesex, 
						se.fixef(hier_gdptage)[6])
				samp.agegdp<-rnorm(Nsamples,param.agegdp, 
						se.fixef(hier_gdptage)[7])
				# store all offset parameters (combo of 
				# threshold, sex, gdp, etc)
				hier_alphas[((cc-1)*Nsamples+1):(cc*Nsamples)]<-
					samp.offset+samp.gdp*g + samp.sex*sexid+
					samp.t*t
				# store all parameters relating to age parameter
				hier_betas[((cc-1)*Nsamples+1):(cc*Nsamples)]<-
					samp.age+samp.agegdp*g + samp.agesex*sexid
				for(ij in 1:Nsamples)
				 {
					 	# compute predicted hearing loss prev
						pr<-100/(1+exp(-(samp.offset[ij]+
							samp.age[ij]*gbdagegroups +
							samp.gdp[ij]*g + samp.sex[ij]*sexid + 
							samp.t[ij]*t + samp.agegdp[ij]*g*gbdagegroups+
							samp.agesex[ij]*sexid*gbdagegroups)))
						# compute expectation, weighted by population
						# (expectation is over all countries in region)
						hiersampledprevpersex[ti, ,ij]<-(pr*popd+
						   agepoptotal*hiersampledprevpersex[ti, ,ij])/						   newagepoptotal	
				}
				agepoptotal<-newagepoptotal # total region population
             }	
			  
		}
		# Summed over all countries, now compute regional averages per age and 
		# regional age-weighted averages
		# compute interval per age 
		for(ij in 1:length(gbdagegroups))
		{
			# check have > 0 population for this group
			if(newagepoptotal[ij] > 0)
			{
				# compute uncertainty intervals for each age group
				# for current threshold, sex & region
				hiercipersexaget[rr,sexid+1,ti,ij,]<-
					as.numeric(quantile(hiersampledprevpersex[ti,ij,],ciint))
				# also compute median predicted prevalence for 
				# each age group at the current threshold, sex & region 
				predicts[ij]<-median(hiersampledprevpersex[ti,ij,])
			}
		}
		hierageprev[rr,sexid+1,ti,]<-predicts # store median prediction
		if(flagplot == 0) # if plotting thresholds for new sex/region
		{
			par(mfrow=c(2,4)) # 2 by 4 set of subplots
			flagplot<-1
		}
		# only plot predictions where had non-zero population information
		pinds<-which(agepoptotal > 0)
		# plot median predicted hearing loss prev
		plot(gbdagegroups[pinds],hierageprev[rr,sexid+1,ti,pinds],
			xlim=c(0,100),ylim=c(0,100),type="n")
		lines(gbdagegroups[pinds],hierageprev[rr,sexid+1,ti,pinds],
			xlim=c(0,100),col=2)
		par(ps=8) # set font size	
	    legend(0,100,"Median",col=2,lty=1,bty="n")
	    # plot uncertainty intervals
		lines(gbdagegroups[pinds],hiercipersexaget[rr,sexid+1,ti,pinds,			1],col=2,lty=2)
	    legend(0,90,paste((1-2*ciint[1])," CI"),lty=2,col=2,bty="n")
			lines(gbdagegroups[pinds],hiercipersexaget[rr,sexid+1,ti,pinds,			2],col=2,lty=2)

		# for all the countries in this region, check if we have any 
		# survey data for this sex & threshold, and if so, plot
        cids<-intersect(countrynames,
        	as.character(countryregion$country[cinr]))
        printedsurvey<-0 # used for placing legends
		for (ci in 1:length(cids))
		{
			cid<-which(countrynames == cids[ci])
 			ii<-which(country == cid & thresh1 == t & 
				female == sexid )
			if(length(ii) > 0)
			{		
				points(age[ii],p[ii],xlab="Age",ylab="Prev",ylim=c(0,100))
				legend(0,80,"Survey data",pch=1,col=1,bty="n")
				printedsurvey<-1
			}
        }
		title(paste(regionnames[rr],"\n thresh=",t,"sexid=",sexid))
		
		
		# also compute age-weighted prevalences
		# first compute for all sampled parameters
		hierageweightsamp<-c(NA)
		for(iii in 1:Nsamples)
		{
			# only compute over ages with >0 population
			hierageweightsamp[iii]<-sum(hiersampledprevpersex[ti,pinds,iii]*
				agepoptotal[pinds])
		}
		# then (assuming have some population data), compute
		# uncertainty intervals and the median age-weighted prediction
		if(sum(agepoptotal) > 0)
		{
			hierageweightsamp<-hierageweightsamp/sum(agepoptotal)
			hierciageweighted[rr,sexid+1,ti,]<- as.numeric(quantile(
				hierageweightsamp,ciint))		
 			# take median of samples
	      	hierageweightedprev[rr,sexid+1,ti]<-median(hierageweightsamp)
		}
			
		# check if have any GBD2004 numbers for this threshold
		inds<-which(gbd2004$gbd05 == 
			as.character(regionnames[rr]) & 				gbd2004$sex == sexid+1 & gbd2004$thresh == t)
		if(length(inds) > 0) # if have estimates for GBD2004, plot it
		{
			# plot all ages and prevalences at this
			# prevalences multiply by 100 to be in percent
			points(gbd2004$age[inds],
				100*gbd2004$prevalence[inds],col=1,pch=16)
			if(printedsurvey == 1)
			{
				legend(0,70,"GBD2004 prev",col=1,pch=16,bty="n")
			} else {
				legend(0,80,"GBD2004 prev",col=1,pch=16,bty="n")
			}
		}
	}		
	dev.off()
   }
}

###########################################
# Bayesian model
# Computing regional estimates
###########################################

# in case changed data above, reset to values used before hierarchy
allcounts<-allallcounts
age<-allage
thresh1<-allthresh
female<-allfemale
p<-allp
gdp<-allgdp
N<-length(country)

# in current way doing Bayesian model, need binomial 
# counts of number of instances with hearing loss
# (countsp) and total number of observations (countsn)
# Here make a new variable to store instances 
# with hearing loss
countsp<-c(NA)
# set countsp to all 
for(i in 1:N) 
{ 
  countsp[i]<-allallcounts[i,1]
}

# for now treating each country as if it has a fixed GDP
# in reality this should depend on the year
# so probably later should move this variable down to survey 
# level of hierarchy
Nc<-max(country) # number of countries
countrygdp<-rep(0,Nc)
for(i in 1:Nc)
{
	# find gdp of country i
	gg<-gdp[which(country == i)]
	if(length(gg) > 0)
	{
	 countrygdp[i]<-gg[1]
	}
}

# prepare data to sent to WinBUGS
data<-list("countsp","countsn","age","country","female","countrygdp",
"thresh1","N","Nc")
# initialize parameters
initsu<- function() {list (b.age=runif(Nc,.01,.1), 
	alpha=rnorm(Nc,0,1), 
	b.sex=rnorm(1,0,1),
	b.gdp=runif(1,0.01,1),
	b.thresh=rnorm(1,.001,.0001),
	lambda = 1, 
	taucountry = .0001)}
# all the parameters care about
parametersu<-c("alpha","b.age","b.thresh","b.gdp","b.sex",
	"lambda","taucountry")

# pass to bugs to compute posterior distributions
# log reg model 
# logit(prev) = alpha[country] + b.age[country]*age + b.sex*female 
#               + b.thresh*thresh
# alpha[country] ~ normal distribution mean=0, var=1/taucountry
# b.age[country] ~ exp distrib with param = lambda + b.gdp*logcountrygdp
# b.gdp is assumed to come from a uniform distribution
# b.sex/b.thresh are assumed to come from a normal distrib with 0
#     mean and large variance
model_bayesgdp7<-bugs(data,initsu,parametersu,"bayesage7.bug",n.chains=3,
	n.iter=700,debug=TRUE)
# add data back to R workspace (will have a vector for each parameter
# listed in the parametersu variable above) 
attach.bugs(model_bayesgdp7)

#  Now compute end to end

# initialize arrays to hold the regional estimates
tmp<-1:length(gbdthresh)*2*Nr
# holds the age weighted prevalences, one for each 
# region, sex & threshold [region,sex,threshold]
bayesageweighted<-array(0*tmp,dim=c(Nr,2,length(gbdthresh)))

tmp<-1:length(gbdthresh)*2*Nr*2
# array for the age weighted prevalences upper and lower 
# confidence intervals for each region, sex & threshold 
bayesciageweighted<-array(0*tmp,dim=c(Nr,2,length(gbdthresh),2))

tmp<-1:length(gbdthresh)*2*Nr*length(gbdagegroups)
# holds the hearing loss prevalences, one for each 
# region, sex and threshold per GBD age group
# [region,sex,threshold,age]
bayesageprev<-array(0*tmp,dim=c(Nr,2,length(gbdthresh),
	length(gbdagegroups)))

# number of samples in the returned posterior values 
# from winBUGS
nparam<-length(lambda)
 
tmp<-1:Nsamples*length(gbdagegroups)*length(gbdthresh)
# These are used to store confidence intervals per 
# region, sex, threshold, age group: cipersexaget 
bayessampledprevpersex<-array(0*tmp,
	dim=c(length(gbdthresh),length(gbdagegroups),Nsamples))
tmp<-1:Nr*2*2*length(gbdagegroups)*length(gbdthresh)
bayescipersexaget<-array(0*tmp,
	dim=c(Nr,2,length(gbdthresh),length(gbdagegroups),2))

runique<-unique(regionid)
for (rr in 1:Nr) # for each region
{
    # now need to do predictions for each sex,age, threshold & age 
    par(mfrow=c(1,1)) # make a new figure for each region
    for(sexid in 0:1) # for each sex (0 == male, 1 == female)
    {
		# reset counts 
		bayessampledprevpersex[,,]<-0
    	flagplot<-0 # make a new plot
    	# open a file to plot results into
    	filestr=paste("gbdbayes2004b",rr,"_s",sexid,".ps",sep="")
    	postscript(file=filestr,width=10,height=8)

		for(ti in 1:length(gbdthresh))
		{
			bayes_alphas<-c(NA)
			bayes_betas<-c(NA)	
			t<-gbdthresh[ti]
			# get set of all countries in region
			cinr<-which(countryregion$regionid == rr)
			# keep track of total population in this region
			agepoptotal<-rep(0,length(gbdagegroups))
			# holds hearing loss predictions from model
			predicts<-rep(0,length(gbdagegroups))
			# no sampled parameters yet
			samp.alpha<-c(NA)
			samp.b.age<-c(NA)
			samp.b.thresh<-c(NA)
			samp.b.sex<-c(NA)
			samp.b.gdp<-c(NA)
			samp.lambda<-c(NA)
		
			tmp<-1:length(cinr)*Nsamples*length(gbdagegroups)
			tmpb<-array(0*tmp,
				dim=c(Nsamples*length(cinr),length(gbdagegroups)))
			for(cc in 1:length(cinr)) # for each country in region
			{
				# weigh by population: 
				# get population for this country for chosenyear
				# for both sexes at GBD age groups
				# future optimization: cache this across all thresholds
				cpop<- countrypopulationsperyear(1,
					as.character(countryregion$country[cinr[cc]]),
					agepopdata,chosenyear,gbdagegroups)
				if(sexid == 0) # if male, stored in second index
				{
				   popd<-cpop[[2]]
				} else { # female
			 	  popd<-cpop[[1]]
				}
				# get gdp for this country for chosen year
				rowind<-which(countrygdpyear$gbd_country == 
					as.character(countryregion$country[cinr[cc]]))
				# index starts at 1990
				g<-log(countrygdpyear[rowind,chosenyear-1990+2])

				# check if have any survey data for this country
				curcountryid<-which(countrynames == 
					countryregion$country[cinr[cc]])
				# might not be in this datasubset (given that we 
				# have thrown out some data because threshold is < 20)
	            if(length(which(country == curcountryid)) == 0)
        		{
           			curcountryid<-NULL
				}
				# this is used for running average computation
				newagepoptotal<-agepoptotal+popd
				pred<-0
				if(length(curcountryid) == 0) # no survey data
				{
			  		# if no data for population or for gdp, skip for now
			  		if(sum(popd) > 0 & length(g) > 0) # otherwise estimate
			  		{
					# sample from posterior param distrib Nsample times
					# and predict hearing loss at all GBD age groups, 
					# for this threshold (t), for this sex (sexid), at 
					# each sample
					for(ij in 1:Nsamples)
					{
						# sample offset parameter alpha from 
						# hyper-parameter, by sampling from one 
						# of samples used to represent posterior 
						samp.alpha[ij]<-rnorm(1,0,taucountry[
							floor(runif(1,1,nparam+1))])
						# similarly sample from b.gdp & lambda
						samp.b.gdp[ij]<-b.gdp[floor(runif(1,1,nparam+1))]
						samp.lambda[ij]<-lambda[floor(runif(1,1,nparam+1))]
						# age parameters is distributed as an 
						# exponential with param lambda + b.gdp*logcountrygdp
						tmp<-samp.lambda + samp.b.gdp[ij]*g
						# so sample from this distribution to get age pattern
						# parameter
						samp.b.age[ij]<-rexp(1,tmp)
						# sample hearing loss threshold parameter
						samp.b.thresh[ij]<-b.thresh[floor(
							runif(1,1,nparam+1))]
						# sample sex parameter
						samp.b.sex[ij]<-b.sex[floor(runif(1,1,nparam+1))]
						# now predict hearing loss at all ages for this 
						# country, sex, threshold & gdp
						samp.predic<-100*invlogit(samp.alpha[ij]+
							samp.b.age[ij]*gbdagegroups+
							samp.b.sex[ij]*sexid + samp.b.thresh[ij]*t)
						# for comparison purposes, store all the offset 
						# alphas (aka not multiplied by age) and 
						# the age parameter betas (all parameters 
						# multiplied by age) 	
						bayes_alphas[(cc-1)*Nsamples+ij]<-samp.alpha[ij]+
							samp.b.sex[ij]*sexid+samp.b.thresh[ij]*t
						bayes_betas[(cc-1)*Nsamples+ij]<-samp.b.age[ij]
 						# take expectation over all countries in region but 
						# weigh so each country contributes 
						# to expectation according to their population
						# keep a running average
						bayessampledprevpersex[ti, ,ij]<-(samp.predic*popd + 
							agepoptotal*bayessampledprevpersex[ti, ,ij])/							newagepoptotal
					}
				    }
			} else { # have survey data for this country
			   # this means in model computed separate alpha & 
			   # b.age parameters for this country
 				   for(ij in 1:Nsamples)
				   {
					# sample from country's parameters
					# since gdp inside b.age, could recalculate b.age 
					# in case predicting for a new year that 
					# country has a different GDP

					# however, for now just sample from b.age for this
					# particular country
					samp.alpha[ij]<-alpha[floor(
						runif(1,1,nparam+1)),curcountryid]
					samp.b.age[ij]<-b.age[floor(
						runif(1,1,nparam+1)),curcountryid]
					samp.b.thresh[ij]<-b.thresh[floor(runif(1,1,nparam+1))]
					samp.b.sex[ij]<-b.sex[floor(runif(1,1,nparam+1))]
					# and do hearing loss prediction
					samp.predic<-100*invlogit(samp.alpha[ij]+
						samp.b.age[ij]*gbdagegroups+
						samp.b.sex[ij]*sexid + samp.b.thresh[ij]*t)
					# again, store alphas (offset, not multiplied by age)
					bayes_alphas[(cc-1)*Nsamples+ij]<-samp.alpha[ij]+
						samp.b.sex[ij]*sexid+samp.b.thresh[ij]*t
					# and store all parameters that are multiplied by age
					bayes_betas	[(cc-1)*Nsamples+ij]<-samp.b.age[ij]
					# store prediction, weighted by population, as part 
					# of running population-weighed expectation
										
					bayessampledprevpersex[ti, ,ij]<-(samp.predic*popd + 
						agepoptotal*bayessampledprevpersex[ti, ,ij])/						newagepoptotal
				   }
			}
			# may be missing gdp data for this country or population data
			# so only add up average if have both of these pieces of inf
			if(sum(popd) > 0 & length(g) > 0) #sum(pred) > 0)
			{
				# weigh each country's contribution to regional 
				# estimate by population of age-sex in that country
				agepoptotal<-agepoptotal+popd
				predicts<-predicts+pred*popd	
			}	
		}
		# Summed over all countries, now compute regional averages per age and 
		# regional age-weighted averages
		# compute interval per age 
		for(ij in 1:length(gbdagegroups))
		{
			# check have > 0 population for this group
			if(newagepoptotal[ij] > 0)
			{
				# compute uncertainty intervals for each age group
				# for current threshold, sex & region
				bayescipersexaget[rr,sexid+1,ti,ij,]<-
					as.numeric(quantile(bayessampledprevpersex[ti,ij,],ciint))
				# also compute median predicted prevalence for 
				# each age group at the current threshold, sex & region 
				predicts[ij]<-median(bayessampledprevpersex[ti,ij,])
			}
		}
		bayesageprev[rr,sexid+1,ti,]<-predicts # store median prediction
		if(flagplot == 0) # if plotting thresholds for new sex/region
		{
			par(mfrow=c(2,4)) # 2 by 4 set of subplots
			flagplot<-1
		}
		# only plot predictions where had non-zero population information
		pinds<-which(agepoptotal > 0)
		# plot median predicted hearing loss prev
		plot(gbdagegroups[pinds],bayesageprev[rr,sexid+1,ti,pinds],
			xlim=c(0,100),ylim=c(0,100),type="n")
		lines(gbdagegroups[pinds],bayesageprev[rr,sexid+1,ti,pinds],
			xlim=c(0,100),col=2)
		par(ps=8) # set font size	
	    legend(0,100,"Median",col=2,lty=1,bty="n")
	    # plot uncertainty intervals
		lines(gbdagegroups[pinds],bayescipersexaget[rr,sexid+1,ti,pinds,			1],col=2,lty=2)
	    legend(0,90,paste((1-2*ciint[1])," CI"),lty=2,col=2,bty="n")
			lines(gbdagegroups[pinds],bayescipersexaget[rr,sexid+1,ti,pinds,			2],col=2,lty=2)

		# for all the countries in this region, check if we have any 
		# survey data for this sex & threshold, and if so, plot
        cids<-intersect(countrynames,
        	as.character(countryregion$country[cinr]))
        printedsurvey<-0 # used for placing legends
		for (ci in 1:length(cids))
		{
			cid<-which(countrynames == cids[ci])
 			ii<-which(country == cid & thresh1 == t & 
				female == sexid )
			if(length(ii) > 0)
			{		
				points(age[ii],p[ii],xlab="Age",ylab="Prev",ylim=c(0,100))
				legend(0,80,"Survey data",pch=1,col=1,bty="n")
				printedsurvey<-1
			}
        }
		title(paste(regionnames[rr],"\n thresh=",t,"sexid=",sexid))
		
		
		# also compute age-weighted prevalences
		# first compute for all sampled parameters
		bayesageweightsamp<-c(NA)
		for(iii in 1:Nsamples)
		{
			# only compute over ages with >0 population
			bayesageweightsamp[iii]<-sum(bayessampledprevpersex[ti,pinds,iii]*
				agepoptotal[pinds])
		}
		# then (assuming have some population data), compute
		# uncertainty intervals and the median age-weighted prediction
		if(sum(agepoptotal) > 0)
		{
			bayesageweightsamp<-bayesageweightsamp/sum(agepoptotal)
			bayesciageweighted[rr,sexid+1,ti,]<- as.numeric(quantile(
				bayesageweightsamp,ciint))		
 			# take median of samples
	      	bayesageweighted[rr,sexid+1,ti]<-median(bayesageweightsamp)
		}
			
		# check if have any GBD2004 numbers for this threshold
		inds<-which(gbd2004$gbd05 == 
			as.character(regionnames[rr]) & 				gbd2004$sex == sexid+1 & gbd2004$thresh == t)
		if(length(inds) > 0) # if have estimates for GBD2004, plot it
		{
			# plot all ages and prevalences at this
			# prevalences multiply by 100 to be in percent
			points(gbd2004$age[inds],
				100*gbd2004$prevalence[inds],col=1,pch=16)
			if(printedsurvey == 1)
			{
				legend(0,70,"GBD2004 prev",col=1,pch=16,bty="n")
			} else {
				legend(0,80,"GBD2004 prev",col=1,pch=16,bty="n")
			}
		}
	}		
    dev.off() # write out file
   }
}
save.image(file="hearingloss_both.RData")
