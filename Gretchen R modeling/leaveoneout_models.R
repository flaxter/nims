# opts stores the options for which model to run
leaveoneout_models <-function(odata,data,countryregion,gbdthresh,gbdagepredict,
	countrynames,agepopdata,gbdagegroups,opts)
{
ncountryunique<-length(unique(odata$ocountry));	
rmsec<-1:ncountryunique
ss<-1:ncountryunique
tmp<-1:ncountryunique*2*2*(length(gbdagegroups)-1)*length(gbdthresh)
tmp<-1:2*ncountryunique;
includespt<-array(0*tmp,dim=c(ncountryunique,2))
usedummy<-1        
Nsamples<-1000
countryunique<-unique(country)
ncountryunique<-length(countryunique)
uniquer<-unique(regionid)
predicterror<-1:length(ocountry);
predicterror<-0*predicterror;
fpredicterror<-predicterror;
relpredicterror<-predicterror;
predictmage<-predicterror;
predictt<-predicterror;
predictsex<-predicterror;
predictreg<-predicterror;
predictgdp<-predicterror;
whichcountry<-predicterror;
storep<-predicterror;
pij<-1
useall<-0
usedummy<-1
includespt<-1:(ncountryunique*2)
includespt<-array(0*includespt,dim=c(ncountryunique,2))	
ageregioninteraction<-0
agepattern<-1
agegdppattern<-0
predict4wide<-1:length(ocountry)
predict4wide<-0*predict4wide

tmp<-1:28*2*3*length(gbdagepredict) ;
standpredict<-array(0*tmp,dim=c(28,2,4,length(gbdagepredict)))
gbdt<-c(35,41,50,65) # threshold to predict at

for (rr in 1:ncountryunique) # go through all countries for which have data
{
	print(paste("Country ",countrynames));
	# remove this country's data and refit
	ii<-which(data$country != countrynames[rr])
	# predict for all data, including overly wide age ranges
	jj<-which(odata$ocountry == countrynames[rr]) # country data
	cinds<-jj
		
	allallcounts<-array(1:(2*length(ii)),dim=c(length(ii),2));
	#allallcounts<-0*allallcounts;
	allallcounts[,1]<-data$allcounts1[ii]
	allallcounts[,2]<-data$allcounts2[ii]
	
	allage<-data$age[ii]
	allthresh<-data$thresh1[ii]
	allfemale<-data$female[ii]
	allgdp<-data$gdp[ii]
	allp<-data$p[ii]
	allregionid<-data$regionid[ii]	
	allsexratio<-data$sexratio[ii]	
	allyear<-data$year[ii]
    allsamplesize<-data$samplesize[ii]
    if(opts.weightexp == 1)
    {
    	w<-1+0*(1:length(allyear));	 
    }
    if(opts.weightexp == .5)
    {
	    w<-allsamplesize^(-.5)
	}
	allt2<-allthresh^2
	alla2<-allage^2;
	agedummy1<-allage
    agedummy2<-allage 
	ainds<-which(allage > 50)
	agedummy2[ainds]<-50


	# run different models depending on which options are set
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.sexgdp ==0 & 
		 opts.agegdp == 0 & opts.ageregional==0 & opts.gdpregional==0 & 
		 opts.sexregional == 0)
	{
		# no hierarchy, use sex, thresh, gdp, age and age dummy
		 gdptage<-glm(allallcounts ~ agedummy1 + agedummy2 +  allsexratio + allthresh +                			allgdp,
		  family=binomial(link="logit"),weights=w)
	}
	
		
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.sexgdp & 
		 opts.agegdp == 0 & opts.ageregional & opts.gdpregional==0 & 
		 opts.sexregional == 0)
	{
		# use hierarchy for age, offset and age dummy
		# use sex, thresh, gdp, age and age dummy, and sex*gdp
		
	  	hier_gdptage<-lmer(allallcounts ~ agedummy1 +agedummy2  +  
	  		allsexratio + allthresh + 		allgdp    + allsexratio*allgdp + 
  		 	(1  + agedummy1 + agedummy2 | allregionid),
        	family=binomial(link="logit"),weights=w)
	} 
	
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.sexgdp & 
		 opts.agegdp == 0 & opts.ageregional & opts.gdpregional & 
		 opts.sexregional == 0)
	{
		# use hierarchy for offset, age, age dummy and gdp
		# use sex, thresh, gdp, age and age dummy, and sex*age
	  	hier_gdptage<-lmer(allallcounts ~ agedummy1 +agedummy2  +  
	  		allsexratio + allthresh + 		allgdp    + allsexratio*allgdp +
  		 	(1  + agedummy1 + agedummy2 + allgdp | allregionid),
        	family=binomial(link="logit"),weights=w)
	} 
		
if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.sexgdp & 
	 opts.agegdp == 1 & opts.ageregional & opts.gdpregional & 
	 opts.sexregional == 0)
{
		# use hierarchy for offset, age, age dummy and gdp
		# use sex, thresh, gdp, age and age dummy, sex*age and age*gdp
  	hier_gdptage<-lmer(allallcounts ~ agedummy1 +agedummy2  +  
  		allsexratio + allthresh + 		allgdp    + allsexratio*allgdp +
  		 agedummy1*allgdp +  		agedummy2*allgdp + 
  		(1  + agedummy1 + agedummy2 + allgdp | allregionid),
        family=binomial(link="logit"),weights=w)
} 
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.agegdp & 
		opts.ageregional & opts.gdpregional & opts.sexgdp == 0 & 
		opts.sexregional == 0)
	{
		# use hierarchy for offset, age, age dummy and gdp
		# use sex, thresh, gdp, age and age dummy, and age*gdp
	  	hier_gdptage<-lmer(allallcounts ~ agedummy1 +agedummy2  +  
	  		allsexratio + allthresh + 		allgdp    + agedummy1*allgdp +
	  		agedummy2*allgdp + 
  		 	(1  + agedummy1 + agedummy2 + allgdp | allregionid),
        	family=binomial(link="logit"),weights=w)        
	}
			
 if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.agegdp == 0 & 
		opts.ageregional & opts.gdpregional & opts.sexgdp == 0 & 
		opts.sexregional == 0)
	{
		# use hierarchy for offset, age, age dummy and gdp
		# use sex, thresh, gdp, age and age dummy
	  	hier_gdptage<-lmer(allallcounts ~ agedummy1 +agedummy2  +  
	  		allsexratio + allthresh + 		allgdp  + 
  		 	(1  + agedummy1 + agedummy2 + allgdp | allregionid),
        	family=binomial(link="logit"),weights=w)
        print(paste("doing no interactions"))
        
	}
			
     storeifmadeit<-1:length(jj);
     storeifmadeit<-0*storeifmadeit;
     print(paste("computed model"));
    # now need to do predictions for each sex,age, threshold & age weighted
	# for the held out country

	# check if have any other regional data for this area, now that 
	# excluded this country	
	# this is used to determine whether or not there will be region parameters 
	# already learned for the region of this country
	if(length(which(oregionid == oregionid[jj[1]] & ocountry != countrynames[rr])) > 0)
	{
		otherregions<-1
		rid<-oregionid[jj[1]]	
		#ri<-toString(ri)
	}	else	{
		print(paste("No other data for the region with this country in it ",
			countrynames[rr]))
		otherregions<-0
	}
  includespt[rr,]<-0;
    ri<-which(sort(uniquer) == rid)
    
    # grab the country-specific computed model parameters (depends on region of countr), 
    # also depends on which model using
   if(opts.ageregional==0 & opts.gdpregional==0 & 
		 opts.sexregional == 0)
	{	 
		 if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.sexgdp ==0 & 
		 opts.agegdp == 0 )
		 {
		 
		    param.offset<-coef(gdptage)[1]
                param.age1<-coef(gdptage)[2]
                param.age2<-coef(gdptage)[3]
                
                param.sex<-coef(gdptage)[4]
                param.t<-coef(gdptage)[5]
                param.gdp<-coef(gdptage)[6]

                # sample pase.coef(gdptag2rameters from normal distribution,
                # using mean and std error for that parameter
                samp.offset<-rnorm(Nsamples,param.offset,
                                se.coef(gdptage)[1])
                samp.age1<-rnorm(Nsamples,param.age1,
                                se.coef(gdptage)[2])
                samp.age2<-rnorm(Nsamples,param.age2,
                                se.coef(gdptage)[3])
                samp.sex<-rnorm(Nsamples,param.sex,
                                se.coef(gdptage)[4])
                samp.t<-rnorm(Nsamples,param.t,
                                se.coef(gdptage)[5])
                samp.gdp<-rnorm(Nsamples,param.gdp,
                                se.coef(gdptage)[6])

		 	
		 } 

   } else {
    # check if any other countries in same region
	if(otherregions)
	{	
		param.offset<-coef(hier_gdptage)$allregionid[ri,1]
		# use standard error for this particular region param
		samp.offset<-rnorm(Nsamples,param.offset, 
				se.ranef(hier_gdptage)$allregionid[ri,1])
		if(opts.agedummy & opts.ageregional & opts.gdpregional == 0 & 
			opts.sexregional == 0)
		{
			param.age1<-coef(hier_gdptage)$allregionid$agedummy1[ri]
			# use standard error for this particular region param
			samp.age1<-rnorm(Nsamples,param.age1, 
				se.ranef(hier_gdptage)$allregionid[ri,2])
			param.age2<-coef(hier_gdptage)$allregionid$agedummy2[ri]
			# use standard error for this particular region param
			samp.age2<-rnorm(Nsamples,param.age2, 
				se.ranef(hier_gdptage)$allregionid[ri,3])
		}
		if(opts.agedummy & opts.ageregional & opts.gdpregional & 
			opts.sexregional == 0)
		{
			param.age1<-coef(hier_gdptage)$allregionid$agedummy1[ri]
			# use standard error for this particular region param
			samp.age1<-rnorm(Nsamples,param.age1, 
				se.ranef(hier_gdptage)$allregionid[ri,2])
			param.age2<-coef(hier_gdptage)$allregionid$agedummy2[ri]
			# use standard error for this particular region param
			samp.age2<-rnorm(Nsamples,param.age2, 
				se.ranef(hier_gdptage)$allregionid[ri,3])
			param.gdp<-coef(hier_gdptage)$allregionid$allgdp[ri];			samp.gdp<-rnorm(Nsamples,param.gdp,
				se.ranef(hier_gdptage)$allregionid[ri,4]);
		}
	
	} else {
		# region without any surveys, sample from parameters
		# sample over parameters to get CI
 		param.offset<-fixef(hier_gdptage)["(Intercept)"]
		# get stdev of parameters
		sigmas<-sigma.hat(hier_gdptage)$sigma$allregionid
		# now samples from these parameters  
		# new region: these are random but can use gdp as well
		samp.offset<-rnorm(Nsamples,param.offset,sigmas[1])
		if(opts.agedummy & opts.ageregional & opts.gdpregional == 0 & 
			opts.sexregional == 0)
		{
			param.age1<-fixef(hier_gdptage)["agedummy1"]
			samp.age1<-rnorm(Nsamples,param.age1,sigmas[2])
 			param.age2<-fixef(hier_gdptage)["agedummy2"]
			samp.age2<-rnorm(Nsamples,param.age2,sigmas[3])
		}
		if(opts.agedummy & opts.ageregional & opts.gdpregional & 
			opts.sexregional == 0)
		{
			param.age1<-fixef(hier_gdptage)["agedummy1"]
			samp.age1<-rnorm(Nsamples,param.age1,sigmas[2])
 			param.age2<-fixef(hier_gdptage)["agedummy2"]
			samp.age2<-rnorm(Nsamples,param.age2,sigmas[3])
			param.gdp<-fixef(hier_gdptage)["allgdp"]
			samp.gdp<-rnorm(Nsamples,param.gdp,sigmas[4])
		}
	}
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.agegdp == 0 & 
		opts.ageregional  & opts.sexgdp == 1 & opts.sexregional == 0)
	{
	   	param.sex<-coef(hier_gdptage)$allregionid$allsexratio[1]
		samp.sex<-rnorm(Nsamples,param.sex, 
			se.fixef(hier_gdptage)[4])
		param.t<-coef(hier_gdptage)$allregionid$allthresh[1]
		samp.t<-rnorm(Nsamples,param.t, 
			se.fixef(hier_gdptage)[5])
		if(opts.gdpregional == 0)
		{
			param.gdp<-coef(hier_gdptage)$allregionid$allgdp[1];
			samp.gdp<-rnorm(Nsamples,param.gdp, 
				se.fixef(hier_gdptage)[6]);
		}
		param.sexgdp<-coef(hier_gdptage)$allregionid$"allsexratio:allgdp"[1];
		samp.sexgdp<-rnorm(Nsamples,param.sexgdp, 
			se.fixef(hier_gdptage)[7]);			
	}
	
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.agegdp & 
		opts.ageregional  & opts.sexgdp == 1 & opts.sexregional == 0 & opts.gdpregional == 1)
	{
		print("age and sex both with gdp")
	  	param.sex<-coef(hier_gdptage)$allregionid$allsexratio[1]
		samp.sex<-rnorm(Nsamples,param.sex, 
			se.fixef(hier_gdptage)[4])
		param.t<-coef(hier_gdptage)$allregionid$allthresh[1]
		samp.t<-rnorm(Nsamples,param.t, 
			se.fixef(hier_gdptage)[5])
		param.gdp<-coef(hier_gdptage)$allregionid$allgdp[1];
		samp.gdp<-rnorm(Nsamples,param.gdp, 
			se.fixef(hier_gdptage)[6]);
		param.sexgdp<-coef(hier_gdptage)$allregionid$"allsexratio:allgdp"[1];
		samp.sexgdp<-rnorm(Nsamples,param.sexgdp, 
			se.fixef(hier_gdptage)[7]);			
		param.agegdp1<-coef(hier_gdptage)$allregionid$"agedummy1:allgdp"[1]
		samp.agegdp1<-rnorm(Nsamples,param.agegdp1, 
			se.fixef(hier_gdptage)[8])
		param.agegdp2<-coef(hier_gdptage)$allregionid$"agedummy2:allgdp"[1]
		samp.agegdp2<-rnorm(Nsamples,param.agegdp2, 
			se.fixef(hier_gdptage)[9])		
	}	
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.agegdp & 
		opts.ageregional & opts.gdpregional & opts.sexgdp == 0 & 
		opts.sexregional == 0)
	{
		print("age*gdp included")
	   	param.sex<-coef(hier_gdptage)$allregionid$allsexratio[1]
		samp.sex<-rnorm(Nsamples,param.sex, 
			se.fixef(hier_gdptage)[4])
		param.t<-coef(hier_gdptage)$allregionid$allthresh[1]
		samp.t<-rnorm(Nsamples,param.t, 
			se.fixef(hier_gdptage)[5])
		param.agegdp1<-coef(hier_gdptage)$allregionid$"agedummy1:allgdp"[1]
		print(paste("pp=",param.agegdp1))
		samp.agegdp1<-rnorm(Nsamples,param.agegdp1, 
			se.fixef(hier_gdptage)[7])
		param.agegdp2<-coef(hier_gdptage)$allregionid$"agedummy2:allgdp"[1]
		samp.agegdp2<-rnorm(Nsamples,param.agegdp2, 
			se.fixef(hier_gdptage)[8])		
	}
	if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & opts.agegdp == 0 & 
		opts.ageregional & opts.gdpregional & opts.sexgdp == 0 & 
		opts.sexregional == 0)
	{
	   	param.sex<-coef(hier_gdptage)$allregionid$allsexratio[1]
		samp.sex<-rnorm(Nsamples,param.sex, 
			se.fixef(hier_gdptage)[4])
		param.t<-coef(hier_gdptage)$allregionid$allthresh[1]
		samp.t<-rnorm(Nsamples,param.t, 
			se.fixef(hier_gdptage)[5])
	}
  }	

	# have model parameters
	# now need to do prediction for each survey data point for held out country
	# OJO gretchen changed the source of the prediction value below -- not sure if OK
	rowind<-which(predictionvalues$iso3 == countrynames[rr])
	prediction<-1:length(cinds)		
	for(ijk in 1:length(cinds))
	{
		# as we decided, don't bother predicting if less than 10 samples for this datapoint
		if(ocounts[cinds[ijk],1]+ocounts[cinds[ijk],2] < 10)
		{
			print(paste('Too few counts ',countrynames[rr]));
		} else 
		{	
		# otherwise, predict for all, including overly wide age ranges
		sexid<-odata$osexratio[cinds[ijk]];
		t<-odata$othresh[cinds[ijk]];
		year_ijk<-odata$oyear[cinds[ijk]];
		# get gdp for this country for particular
		g<-odata$ogdp[cinds[ijk]];
		cage<-odata$oage[cinds[ijk]];
		cage1<-cage;
				
		if(cage > 50)
		{
			cage2<-50;
		} else {
			cage2<-cage;	
		}
		age1<-odata$oagel[cinds[ijk]];
		age2<-odata$oageh[cinds[ijk]];
		tmpt2<-t^2;
		tmpca2<-odata$cage^2;
		pre<-1:Nsamples;
		pre<-0*pre;
		if(age2 - age1 >= width_threshold)
		{
			predict4wide[pij]<-1;
			# predict at each age within width
			# do a weighted avg according to country population
			# (assuming uniform age distribution within 
			# each group)
			# get population for this year
			popis<-countrypopulationsperyear(countrynames[rr],agepopdata,
				year_ijk,gbdagegroups);
			femalepopf<-popis[[1]]
			malepopf<-popis[[2]]	
			totaltmppop<-0
			numa<-round(age2-age1+1)
			prediction[ijk]<-0
			for(ijkl in 1:numa)
			{
				agetmp<-age1+ijkl-1;			
				agetmp1<-agetmp;
				if(agetmp > 50)
				{
					agetmp2<-50;	
				} else {
					agetmp2<-agetmp;
				}
				# index into population data
				aindex<-floor(agetmp/5)+1 # data starts at 0
				aindex<-min(c(aindex,length(malepopf)-1))
				# take population for this country & age, 
				# weighted by sex ratio
				pval<-(femalepopf[aindex]*sexid + malepopf[aindex]*(1-sexid))
				totaltmppop<-totaltmppop+pval
				ptmp<-1:Nsamples;
				ptmp<-0*ptmp;
				if(opts.ageregional==0 & opts.gdpregional==0 & 
					 opts.sexregional == 0)
				{	 
					 if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 					    opts.sexgdp ==0 & opts.agegdp == 0 )
					 {
			 	   		# compute predicted hearing loss prev
						ptmp[]<-100/(1+exp(-(samp.offset+
							samp.age1*agetmp1 +
							samp.age2*agetmp2 + 
							samp.gdp*g + samp.sex*sexid + 
							samp.t*t )));


					 }
				} else {
		 		if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp==0 & opts.sexgdp == 0 )
				{
		 	   		# compute predicted hearing loss prev
					ptmp[]<-100/(1+exp(-(samp.offset+
						samp.age1*agetmp1 +
						samp.age2*agetmp2 + 
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t )));
				}
		 			
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp == 0 )
				{
		 	   		# compute predicted hearing loss prev
					ptmp[]<-100/(1+exp(-(samp.offset+
						samp.age1*agetmp1 +
						samp.age2*agetmp2 + 
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t +
						samp.agegdp1*g*agetmp1 + 
						samp.agegdp2*g*agetmp2 )));
				}
					
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp == 1 )
				{
			 	  		# compute predicted hearing loss prev
					ptmp[]<-100/(1+exp(-(samp.offset+
						samp.age1*agetmp1 +
						samp.age2*agetmp2 + 
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t +
						samp.sexgdp*g*sexid + 
						samp.agegdp1*g*agetmp1 + 
						samp.agegdp2*g*agetmp2 )));
				}
			   	if(opts.agedummy  & opts.sex & opts.thresh & opts.gdp & 
			   		opts.sexgdp & opts.agegdp == 0)
				{	
				  	# compute predicted hearing loss prev
					ptmp[]<-100/(1+exp(-(samp.offset+
						samp.age1*agetmp1 +
						samp.age2*agetmp2 + 
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t +
						samp.sexgdp*g*sexid)));
				}
				}
				pre[]<-pre[]+ptmp[]*pval;	
				
		
			if(opts.ageregional==0 & opts.gdpregional==0 & 
					 opts.sexregional == 0)
				{	 
					 if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 					    opts.sexgdp ==0 & opts.agegdp == 0 )
					 {
					tmpnons<-100/
			    		(1+exp(-(param.offset+param.age1*agetmp1 + 
			    		param.age2*agetmp2 + param.gdp*g + 
	    				param.sex*sexid + param.t*t )));
						}					 	
				} else {	 	
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp == 0 & opts.sexgdp == 0 )
				{
					tmpnons<-100/
			    		(1+exp(-(param.offset+param.age1*agetmp1 + 
			    		param.age2*agetmp2 + param.gdp*g + 
	    				param.sex*sexid + param.t*t )));
				}
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp == 0 )
				{
					tmpnons<-100/
			    		(1+exp(-(param.offset+param.age1*agetmp1 + 
			    		param.age2*agetmp2 + param.gdp*g + 
	    				param.sex*sexid + param.t*t + 
	    				param.agegdp2*g*agetmp2 + 
	    				param.agegdp1*g*agetmp1  ) ));
				}
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp )
				{
					tmpnons<-100/
			    		(1+exp(-(param.offset+param.age1*agetmp1 + 
			    		param.age2*agetmp2 + param.gdp*g + 
	    				param.sex*sexid + param.t*t +
	    				param.sexgdp*g*sexid +  
	    				param.agegdp2*g*agetmp2 + 
	    				param.agegdp1*g*agetmp1  ) ));
				}
			   	if(opts.agedummy  & opts.sex & opts.thresh & opts.gdp & 
				   		opts.sexgdp & opts.agegdp == 0)
				{		
					tmpnons<-100/
			    		(1+exp(-(param.offset+param.age1*agetmp1 + 
			    		param.age2*agetmp2 + param.gdp*g + 
	    				param.sex*sexid + param.t*t + 
	    				param.sexgdp*g*sexid) ));
	    		}
	    		}
		    	prediction[ijk]<-prediction[ijk]+pval*tmpnons;
			}
			# normalize 
			pre[]<-pre[]/totaltmppop
			prediction[ijk]<-prediction[ijk]/totaltmppop
		} else {
			predict4wide[pij]<-0
			# how compute prediction depends on model
			if(opts.ageregional==0 & opts.gdpregional==0 & 
					 opts.sexregional == 0)
				{	 
					 if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 					    opts.sexgdp ==0 & opts.agegdp == 0 )
					 {
					print(paste("flat model")) 	
					pre[]<-100/(1+exp(-(samp.offset+
						samp.age1*cage1 + samp.age2*cage2 +
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t)));
						
						prediction[ijk]<-100/(1+exp(-(param.offset						+param.age1*cage1  +param.age2*cage2 + 
						param.gdp*g + param.sex*sexid + param.t*t))); 					}
				} else {
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp == 0 )
				{
					pre[]<-100/(1+exp(-(samp.offset+
						samp.age1*cage1 + samp.age2*cage2 +
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t  + 
						samp.agegdp1*cage1*g +
						samp.agegdp2*cage2*g )))

				}
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp == 0 & opts.sexgdp == 0 )
				{
					pre[]<-100/(1+exp(-(samp.offset+
						samp.age1*cage1 + samp.age2*cage2 +
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t )));
				}
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp == 0 )
				{
					pre[]<-100/(1+exp(-(samp.offset+
						samp.age1*cage1 + samp.age2*cage2 +
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t  + 
						samp.agegdp1*cage1*g +
						samp.agegdp2*cage2*g )))
				}
				if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
					opts.agegdp & opts.sexgdp )
				{
					pre[]<-100/(1+exp(-(samp.offset+
						samp.age1*cage1 + samp.age2*cage2 +
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t  + 
						samp.sexgdp*sexid*g + 
						samp.agegdp1*cage1*g +
						samp.agegdp2*cage2*g )))
				}
			   	if(opts.agedummy  & opts.sex & opts.thresh & opts.gdp & 
				   		opts.sexgdp & opts.agegdp == 0)
				{		
					pre[]<-100/(1+exp(-(samp.offset+
						samp.age1*cage1 + samp.age2*cage2 +
						samp.gdp*g + samp.sex*sexid + 
						samp.t*t  + 
						samp.sexgdp*sexid*g )))
				}		
			}
			if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
				opts.agegdp & opts.sexgdp == 0 )
			{
			 	prediction[ijk]<-100/(1+exp(-(param.offset+param.age1*cage1  +
			     	param.age2*cage2 + param.gdp*g + param.sex*sexid + param.t*t + 
			     	param.agegdp1*cage1*g + param.agegdp2*cage2*g)));
			}		
			if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
				opts.agegdp == 0 & opts.sexgdp == 0 )
			{
			 	prediction[ijk]<-100/(1+exp(-(param.offset+param.age1*cage1  +
			     	param.age2*cage2 + param.gdp*g + param.sex*sexid + param.t*t )));
			}		
			if(opts.agedummy & opts.sex & opts.thresh & opts.gdp & 
				opts.agegdp & opts.sexgdp == 1 )
			{
			 	prediction[ijk]<-100/(1+exp(-(param.offset+param.age1*cage1  +
			     	param.age2*cage2 + param.gdp*g + param.sex*sexid + param.t*t + 
			     	param.sexgdp*sexid*g + 
			     	param.agegdp1*cage1*g + param.agegdp2*cage2*g)));
			}		
		   	if(opts.agedummy  & opts.sex & opts.thresh & opts.gdp & 
			   		opts.sexgdp & opts.agegdp == 0)
			{		
			 	prediction[ijk]<-100/(1+exp(-(param.offset+param.age1*cage1  +
			     	param.age2*cage2 + param.gdp*g + param.sex*sexid + param.t*t + 
			     	param.sexgdp*sexid*g)));
			} 
    	}
   		# store all the data	
    	storep[pij]<-prediction[ijk]
    	predicterror[pij]<-prediction[ijk]-100*odata$op[cinds[ijk]]
    	relpredicterror[pij]<-predicterror[pij]/(100*odata$op[cinds[ijk]])
    	whichcountry[pij]<-rr
    	predictmage[pij]<-cage
    	predictt[pij]<-t
    	predictsex[pij]<-sexid
    	predictreg[pij]<-oregionid[jj[1]]
    	predictgdp[pij]<-g
    	
    	pij<-pij+1
		# compute uncertainty intervals for country
		# for current threshold, sex & region
		cis<-as.numeric(quantile(pre,c(.025,0.975)))
		if(cis[1] <= 100*odata$op[cinds[ijk]] & cis[2] >= 100*odata$op[cinds[ijk]])
		{
			includespt[rr,1]<-includespt[rr,1]+1
			storeifmadeit[ijk]<-1
		} else {
			print(paste("point is not in for ",countrynames[rr],
				" ijk",ijk,"value=",
				100*odata$op[cinds[ijk]]," ci=",cis[1],",",cis[2]));
			includespt[rr,2]<-includespt[rr,2]+1
		}
		}
   }    
	ie<-which(whichcountry == rr)
 	ss[rr]<-length(ie)
	rmsec[rr]<-sqrt(mean(predicterror[ie]^2))

}
# return data
if(opts.ageregional==0 & opts.gdpregional==0 & 
					 opts.sexregional == 0)
{
return(list(predicterror,rmsec,includespt,whichcountry,prediction,gdptage))
} else {
return(list(predicterror,rmsec,includespt,whichcountry,prediction,hier_gdptage))
}
}
