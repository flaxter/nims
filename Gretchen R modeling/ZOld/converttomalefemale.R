converttomalefemale<- function(survey,thresh,thresh2,year,country,countrynames,
	samplesized,agepopdata,agel,ageh,gdp,regionid,age,sex,p)
{
i<-1
j<-1
pop<-c(NA)
countsn<-p*0
newgdp<-c(NA)
newregion<-c(NA)
newage<-c(NA)
newthresh<-c(NA)
newsex<-c(NA)
newsurvey<-c(NA)
newcounts<-c(NA)
newp<-c(NA)
newcountry<-c(NA)
newthresh2<-c(NA)
newagel<-c(NA)
newageh<-c(NA)
newyear<-c(NA)
k<-1
while(i <= N)
{
	studyid<-survey[i]
	yearid<-year[i]
	countryn<-countrynames[country[i]]
	samplesize<-samplesized$SampleSize[which(samplesized$UID == studyid)]
	j<-1
	totalp<-0
	pop<-c(NA)
	# use these two only when only have total (non-sex specific) numbers for each
	fpop<-c(NA)
	mpop<-c(NA)
	starti<-i
	totalinds<-c(NA)
	otherinds<-c(NA)
	totalmenp<-0
	totalwomenp<-0

	totalmen3p<-0
	totalwomen3p<-0

	# iterate through all the surveys 
	while(i <=N & survey[i] == studyid)
	{
		curtage<-agel[i]
		sexid<-sex[i]
		if(agel[i] < 5)
			{
				if(agel[i] <=1)
				{
					agestart<-4
				}
				else
				{
					agestart<-5
				}
			}		else
		{	
			agestart<-floor(agel[i]/5)+5
		}
		if(ageh[i] < 5)
			{
				if(ageh[i] <= 1)
				{
					ageend<-4
				}
				else
				{
					ageend<-5
				}
			} else
		{
			ageend<-floor(ageh[i]/5)+5
		}
		# choose last entry if exceeds end
		agestart=min(c(ageend,length(agepopdata[1,])))
		ageend=min(c(agestart,length(agepopdata[1,])))
		# find correct row in age matrix
		if(sexid == 1)
		{
			rowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Female")
			otherinds<-c(otherinds,i)
		}
		if(sexid == 2)
		{
			rowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Male")
			otherinds<-c(otherinds,i)
		}
		if(sexid == 3)
		{
			rowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Total")
			frowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Total")
			mrowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Total")
			totalinds<-c(totalinds,i)
		}

		# then try to find correct index
		# for now include all of range that age falls into
		# this is wrong because may double count in wrong 
		# way when something falls only partially into interval
		
		if(sexid == 3)
		{
			fpop[j]<- sum(agepopdata[frowind,agestart:ageend])
			mpop[j]<- sum(agepopdata[mrowind,agestart:ageend])
			totalwomen3p<-totalwomen3p+fpop[j]
			totalmen3p<-totalmen3p+mpop[j]
			totalp<-totalp+pop[j]
		} else {
			pop[j]<- sum(agepopdata[rowind,agestart:ageend])
		}
		if(sexid ==1)
		{
			totalwomenp<-totalwomenp+pop[j]
		}
		if(sexid ==2)
		{
			totalmenp<-totalmenp+pop[j]
		}
		iold<-i
		jold<-j
		i<-i+1
		j<-j+1
		while(i <=N & survey[i] == studyid & curtage == agel[i] & sex[i] == sexid)
		{
			if(sexid == 3)
			{
				fpop[j]<-fpop[jold]
				mpop[j]<-mpop[jold]
			} else {
				pop[j]<-pop[jold]
			}
			i<-i+1
			j<-j+1
		}
	}
	if(sum(pop[which(pop>0)]) > 0) 
	{
		totalp<-totalmenp+totalwomenp
	}
	if(totalmenp > 0 & totalwomenp > 0)
	{
		popsexratio<- totalwomenp/(totalwomenp+totalmenp)
	}
	if(sexid == 3)
       {
		fpop<-fpop/(totalmen3p+totalwomen3p)
		mpop<-mpop/(totalmen3p+totalwomen3p)
		fpops<-fpop*samplesize
		mpops<-mpop*samplesize
       }

	if(sum(pop[which(pop>0)]) > 0) 
	{
	# then need to average across all in survey
	pop<-pop/totalp
	# then need to re-weigh by actual survey sample size
	# and round to make sure count data
	# but have to make sure still sums to correct total: handle later
	pops<-round(pop*samplesize)
	# if any set to 0, must be wrong because have prevalence 
	# for that group. so set to 1?
	pops[which(pops == 0)]<-1
	# now make new variables with only men and women
	inds<-which(pops >= 0)
	newcounts[k:(k+length(inds)-1)]<- pops[inds]
	pinds<-(starti-1)+inds
	newage[k:(k+length(inds)-1)] <- age[pinds]
	newsex[k:(k+length(inds)-1)] <- sex[pinds]
	newregion[k:(k+length(inds)-1)] <-  as.character(regionid[pinds])
	newgdp[k:(k+length(inds)-1)] <- gdp[pinds]
	newsurvey[k:(k+length(inds)-1)] <- survey[pinds]
	newthresh[k:(k+length(inds)-1)] <- thresh[pinds]
	countsn[k:(k+length(inds)-1)]<- pops[inds]
	newp[k:(k+length(inds)-1)]<-  p[pinds]
	newcountry[k:(k+length(inds)-1)]<- country[pinds]
	newthresh2[k:(k+length(inds)-1)]<-  thresh2[pinds]
	newagel[k:(k+length(inds)-1)]<-  agel[pinds]
	newageh[k:(k+length(inds)-1)]<-  ageh[pinds]	
	newyear[k:(k+length(inds)-1)]<-  year[pinds]	
	k<-k+length(pinds)
	} else {
		# only both data 
		# have to reformat into two entries
		pops<-c(round(fpops),round(mpops))
		inds<-1:length(pops)
		newcounts[k:(k+length(inds)-1)]<- pops
		s3inds<-1:length(fpops)
		pinds<-(starti-1)+s3inds
		newage[k:(k+length(pinds)*2-1)]<- c(age[pinds],age[pinds])
		newsex[k:(k+length(pinds)*2-1)]<- c(rep(1,length(fpops)),
							rep(2,length(fpops)))
		newregion[k:(k+length(pinds)*2-1)]<- c( as.character(regionid[pinds]),
					 as.character(regionid[pinds]))
		newsurvey[k:(k+length(pinds)*2-1)]<- c(survey[pinds],survey[pinds])
		newthresh[k:(k+length(pinds)*2-1)]<- c(thresh[pinds],thresh[pinds])
		newp[k:(k+length(pinds)*2-1)]<- c(p[pinds],p[pinds])
		newcountry[k:(k+length(pinds)*2-1)]<-c(country[pinds],country[pinds])
		newthresh2[k:(k+length(pinds)*2-1)]<-c(thresh2[pinds],thresh2[pinds])
		newagel[k:(k+length(pinds)*2-1)]<- c(agel[pinds],agel[pinds])
		newageh[k:(k+length(pinds)*2-1)]<- c(ageh[pinds],ageh[pinds])
		newyear[k:(k+length(pinds)*2-1)]<- c(year[pinds],year[pinds])
		k<-k+2*length(pinds)
	}
}
return(list(newage,newsex,newregion,newsurvey,newthresh,newcounts,newp,newcountry,
	newthresh2,newagel,newageh,newyear))
}