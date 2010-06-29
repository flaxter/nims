agesamplesize<- function(survey,year,country,countrynames,
	samplesized,agepopdata,agel,ageh)
{
i<-1
j<-1
pop<-c(NA)
countsn<-p*0
while(i <= N)
{
	studyid<-survey[i]
	yearid<-year[i]
	countryn<-countrynames[country[i]]
	samplesize<-samplesized$SampleSize[which(samplesized$UID == studyid)]
	j<-1
	totalp<-0
	pop<-c(NA)
	oldi<-i
	totalinds<-c(NA)
	otherinds<-c(NA)
	totalmenp<-0
	totalwomenp<-0
	# iterate through all the surveys 
	while(i <=N & survey[i] == studyid)
	{
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
			totalinds<-c(totalinds,i)
		}

		# then try to find correct index
		# for now include all of range that age falls into
		# this is wrong because may double count in wrong 
		# way when something falls only partially into interval
		
		pop[j]<- sum(agepopdata[rowind,agestart:ageend])
		if(sexid == 3)
		{
			totalp<-totalp+pop[j]
		}
		if(sexid ==1)
		{
			totalwomenp<-totalwomenp+pop[j]
		}
		if(sexid ==2)
		{
			totalmenp<-totalmenp+pop[j]
		}
		i<-i+1
		j<-j+1
	}
	if(totalp == 0) # no both data for this entry
	{
		totalp<-sum(pop)
	}
	if(totalmenp > 0 & totalwomenp > 0)
	{
		popsexratio<- totalwomenp/(totalwomenp+totalmenp)
	}

	# then need to average across all in survey
	pop<-pop/totalp
	# then need to re-weigh by actual survey sample size
	# and round to make sure count data
	# but have to make sure still sums to correct total: handle later
	pops<-round(pop*samplesize)
	# if any set to 0, must be wrong because have prevalence 
	# for that group. so set to 1?
	pops[which(pops == 0)]<-1
	countsn[oldi:(oldi+(i-oldi-1))] = pops
}
return(countsn)
}