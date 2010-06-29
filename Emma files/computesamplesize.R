computesamplesize<- function(survey,thresh,thresh2,year,country, #samplesized,
	agepopdata,agel,ageh,gdp,regionid,age,sex,p)
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
newsexratio<-c(NA)
newrecordid<-c(NA)
poscounts<-c(NA)
newsamplesize<-c(NA) # just keep around for later
k<-1
while(i <= N)
{
	studyid<-survey[i];
	yearid<-year[i];
	countryn<-country[i];
	j<-1
	ist<-i
	totalp<-0
	pop<-c(NA)
	sexratio<-c(NA)
	# use these two only when only have total (non-sex specific) numbers for each
	fpop<-c(NA)
	mpop<-c(NA)
	starti<-i
	startk<-k
	if(i !=k)
	{
		print("start bad")
		print(i)
		print(k)
		print("--")	
		half=theworld
	}
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
			if(agel[i] < 1)
			{
				agestart<-6
			}
			else
			{
				agestart<-7
			}
		} else {	
			agestart<-floor(agel[i]/5)+7
		}
		if(ageh[i] < 5)
		{
			if(ageh[i] < 1)
			{
				ageend<-6
			}
			else
			{
				ageend<-7
			}
		} else {
			ageend<-floor(ageh[i]/5)+7
		}
		# choose last entry if exceeds end
		agestart=min(c(agestart,length(agepopdata[1,])))
		ageend=min(c(ageend,length(agepopdata[1,])))
		
		# check have age-pop data per country
		iei<-which(agepopdata$gbd_country == countryn & agepopdata$year == 2005)
		if(length(iei) < 1)
		{
			print(paste("No population data for chosen year for country",countryn))	
		} 
		# find correct row in age matrix
		if(sexid == 1)
		{
			rowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Female")
			otherinds<-c(otherinds,i)
		}
		if(sexid == 0)
		{
			rowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Male")
			otherinds<-c(otherinds,i)
		}
		if(sexid != 0 & sexid != 1)
		{
			rowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Total")
			frowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Female")
			mrowind<-which(agepopdata$gbd_country == countryn & agepopdata$year == yearid 
				& agepopdata$sex == "Male")
			totalinds<-c(totalinds,i)
		}

		# then try to find correct index
		# for now include all of range that age falls into
		# this is wrong because may double count in wrong 
		# way when something falls only partially into interval
		if(sexid != 0 & sexid != 1)
		{
			ageinds<-agestart:ageend;
			if(ageh[i] >= 90) # check if have data here
			{
				if(is.na(agepopdata[frowind,23]) & 
					is.na(agepopdata[frowind,26]))
				{
					# 25 represents the aggregate of 80+
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					fpop[j]<- sum(agepopdata[frowind,ageinds]);
				} else if(is.na(agepopdata[frowind,23])) {
					# 25 represents the aggregate of 80+ & 90+
					# 26 is 90+ 
					# to get 80-90, could subtract 90+ from 80+
					# but here don't need
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					fpop[j]<- sum(agepopdata[frowind,ageinds]);
				} else if(is.na(agepopdata[frowind,26])) {
					# use 23 and 24
					ageinds<-c(ageinds[which(ageinds < 23)],23,24);
					fpop[j]<- sum(agepopdata[frowind,ageinds]);
				} else {
					# use 23 and 24 & 26
					ageinds<-c(ageinds[which(ageinds < 23)],23,24,26);
					fpop[j]<- sum(agepopdata[frowind,ageinds]);
				}
				
				# repeat for guys
				if(is.na(agepopdata[mrowind,23]) & 
					is.na(agepopdata[mrowind,26]))
				{
					# 25 represents the aggregate of 80+
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				} else if(is.na(agepopdata[mrowind,23])) {
					# 25 represents the aggregate of 80+, 26 90+
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				} else if(is.na(agepopdata[mrowind,26])) {
					# use 23 and 24
					ageinds<-c(ageinds[which(ageinds < 23)],23,24);
					mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				} else {
					# use 23 and 24 & 26
					ageinds<-c(ageinds[which(ageinds < 23)],23,24,26);
					mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				}
				
				
			} else if(ageh[i] >= 80)
			{
				if(is.na(agepopdata[frowind,23]))
				{
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					fpop[j]<- sum(agepopdata[frowind,ageinds]);
				} else {
					fpop[j]<- sum(agepopdata[frowind,ageinds]);
				}
				
				# repeat for men
				if(is.na(agepopdata[mrowind,23]))
				{
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				} else {
					mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				}				
			} else {
				mpop[j]<- sum(agepopdata[mrowind,ageinds]);
				fpop[j]<- sum(agepopdata[frowind,ageinds]);			}
			
			#fpop[j]<- sum(agepopdata[frowind,agestart:ageend])
			#mpop[j]<- sum(agepopdata[mrowind,agestart:ageend])
			pop[j]<-fpop[j]+mpop[j]
			sexratio[j]<-fpop[j]/(fpop[j]+mpop[j])
			totalwomen3p<-totalwomen3p+fpop[j]
			totalmen3p<-totalmen3p+mpop[j]
			totalp<-totalp+pop[j]
		} else {
			ageinds<-agestart:ageend;			
			if(ageh[i] >= 90) # check if have data here
			{
				if(is.na(agepopdata[rowind,23]) & 
					is.na(agepopdata[rowind,26]))
				{
					# 25 represents the aggregate of 80+
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					pop[j]<- sum(agepopdata[rowind,ageinds]);
				} else if(is.na(agepopdata[rowind,23])) {
					# 25 represents the aggregate of 80+, 26 90+
					ageinds<-c(ageinds[which(ageinds < 23)],25,26);
					pop[j]<- sum(agepopdata[rowind,ageinds]);
				} else if(is.na(agepopdata[rowind,26])) {
					# use 23 and 24
					ageinds<-c(ageinds[which(ageinds < 23)],23,24);
					pop[j]<- sum(agepopdata[rowind,ageinds]);
				} else {
					# use 23 and 24 & 26
					ageinds<-c(ageinds[which(ageinds < 23)],23,24,26);
					pop[j]<- sum(agepopdata[rowind,ageinds]);
				}
				
			} else if(ageh[i] >= 80)
			{
				if(is.na(agepopdata[rowind,23]))
				{
					ageinds<-c(ageinds[which(ageinds < 23)],25);
					pop[j]<- sum(agepopdata[rowind,ageinds]);
				} else {
					pop[j]<- sum(agepopdata[rowind,ageinds]);
				}				
			} else {
				pop[j]<- sum(agepopdata[rowind,ageinds]);
			}
			#pop[j]<- sum(agepopdata[rowind,agestart:ageend])
			sexratio[j]<-sexid
		}
		if(sexid ==1)
		{
			totalwomenp<-totalwomenp+pop[j]
		}
		if(sexid ==0)
		{
			totalmenp<-totalmenp+pop[j]
		}
		iold<-i
		jold<-j
		#i<-i+1
		#j<-j+1
		# use to go through all thresholds: sample size same for each
		while(i <=N & survey[i] == studyid & curtage == agel[i] & sex[i] == sexid)
		{
			# keep all in original form (not converting aggregate data to male/female)
			pop[j]<-pop[jold]
			sexratio[j]<-sexratio[jold]
			i<-i+1
			j<-j+1
		}
	}
	if(totalp == 0) #sum(pop[which(pop>0)]) > 0) 
	{
		totalp<-totalmenp+totalwomenp
	} 
	if(totalmenp > 0 & totalwomenp > 0)
	{
		popsexratio<- totalwomenp/(totalwomenp+totalmenp)
		#for(ii in 1:length(sexratio))
		#{
		#sexratio[ii]<-popsexratio
		#	
		#}
	}
	if(sexid != 0 & sexid != 1)
       {
		fpop<-fpop/(totalmen3p+totalwomen3p)
		mpop<-mpop/(totalmen3p+totalwomen3p)
		fpops<-fpop*samplesize
		mpops<-mpop*samplesize
       }

	if(sum(pop[which(pop>0)]) > 0) 
	{
	# then need to average  across all in survey
	pop<-pop/totalp
	# then need to re-weigh by actual survey sample size
	# and round to make sure count data
	# but have to make sure still sums to correct total: handle later
	
	samplesize<-surveydata$sample_size[starti]
	if(length(samplesize) == 0)
	{
		pops<-sexagen[k:(k+length(pop)-1)]
	} else {
		pops<-round(pop*samplesize)
	}
	
	# if any set to 0, must be wrong because have prevalence 
	# for that group. so set to 1?
	pops[which(pops == 0)]<-1
	# now make new variable with counts
	inds<-which(pops >= 0)
	newsamplesize[k:(k+length(inds)-1)]<-samplesize
	newsexratio[k:(k+length(inds)-1)]<-sexratio[inds]
	newcounts[k:(k+length(inds)-1)]<- pops[inds]
	pinds<-(starti-1)+inds
	newrecordid[k:(k+length(inds)-1)]<-surveydata$record_id[pinds]
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
	newageh[k:(k+length(inds)-1)]<-  ageh[pinds	]
	newyear[k:(k+length(inds)-1)]<-  year[pinds]
	poscounts[k:(k+length(inds)-1)]<-round(surveydata$prev[pinds]*pops[inds])
	k<-k+length(pinds)
	} 
}

ii<-which(poscounts > newcounts)
# but need to check if have actual data for these points: if so use
# these counts instead
haveinds<-which(!is.na(surveydata$n))
# use actual survey counts here instead
newcounts[haveinds]<-surveydata$n[haveinds]

ii<-which(poscounts > newcounts)
# also if have actual positive counts of hearing loss for this population, use 
haveinds<-which(!is.na(surveydata$count))
# round them to make integer
potposcounts<-poscounts;
potposcounts[haveinds]<-round(surveydata$count[haveinds])
ii<-which(potposcounts > newcounts)

if(length(ii) > 0)
{
	print("made something impossible after change pos counts")
	# only change ones that are okay
	goodones<-which(potposcounts <= newcounts)
	poscounts[goodones]<-potposcounts[goodones]
} else {
	poscounts<-potposcounts;		
}
ii<-which(poscounts > newcounts)
if(length(ii) > 0)

{
	print("still something impossible after changed survey")
	print(ii)
}

# check which counts vs prev are further apart than 0.01
ii<-which(abs(surveydata$count[haveinds]/surveydata$n[haveinds]-surveydata$prev[haveinds]) > .001)
surveydata[haveinds[ii],]
# how far off
(surveydata$count[haveinds[ii]]/surveydata$n[haveinds[ii]]-surveydata$prev[haveinds[ii]])

return(list(newage,newsex,newregion,newsurvey,
	newthresh,newcounts,newp,newcountry,
	newthresh2,newagel,newageh,newyear,newsexratio,
	poscounts,newrecordid,newsamplesize,newgdp))
}