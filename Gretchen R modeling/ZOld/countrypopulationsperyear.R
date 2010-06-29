countrypopulationsperyear<- function(country,countrynames,
	agepopdata,yearid,agegroups)
{
N<-length(unique(country))

femalepop<-mat.or.vec(N,length(agegroups))
malepop<-mat.or.vec(N,length(agegroups))
uniquec<-unique(country)
for(i in 1:N)
{
	countryn<-countrynames[uniquec[i]]
	# first do females
	frowind<-which(agepopdata$gbd_country == countryn & 
		agepopdata$year == yearid 
		& agepopdata$sex == "Female")
	# men
	mrowind<-which(agepopdata$gbd_country == countryn & 
		agepopdata$year == yearid 
		& agepopdata$sex == "Male")
	for(j in 1:(length(agegroups)-1))
	{
		# assume age starts at 5
		agestart<-floor(agegroups[j]/5)+5
		ageend<-floor((agegroups[j+1]-1)/5)+5
		# make sure don't exceed end threshold
		agestart=min(c(ageend,length(agepopdata[1,])))
		ageend=min(c(agestart,length(agepopdata[1,])))

		femalepop[i,j]<- sum(agepopdata[frowind,agestart:ageend])	
		malepop[i,j]<- sum(agepopdata[mrowind,agestart:ageend])	
	}
}
returnvalues<-list(femalepop,malepop)
return(returnvalues)
}