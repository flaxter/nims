agecumulative<- function(survey,year,thresh1,thresh2,agel,ageh,p)
{
i<-1
N<-length(p)
cump<-c(NA)
while(i <= N)
{
	if(thresh2[i] == 99) # already cumulative
	{
		cump[i]<-p[i]
		i<-i+1
	} else {
		istart<-i
		studyid<-survey[i]
		startage<-agel[i]
		endage<-ageh[i]
		sexid<-female[i]
		cump[istart]<-0
		while(i<= N & thresh2[i] <= 99 & studyid == survey[i] & 
			startage == agel[i] & endage == ageh[i] & 
			sexid == female[i])
		{
			cump[istart]<-cump[istart]+p[i]
			i<-i+1
		}
		i<-istart+1 # so can repeat for next round
	}
}
return(cump)
}