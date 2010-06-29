plotlog <- function(fit.1,age,female,thresh1,p,t,sexid,country,
	c,countryflag,
	newplotflag,colorid,plotdataflag,printfile,ci) 
{
if(countryflag == 1) # only predict for single country
{
	sinds<-which(thresh1 == t & female == sexid & country == c)
} else {
	sinds<-which(thresh1 == t & female == sexid)
}
if(newplotflag == 1)
{

	if(plotdataflag)
	{
	plot(age,p,xlab="Age",ylab="Prevalence",type="n")
	} else {
	plot(c(0,100),c(0,100),xlab="Age",ylab="Prevalence",type="n")
	}

} 
# else just plot points 
if(plotdataflag)
{
      points(age[sinds],p[sinds],pch="+",col=colorid)
}

x<-0:100 # age range want to predict values at
curve(100*invlogit(coef(fit.1)[1]+coef(fit.1)[2]*x + coef(fit.1)[3]*t+coef(fit.1)[4]*sexid
 + coef(fit.1)[5]*x*t + coef(fit.1)[6]*sexid*x), add=TRUE,col=colorid)
# plot confidence interval
if(ci > 0)
{
	N.sim<-1000
	sim.p<-sim(fit.1,N.sim)
	predics<-mat.or.vec(length(x),N.sim)
	for(i in 1:N.sim)
	{
		rnum<-round(runif(6,1,1000))
		predics[,i]<-100*invlogit(sim.p$coef[rnum[1],1]+
			sim.p$coef[rnum[2],2]*x + 
			sim.p$coef[rnum[3],3]*t+sim.p$coef[rnum[4],4]*sexid
 			+ sim.p$coef[rnum[5],5]*x*t + sim.p$coef[rnum[6],6]*sexid*x)
		#lines(x,predics[,i], col=colorid,lty=3)
	}
	ciint<-mat.or.vec(length(x),2)
	# compute quantile for each age
	for(ij in 1:max(x)+1)
	{
		ciint[ij,]<-quantile(predics[ij,],c((1-ci)/2,(ci+(1-ci)/2)))
	}
	lines(x,ciint[,1],col=colorid,lty=3)
	lines(x,ciint[,2],col=colorid,lty=3)		
}


yrange<-max(p)-min(p);
if(plotdataflag)
{
if(sexid == 1)
{
	legendstr<-paste("Threshold",t,"Female")
} 
if(sexid == 0)
{
	legendstr<-paste("Threshold",t,"Male")
} 
if(countryflag == 1)
{
	legendstr<-paste(legendstr,"Country",c)
}
legend(min(age)-1, max(p)-1-(yrange/10)*(colorid-1),legendstr,
	col=colorid,pch="+",bty="n")
} else {
#	legend(min(age)-1, max(p)-1-(yrange/10)*(colorid-1),
	legend(min(age)-1, 50,
		paste("No GDP, T=",t),
		pch="-",col=colorid,bty="n")
}
if(newplotflag == 1)
{
	titlestr<-"Logistic Regression\nlogit(p) ~ age + thresh + female + thresh*age + female*age"
	if(ci > 0)
	{
		titlestr<-paste(titlestr," CI=",ci)
	}
	title(titlestr,cex.main=1)
}
if(printfile == 1)
{

filestr<-paste("logr_agesexthresh_t",t,"_sex",femaleid,".png",sep="")
dev.copy(png,filestr)
 dev.off() 
}

} # end function
