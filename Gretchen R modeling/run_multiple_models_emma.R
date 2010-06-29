library("arm")
# set path where store data etc
setwd("/Users/emma/Documents/thesis_brainstorm/meetings_notes/who/newdata/forg")
# preprocess data
source("data_preprocess.R")
# after running this, the odata data frame contains almost all data need for 
# leave one out analysis

# initialize code
source("leaveoneout_models.R")

# set options for which covariates to include in this round
# not all of these 
  opts.agedummy=1
  opts.sex=1
  opts.thresh<-1
  opts.gdp<-1
  opts.sexgdp<-1
  opts.agegdp<-0
  opts.ageregional<-1
  opts.gdpregional<-1
  opts.sexregional<-0
  opts.weightexp=.5  # this reweights data so studies contribute by the sqrt(sample sizr)
 
loo_sexgdp<-leaveoneout_models(odata,data,countryregion,gbdthresh,
	gbdagepredict,countrynames,agepopdata,gbdagegroups,opts);
save.image("sexgdp_model1.RData")
# to run a different model, change options
opts.sexgdp<-0
opts.agegdp<-1
loo_agegdp<-leaveoneout_models(odata,data,countryregion,gbdthresh,
	gbdagepredict,countrynames,agepopdata,gbdagegroups,opts);
save.image("agegdp_sexgdp_model1.RData")
 
# RMSE error over all survey data
sqrt(mean(loo_sexgdp_nogdpregional[[1]]^2))
# mean error per country
mean(loo_sexgdp_nogdpregional[[2]])
# percent of data points in confidence intervals
sum(loo_sexgdp_nogdpregional[[3]][,1])/sum(loo_sexgdp_nogdpregional[[3]])
