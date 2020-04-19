library(MASS)
#-------------------------------------------#
# import nCoV_simple.csv
#-------------------------------------------#
ncov_simple=read.csv(file.choose())
ncov_simple=ncov_simple$days
summary(ncov_simple)


#-------------------------------------------#
# fit a few models
#-------------------------------------------#
# get parameter estimates
fitdistr(ncov_simple,'normal')
fitdistr(ncov_simple,'lognormal',lower=0.001)
fitdistr(ncov_simple,'gamma',lower=0.1)

# compare log likelihood
log(prod(dnorm(ncov_simple,mean=4.1076,sd=2.3166)))
log(prod(dlnorm(ncov_simple,meanlog=1.21,sdlog=0.73)))
log(prod(dgamma(ncov_simple,shape=2.61,rate=0.636)))


#-------------------------------------------#
# model fitting result visualization
#-------------------------------------------#
hist(ncov_simple,main='Incubation periods of 50 patients',
     col='lightblue',freq = F,
     xlab='days',nclass=10)
curve(dnorm(x,mean=4.1076,sd=2.3166),add=T,col='red',lwd=2,lty='dotted')
curve(dgamma(x,shape=2.61,rate=0.636),add=T,col='purple',lwd=2,lty='solid')
curve(dlnorm(x,meanlog=1.21,sdlog=0.73),add=T,col='darkgreen',lwd=2,lty='dashed')

legend('topright',legend =c('normal','lognormal','gamma'),
       col=c('red','darkgreen','purple'),
       lty=c('dotted','dashed','solid'))









