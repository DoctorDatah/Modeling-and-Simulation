library(MASS)

ncov_simple=read.csv(file.choose())
ncov_simple=ncov_simple$days
summary(ncov_simple)

#-------------------------------------------#
# Homework 6
#-------------------------------------------#
fitdistr(ncov_simple,'weibull')

# log likelihood
log(prod(dweibull(ncov_simple,shape=1.8192,scale=4.6112)))

# probabilities
pweibull(14,shape=1.8192,scale=4.6112) # P(T<14)
1-pweibull(5,shape=1.8192,scale=4.6112)  #P(T>5)


#-------------------------------------------#
# Homework 7
#-------------------------------------------#
############# for 90% percentile
quantile(ncov_simple,probs=c(0.9)) 

#bootstrapping
bootsize=5000
bootresult=rep(NA,bootsize)
for(i in 1:bootsize){
  bootresult[i]=quantile(sample(ncov_simple,replace=T),probs=c(0.9))
}

#more variation than the mean and the median
sd(bootresult)

#95% confidence interval
quantile(bootresult,probs=c(0.025,0.975))


############# for max
max(ncov_simple)

#bootstrapping
bootsize=5000
bootresult=rep(NA,bootsize)
for(i in 1:bootsize){
  bootresult[i]=max(sample(ncov_simple,replace=T))
}

#more variation than the mean and the median
sd(bootresult)

#95% confidence interval
quantile(bootresult,probs=c(0.025,0.975))




#-------------------------------------------#
# Homework 9
#-------------------------------------------#
# Q2 and Q3
# run 10000 times to show CLT
nsim=10000
result=rep(NA,nsim)
for(i in 1:nsim){
  a=runif(1000,min=0,max=3)
  result[i]=mean(a^2)*3
}
hist(result,nclass=20)

quantile(result,probs=c(0.025,0.975))


# Q4
a=runif(100000,min=0,max=36)
mean(30000*exp(-(a-34)^2/160))*36


a=runif(100000,min=0,max=40)
mean(30000*exp(-(a-34)^2/160))*40





