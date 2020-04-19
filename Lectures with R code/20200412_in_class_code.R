#-------------------------------------------#
# Lady tasting tea
#-------------------------------------------#
tea = c('T','T','T','T','M','M','M','M')

choice=sample(tea,size=4,replace=F); choice



nsim=1000
result=rep(NA,nsim)
for(i in 1:nsim){
  result[i]=  sum(sample(tea,size=4,replace=F)=='T')
}
table(result)/nsim


#-------------------------------------------#
# Coin flipping
#-------------------------------------------#

# this is how to simulate flipping one fair coin for 10 times
sample(c(0,1),size=10,replace=T)


nsim=5000
result=rep(NA,nsim)
for(i in 1:nsim){
  result[i]=sum(sample(c(0,1),size=10,replace=T)==1)
}
table(result)/nsim
sum(dbinom(c(0,1,9,10),size=10,prob=0.5))

#-------------------------------------------#
# Simulate drunkard's walk
#-------------------------------------------#
p=0.5 #prob. of moving forward
nstep=1000 #number of steps to simulate
nstep.trace=rep(NA,nstep) #empty vector to store the drunkard's walk
nstep.trace[1]=0 #starting position

for(i in 2:nstep){
  if(runif(1)<p){
    nstep.trace[i]=nstep.trace[i-1] + 1
  }else{
    nstep.trace[i]=nstep.trace[i-1] - 1
  }
}

plot(nstep.trace,type='l')



#-------------------------------------------#
# MCMC demo
# proposal: from prior
#-------------------------------------------#
nhead=85 #number of heads
ntotal=100 #total number of flips

prior.a=2
prior.b=2
curve(dbeta(x,shape1=prior.a,shape2=prior.b),xlab=NA,ylab=NA,
      main='prior belief on p',yaxt='n')

nchain=10000 #number of MCMC runs
result=rep(NA,nchain)
result[1]=0.1 #starting position of p

for(i in 2:nchain){
  prop=rbeta(1,shape1=prior.a,shape2=prior.b)
  acc.ratio=prop^nhead*(1-prop)^(ntotal-nhead)/(result[i-1]^nhead*(1-result[i-1])^(ntotal-nhead))
  if(runif(1)<acc.ratio){
    result[i]=prop
  }else{
    result[i]=result[i-1]
  }
}
plot(result,type='l')


### plot results
mydomain=seq(0,1,by=0.001)
burnin=round(nchain*0.2)

hist(result[burnin:length(result)],col='lightblue',lwd=2,
     main=paste0(ntotal,' flips with ', nhead,' Heads'),probability = T,
     xlim=c(0,1))

abline(v=nhead/ntotal,lty='solid',col='darkblue',lwd=2)

points(mydomain,
       dbeta(mydomain,shape1=prior.a,shape2=prior.b),type='l',
       col='red',lwd=2,lty='dashed')  #prior distribution






#-------------------------------------------#
# MCMC demo
# proposal: normal
#-------------------------------------------#
nhead=8 #number of heads
ntotal=10 #total number of flips

prior.a=2
prior.b=4
curve(dbeta(x,shape1=prior.a,shape2=prior.b),xlab=NA,ylab=NA,
      main='prior belief on p',yaxt='n')

nchain=100 #number of MCMC runs
result=rep(NA,nchain)
result[1]=0.1 #starting position of p

for(i in 2:nchain){
  prop=rnorm(1,mean=result[i-1],sd=0.03)
  
  lratio=prop^nhead*(1-prop)^(ntotal-nhead)/(result[i-1]^nhead*(1-result[i-1])^(ntotal-nhead))
  priorratio=dbeta(prop,shape1=prior.a,shape2=prior.b)/dbeta(result[i-1],shape1=prior.a,shape2=prior.b)
  
  acc.ratio=lratio*priorratio
  if(runif(1)<acc.ratio){
    result[i]=prop
  }else{
    result[i]=result[i-1]
  }
}
plot(result,type='l')


### plot results
mydomain=seq(0,1,by=0.001)
burnin=round(nchain*0.2)

hist(result[burnin:length(result)],col='lightblue',lwd=2,
     main=paste0(ntotal,' flips with ', nhead,' heads'),probability = T,
     xlim=c(0,1))

abline(v=nhead/ntotal,lty='solid',col='darkblue',lwd=2)

points(mydomain,
       dbeta(mydomain,shape1=prior.a,shape2=prior.b),type='l',
       col='red',lwd=2,lty='dashed')  #prior distribution



#true posterir distribution in closed form
post.a=prior.a + nhead
post.b=prior.b + ntotal - nhead
curve(dbeta(x,shape1=post.a,shape2=post.b),add=T,col='red',lwd=2)




