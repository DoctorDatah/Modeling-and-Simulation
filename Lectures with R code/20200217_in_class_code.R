#-------------------------------------------#
# Acceptance rejection example
#-------------------------------------------#
xdom=seq(0,1,by=0.01)
plot(xdom,6*xdom*(1-xdom)) #shape of the target function
curve(dbeta(x,shape1=2,shape2=2),
      add=T,col='red',lwd=2) #verify that this is actually beta(2,2)


####### proposal: unif(0,1), C=1.5
#global constants
nsim=10000
result=numeric()

#run algorithm
for(i in 1:nsim){
  xprop=runif(1)
  u=runif(1)
  acc.rate=6*xprop*(1-xprop)/((1.5)*dunif(xprop))
  if(u < acc.rate){
    result=c(result,xprop)
  }   
}

#verify results
mean(result) #should be 0.5

hist(result,nclass=20,probability=T)
curve(dbeta(x,shape1=2,shape2=2),
      add=T,col='red',lwd=2)  #should be close to beta(2,2)

length(result)/nsim #acceptance rate
