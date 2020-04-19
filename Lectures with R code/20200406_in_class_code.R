#-------------------------------------------#
# Monte Carlo integration
#-------------------------------------------#

### example 1
curve(x^2,from=0,to=1)
a=runif(1000)
mean(a^2)

### example 2
curve(x^4,from=1,to=3)
a=runif(1000,min=1,max=3)
mean(a^4)*2


### example 3
a=runif(1000,min=0,max=2)
mean(a^3-a^2) * 2


classresult=c(1.355, 1.22341, 1.405169,1.328333, 1.455,1.498, 1.274876,
              1.283769, 1.32, 1.22, 1.188352, 1.29, 1.466008, 1.32337,1.3333,
              1.277916,1.351849, 1.16)
hist(classresult)
abline(v=((2^4)/4) - ((2^3)/3),col='red')



### show CLT of estimates
nsim=1000
result=rep(NA,nsim)
for(i in 1:nsim){
  a=runif(1000,min=0,max=2)
  result[i]=mean(a^3-a^2)*2
}
hist(result,nclass=20)
abline(v=(4-8/3),lty='dashed',col='red')