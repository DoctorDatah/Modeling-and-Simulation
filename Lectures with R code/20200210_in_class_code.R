#-------------------------------------------#
# Density estimation demo
#-------------------------------------------#
reactiontime=c(232,258,233,225,290,208,224,225,350,318,
               sample(c(200:400),size=8))

#actual reaction time was created during lecture
reactiontime

hist(reactiontime,
     xlab='ms',
     main='Reaction Time',
     probability=T, 
     col='royalblue',
     nclass=10, 
     yaxt='n', 
     ylab=NA)

#smaller bw = overfitting
dest = density(reactiontime,kernel="gaussian",bw=10)
lines(dest,col="red",lwd=2)



#-------------------------------------------#
# Exercise for exponential
#-------------------------------------------#
pexp(5,rate=1/12.5)
1-pexp(15,rate=1/12.5,lower.tail = F)
pexp(10,rate=1/12.5) - pexp(5,rate=1/12.5)

pexp(10,rate=1/12.5) - pexp(5,rate=1/12.5)

## plot seperately
hist(rexp(1000,rate=1/12.5))
curve(dexp(x,rate=1/12.5),xlim=c(0,80))

## plot together
hist(rexp(1000,rate=1/12.5),probability=T)
curve(dexp(x,rate=1/12.5),xlim=c(0,80),add=T,col='red')



#-------------------------------------------#
# Simulating CLT
#-------------------------------------------#
library(randomNames)

class.size=25
#myroster=randomNames(class.size)

#simulate scores
midterm=runif(class.size, min=0, max=100)
#data.frame(myroster,midterm)  #show result
#class average
mean(midterm)


# data collected during lecture
fakeclassaverages=c(50.92,48.12,51.49,40.25,50.38,
                    49.62,46.19,45.11,47.15,59.03,
                    37.94,40.21,57.73,49.70,55.99,
                    38.5,50.25,52.66,47.08,42.6)

hist(fakeclassaverages)

