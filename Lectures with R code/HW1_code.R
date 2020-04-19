#------------------------------------------------------------#
# Q2: sum of three dice rolls
#------------------------------------------------------------#
nsim=10000  #number of simulated runs
result=rep(NA,nsim)  #empty vector for storing the simulated results
for (i in 1:nsim){
  dices=sample(c(1:6),size=3,replace=T) #sample 3 dices
  result[i]=sum(dices) #store the sum of dices
}
plot(as.factor(result),main='sum of three dice rolls')




#------------------------------------------------------------#
# Q4: Martingale doubling system
#------------------------------------------------------------#
nsim=1000 #number of games to simulate
result.length=rep(NA,nsim) #store the length of each game
result.win=rep(NA,nsim) #store the outcome of each game

for(k in 1:nsim){  ###start of for loop
  
  #---initialization---#
  xcurrent=0
  bcurrent=1
  ncurrent=0
  nmax=100 #max number of runs of while loop
  
  #---updating---#
  while( (xcurrent<5) && (xcurrent>-100) && (ncurrent<nmax)){
    
    #simulate one game
    R = sample(c(1,0),size=1,prob=c(18/38,20/38))
    if(R==1){
      xnext=xcurrent+bcurrent
      bnext=1
    }else{
      xnext=xcurrent-bcurrent
      bnext=2*bcurrent
    }
    nnext=ncurrent+1
    
    #update current values
    xcurrent=xnext
    bcurrent=bnext
    ncurrent=nnext
    
    #print results for one iteration
    #print(paste0("Game", ncurrent, ' - result=', R,
    #            '  net winning=$', xcurrent,
    #            '  next betting=$', bcurrent))
  }
  
  if(ncurrent==nmax){print('Warning: while loop reached max iteration')}
  
  result.length[k]=ncurrent
  result.win[k]=xcurrent
} ###end of for loop

# visualize result
plot(factor(result.win==5,labels=c('-$100','$5')))
plot(as.factor(result.length[result.win==5]),
     main='Game length resulting in $5 win')





#------------------------------------------------------------#
# Q5: Large or small hospital
#------------------------------------------------------------#
nsim=200  #number of years to simulate

#number of days on which more than 60% babies were boys in each hospital
result.large=rep(NA,nsim) 
result.small=rep(NA,nsim)

for(k in 1:nsim){
  
  #---simulate one year---#
  year.large=rep(NA,365) #percentage of boys in large hospital
  year.small=rep(NA,365) #percentage of boys in small hospital
  
  for(d in 1:365){
    #simulate one day
    large=sample(c(0,1),size=45,replace=T)
    year.large[d]=mean(large)
    small=sample(c(0,1),size=15,replace=T)
    year.small[d]=mean(small)
  }
  
  # store the end-of-year results for the simulated year
  result.large[k]=sum(year.large>=0.6)
  result.small[k]=sum(year.small>=0.6)
}

# visualize results
par(mfrow=c(1,2))
hist(result.large, xlab='Num days with >60% boys', 
     col='salmon',
     main='large hostipal')
hist(result.small, xlab='Num days with >60% boys', 
     col='lightblue',
     main='small hostipal')
