#-----------------------------------------------------------#
# Example 1: number of heads in two coin tosses
#-----------------------------------------------------------#
nsim=10000
result=rep(NA,nsim)

for(i in 1:nsim){
  toss1=sample(x=c('H','T'),size=1)
  toss2=sample(x=c('H','T'),size=1)
  
  #save total number of heads in each simulation
  result[i]=(toss1=='H')+(toss2=='H')
}

table(result)/nsim



#-----------------------------------------------------------#
# Example 2: pair of six
#-----------------------------------------------------------#
nsim=10000
result=rep(NA,nsim)

for(i in 1:nsim){
  roll1=sample(x=c(1:6),size=10,replace=T)
  roll2=sample(x=c(1:6),size=10,replace=T)
  
  #save win/lose in each simulation
  if(max(roll1+roll2)==12){
    result[i]='win'   #win if pair of six showed up
  }else{
    result[i]='lose'
  }
}

table(result)/nsim


#-----------------------------------------------------------#
# Example 3: heads or tails, distribution of winning
#-----------------------------------------------------------#
nsim=100000
ngame=10

# store the results for each simulation
winning=rep(NA,nsim)

for(i in 1:nsim){
  #result of one game
  one.game=sample(c(-1,1),size=ngame,replace=T)
  winning[i]=sum(one.game)
}

# visualize result
hist(winning,nclass=10)


#-----------------------------------------------------------#
# Example 3: heads or tails, game length
#-----------------------------------------------------------#
target=5  #target winning
current=0  #starting winning
num.game=0
current.trace=current #save the winning trajectory for plotting

while((current<target)&(num.game<10000)){
  num.game=num.game+1
  current=current + sample(c(-1,1),size=1,replace=T)
  current.trace=c(current.trace,current)
}

# visualize result
if(num.game>=10000){
  mytitle='did not reach target in 10000 runs'
}else{
  mytitle=paste('target reached in',num.game,'runs')
}
plot(current.trace,type='l',main=mytitle,
     xlab='number of runs',
     ylab='winnings')
abline(h=target,lty='dotted',col='red')