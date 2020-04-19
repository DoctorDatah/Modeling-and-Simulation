#-------------------------------------------#
# basic loop examples
#-------------------------------------------#
a=2
for(i in 2:10){  #i is created after this step
  a=a+3
  print(paste('Step',i,'value of a:',a))
}

while(a < 40){
  a=a+3
  print(paste('Step',i,'value of a:',a))
}



#-------------------------------------------#
# Exercise
#-------------------------------------------#
#1
for(i in 1:10){
  print('hello, world')
}

#2
for(i in 1:30){
  print(paste(i,'moose', 4*i, 'legs'))
}

#3
result=rep(NA,100)
for(i in 1:100){
  if( (i%%3==0) | (grepl(3,x=i)) ){
    result[i]="p"
  }else{
    result[i]=i
  }
}
sum(result=='p')

#4
count=1
countmax=10000
k=1
while( (k*sqrt(k+1)<2000) & count<countmax){
  print(k*sqrt(k+1))
  k=k+1
  count=count+1
}


#5
count=1
countmax=10000
result=numeric()
result[1]=1
result[2]=1
k=2
while( result[k]<10000 & count<countmax){
  result[k+1]=result[k]+result[k-1]
  k=k+1
  count=count+1
}
#double check; here we simply delete the last value 
result=result[-length(result)]
mean(result)