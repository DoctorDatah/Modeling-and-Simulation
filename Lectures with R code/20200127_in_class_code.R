#-------------------------------------------#
# basic math
#-------------------------------------------#
(log(5^0.7)+0.7^4)/(5+0.3^4)

exp(5)*13.5^4/factorial(10)

x=c(1,2,3)
factorial(10)/(factorial(10-x)*factorial(x))*0.3^x*0.7^(10-x)
#note: the above is the binomial pmf for n=10, p=0.3 
dbinom(x=c(1,2,3),size=10,prob=0.3)


#-------------------------------------------#
# discrete distributions
#-------------------------------------------#
### discrete uniform
sample(x=c(1,2,3),size=100,replace=T)

### finite sample space
mean(sample(x=c(1,2,3), prob=c(0.1,0.6,0.3), replace=T, size=1000))

### binomial
dbinom(x=7, size=10, prob=0.8)  #example 2
sum(dbinom(x=c(10:15), size=15, prob=0.5))  #example 3
rbinom(n=50,size=15,prob=0.5) #example 3, simulate 50 days

### geometric
dgeom(4,prob=0.4)  #4 is the number of FAILURES, not number of trials
dnbinom(4,size=1,prob=0.4) #same result

### negative binomial
dnbinom(7,size=3,prob=0.45) #7 is the number of FAILURES
#simulate how many people to survey to get 3 supporters
rnbinom(n=100,size=3,prob=0.45)+3

### poisson
sum(dpois(c(0:3),lambda=5))
#simulate number of spam emails in the next 20 days
rpois(n=20,lambda=5)
