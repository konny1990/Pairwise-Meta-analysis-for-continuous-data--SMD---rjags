#installpackage
install.packages("R2jags")
install.packages("jags")
library(R2jags)
library(rjags)
install.packages("meta")
library(meta)
help(jags)

##Data###
ncont1=c(12,13,14)
ncont2=c(14,15,16)
mean1=c(187.5,193.4,161.3)
mean2=c(165.6,171.9,180.5)
sd1=c(12.3, 8.5,6.9)
sd2=c(11.9, 10.8, 11.1)

###SMD via metacont###
pooledSMD1=metacont(ncont1,mean1,sd1,ncont2,mean2,sd2,sm="SMD")
summary(pooledSMD1)


m=cbind(mean1,mean2)
sd=cbind(sd1,sd2)
n=cbind(ncont1,ncont2)
mydata = list(ns=3,m=m,sd=sd,n=n)

#calcualate pooled SD as data
pooled.sd.calc<-function(sd,n){
  na=length(sd)
  nominator=apply(n * sd * sd, 1, sum)
  denominator = apply(n, 1, sum) - na   

pooled.sd = sqrt(nominator/denominator)
pooled.sd}

mydata = list(ns=3,m=m,sd=sd,n=n, pooled.sd=pooled.sd.calc(sd,n))

              
####################
#then make the model
#######################
PMAcontinuous=function() {

  for(i in 1:ns) { 
    
    #likelihood
    for(j in 1:2) {
    m[i,j] ~ dnorm(f[i,j],prec[i,j])#likelihood in one arm
    }
    
  
     f[i,1]<- u[i]*pooled.sd[i]
     f[i,2]<- (u[i] + d[i])*pooled.sd[i]
    
    
    #parametrisation   

    d[i] ~ dnorm(mean,prec2)
    for(j in 1:2) {
    variance[i,j]=(sd[i,j]^2)/n[i,j]
    prec[i,j]=1/variance[i,j]
    }
  
  }
  
      #prior distributions
  for (i in 1:ns) {u[i] ~ dnorm(0,.01)}
  tau ~ dunif(0,10)   #dnorm(0,100)%_%T(0,)       #This is not a proper prior for Tau!!! You are imposing FE                          
  prec2<- 1/pow(tau,2)
  mean ~ dnorm(0,0.01)

}#end of model

#####################
# initial values
#######################

initialval = NULL
#initialval = list(list(tau=0.2,mean=0.3))

#######################
# run the model
#######################

PMAinJAGS<- jags(mydata,initialval,parameters.to.save = c("tau","mean"), n.chains = 2, n.iter = 10000, n.burnin = 1000, DIC=F, model.file = PMAcontinuous)
traceplot(PMAinJAGS) #corvengence
#results
print(PMAinJAGS)