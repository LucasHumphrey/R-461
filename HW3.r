##1
treatments.not.random=c(rep(1,10),rep(2,10),rep(3,10))
treatment=sample(treatments.not.random)
n=length(treatment)
Exp.Unit=1:n
CRD.table=data.frame(Exp.Unit,treatment,row.names=NULL)
CRD

mu=4.7
s2=4
tau.1=-3
tau.2=5
tau.3=-2
n=length(treatment)
means=rep(NA,n)
means[treatment==1]=mu+tau.1
means[treatment==2]=mu+tau.2
means[treatment==3]=mu+tau.3
Y.sim=means+rnorm(n,mean=0,sd=sqrt(s2))
SimData=data.frame(Exp.Unit,treatment,Y.sim)
SimData

boxplot(Y.sim~treatment,main="Boxplot of Simulated ANOVA Data")

##2
tau.1=0
tau.2=0
tau.3=0
means[treatment==1]=mu+tau.1
means[treatment==2]=mu+tau.2
means[treatment==3]=mu+tau.3
Y.sim=means+rnorm(n,mean=0,sd=sqrt(s2))
SimData=data.frame(Exp.Unit,treatment,Y.sim)
SimData

boxplot(Y.sim~treatment,main="Boxplot of Reduced ANOVA Data")

##3
mu=4.7
s2=10000
tau.1=-3
tau.2=5
tau.3=-2
n=length(treatment)
means=rep(NA,n)
means[treatment==1]=mu+tau.1
means[treatment==2]=mu+tau.2
means[treatment==3]=mu+tau.3
Y.sim=means+rnorm(n,mean=0,sd=sqrt(s2))
SimData=data.frame(Exp.Unit,treatment,Y.sim)
SimData

boxplot(Y.sim~treatment,main="Boxplot of Simulated ANOVA Data")

##4
