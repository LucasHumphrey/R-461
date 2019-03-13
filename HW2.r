## 1
treatments.not.random=c(rep("none",5),rep("low",5),rep("medium",5),rep("high",5))
plot.ID=1:length(treatments.not.random)
treatments.random=sample(treatments.not.random)
data.frame(plot.ID,treatment=treatments.random)

## 2
data.frame(plot.ID,treatment=treatments.random)

##3
treatments.not.random=c(rep("r1",3),rep("r2",5),rep("r3",5))
plot.ID=1:length(treatments.not.random)
treatments.random=sample(treatments.not.random)
data.frame(plot.ID,treatment=treatments.random)

##6
##a)
X=rnorm(n=1000,mean=-2,sd=sqrt(3))
hist(X,main="Histogram of 1000 N(-2,3) iid Random Variables")

##b)
Y=rnorm(n=1000,mean=3,sd=1)
hist(Y,main="Histogram of 1000 N(3,1) iid Random Variables")

##c)
Z=X+Y
hist(Z,main="Histogram of X+Y")

##e)
mean(Z)
var(Z)
