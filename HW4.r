##2d
soap=c(rep("Reg",4),rep("Deo",4),rep("Moist",4))
weight=c(-0.30,-0.10,-0.14,0.40,2.63,2.61,2.41,3.15,1.86,2.03,2.26,1.82)
wtloss=data.frame(soap,weight)
wtloss

library(lsmeans)

aov.wtloss = aov(weight~soap)
lsmeans(aov.wtloss,"soap")

aov.wtloss = aov(weight~soap)
lsm.wtloss = lsmeans(aov.wtloss,"soap")
contrast(lsm.wtloss,list("Reg.minus.Deo.plus.Moist"=c(1,-1/2,-1/2)))

##3
push=c(rep("r0",7),rep("r1",10),rep("r2",10),rep("r3",5))
time=c(38.14,38.20,38.31,38.14,38.29,38.17,38.20,38.28,38.17,38.08,38.25,38.18,38.03,37.95,38.26,38.30,38.21,38.17,38.13,38.16,38.30,38.34,38.34,38.17,38.18,38.09,38.06,38.14,38.30,38.21,38.04,38.37)
light=data.frame(push,time)
light

boxplot(time~push)

##3c
mean.0=mean(time[push=="r0"])
mean.0
mean.1=mean(time[push=="r1"])
mean.1
mean.2=mean(time[push=="r2"])
mean.2
mean.3=mean(time[push=="r3"])
mean.3

##3d
library(lsmeans)

aov.light = aov(time~push)
lsmeans(aov.light,"push")

aov.light = aov(time~push)
lsm.light = lsmeans(aov.light,"push")
contrast(lsm.light,list("r1.minus.r0"=c(-1,1,0,0)))

##3e
library(lsmeans)

aov.light = aov(time~push)
lsmeans(aov.light,"push")

aov.light = aov(time~push)
lsm.light = lsmeans(aov.light,"push")
contrast(lsm.light,list("r1.plus.r2.plus.r3.minus.r0"=c(-1,1/3,1/3,1/3)))
