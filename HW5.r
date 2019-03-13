drug<-c(rep("A", 2),rep("B", 2),rep("C", 2))
bpChange<-c(-14,-4,5,-1,-2,6)
data<-data.frame(drug=as.factor(drug), bpChange=bpChange)
data

library(knitr)
model1<-aov(bpChange~drug, data = data)
anova(model1)


library(lsmeans)
lsmDrug=lsmeans(model1,~drug)
summary(contrast(lsmDrug, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95, side="two-sided")


push=c(rep("r0",7),rep("r1",10),rep("r2",10),rep("r3",5))
time=c(38.14,38.20,38.31,38.14,38.29,38.17,38.20,38.28,38.17,38.08,38.25,38.18,38.03,37.95,38.26,38.30,38.21,38.17,38.13,38.16,38.30,38.34,38.34,38.17,38.18,38.09,38.06,38.14,38.30,38.21,38.04,38.37)
data=data.frame(push=as.factor(push), time=time)
data

library(lsmeans)
lsmPush=lsmeans(model1,~push)
summary(contrast(lsmPush, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95, side="two-sided")

library(knitr)
model1<-aov(time~push, data = data)
anova(model1)
